//! Much of code below is derived from MashPlant/lalr1.

use aho_corasick::AhoCorasick;
use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use std::collections::{HashMap, HashSet};
use std::fmt::Write as FmtWrite;
use std::fs::File;
use std::io::Write;
use syn;
use syn::spanned::Spanned;

/// Configurations parsed from attrs
#[derive(Default)]
struct GenConfig {
    verbose: bool,
}

/// Function argument info
enum ArgInfo {
    Self_,
    Arg {
        name: String,
        ty: String,
        arg: syn::PatType,
    },
}

/// Convert syn::FnArg into ArgInfo
fn parse_arg(arg: &syn::FnArg) -> ArgInfo {
    match arg {
        syn::FnArg::Receiver(_) => ArgInfo::Self_,
        syn::FnArg::Typed(arg) => ArgInfo::Arg {
            name: arg.pat.clone().into_token_stream().to_string(),
            ty: arg.ty.clone().into_token_stream().to_string(),
            arg: arg.clone(),
        },
    }
}

/// Create GenConfig from attrs
fn gen_config(parser_impl: &syn::ItemImpl) -> GenConfig {
    let mut verbose = false;
    for attr in &parser_impl.attrs {
        let ident = attr.path.clone().into_token_stream().to_string();
        match ident.as_str() {
            "verbose" => verbose = true,
            _ => {}
        }
    }

    GenConfig { verbose }
}

/// Represent a production rule.
struct ProdRule {
    name: String,
    ty: String,
    // name, var, type
    prod: Vec<String>,
    arg: Vec<(String, String, syn::PatType)>,
    // original syn structs
    body: syn::Block,
    sig: syn::Signature,
}

/// Calculate the FIRST set of part of rhs
fn first_set_rhs(
    rule: &ProdRule,
    start: usize,
    terminals: &HashSet<String>,
    first_set: &HashMap<String, HashSet<Option<String>>>,
    follow_set: &HashMap<String, HashSet<Option<String>>>,
) -> HashSet<Option<String>> {
    let mut res = HashSet::new();
    if rule.prod.len() == 0 {
        res.insert(None);
    } else {
        for prod in &rule.prod[start..] {
            if terminals.contains(prod) {
                // terminal
                res.insert(Some(prod.clone()));
                return res;
            } else {
                // non-terminal
                let first = &first_set[prod];
                res = res.union(first).cloned().collect();
                res.remove(&None);
                if !first.contains(&None) {
                    return res;
                }
            }
        }
        res = res.union(&follow_set[&rule.name]).cloned().collect();
    }
    res
}

fn gen_template(
    start_symbol: String,
    token: String,
    parser_impl: &syn::ItemImpl,
    config: &GenConfig,
) -> TokenStream {
    let parser_type = parser_impl.self_ty.as_ref();
    let parser_def = parser_type.into_token_stream().to_string();
    let mut rules = vec![];
    let mut start_type = String::from("()");
    let mut type_mapping: HashMap<String, String> = HashMap::new();
    for item in &parser_impl.items {
        if let syn::ImplItem::Method(method) = item {
            let attr = method.attrs.get(0).unwrap();
            let rule = attr.tokens.to_string();
            let rule = rule[1..rule.len() - 1].trim();
            let mut rule_split = rule.split_whitespace();
            let lhs = match rule_split.next() {
                Some(lhs) => lhs.to_owned(),
                None => panic!(
                    "The rule `{}` method `{}` defined doesn't have a valid lhs.",
                    rule, method.sig.ident
                ),
            };
            let lhs_ty = match &method.sig.output {
                syn::ReturnType::Type(_, ty) => ty.into_token_stream().to_string(),
                syn::ReturnType::Default => String::from("()"),
            };

            // type checking
            if let Some(ty) = type_mapping.get(&lhs) {
                if *ty != lhs_ty {
                    let span = match &method.sig.output {
                        syn::ReturnType::Default => method.sig.span(),
                        syn::ReturnType::Type(_arrow, ty) => ty.span(),
                    }
                    .unwrap();
                    span.error(format!(
                        "non-terminal {} has conflicting return types of {} and {}",
                        lhs, ty, lhs_ty
                    ))
                    .help(format!("change return type to {}", ty))
                    .emit();
                }
            } else {
                type_mapping.insert(lhs.clone(), lhs_ty.clone());
            }

            match rule_split.next() {
                Some("->") => {}
                _ => panic!(
                    "The rule `{}` method `{}` defined doesn't have a `->`.",
                    rule, method.sig.ident
                ),
            };
            let rhs = rule_split.map(|s| s.to_owned()).collect::<Vec<String>>();
            let rhs_arg = method.sig.inputs.iter().map(parse_arg).collect::<Vec<_>>();
            let skip_self = match rhs_arg.get(0) {
                Some(ArgInfo::Self_) => 1,
                _ => 0,
            };
            let rhs_arg: Vec<(String, String, syn::PatType)> = rhs_arg
                .into_iter()
                .skip(skip_self)
                .map(|arg| match arg {
                    ArgInfo::Self_ => panic!(
                        "Method `{}` takes self argument at illegal position.",
                        method.sig.ident
                    ),
                    ArgInfo::Arg { name, ty, arg } => (name, ty, arg),
                })
                .collect();
            if lhs == start_symbol {
                start_type = lhs_ty.clone();
            }
            rules.push(ProdRule {
                name: lhs,
                ty: lhs_ty,
                prod: rhs,
                arg: rhs_arg,
                body: method.block.clone(),
                sig: method.sig.clone(),
            });
        } else {
            panic!("Impl block of gll should only contain methods.");
        }
    }

    for (_method, rule) in parser_impl
        .items
        .iter()
        .filter_map(|item| {
            if let syn::ImplItem::Method(method) = item {
                Some(method)
            } else {
                None
            }
        })
        .zip(rules.iter())
    {
        for ((name, arg, pat), prod) in rule.arg.iter().zip(rule.prod.iter()) {
            if let Some(ty) = type_mapping.get(prod) {
                let expected = format!("& {}", ty);
                if *arg != expected {
                    pat.ty
                        .span()
                        .unwrap()
                        .error(format!(
                            "argument {} should have type `&{}` instead of `{}`",
                            name, ty, arg
                        ))
                        .help(format!("change argument type to `&{}`", ty))
                        .emit();
                }
            }
        }
    }

    // terminals and non-terminals
    let mut non_terminals = HashSet::new();
    for rule in &rules {
        non_terminals.insert(rule.name.clone());
    }
    let mut terminals = HashSet::new();
    for rule in &rules {
        for prod in &rule.prod {
            if !non_terminals.contains(prod) {
                terminals.insert(prod.clone());
            }
        }
    }
    if config.verbose {
        eprintln!("T {:?}", terminals);
        eprintln!("NT {:?}", non_terminals);
    }

    // FIRST set, None means Eps
    let mut first_set: HashMap<String, HashSet<Option<String>>> = HashMap::new();
    loop {
        let mut new = first_set.clone();
        for rule in &rules {
            if rule.prod.len() > 0 {
                // no eps
                for prod in &rule.prod {
                    if terminals.contains(prod) {
                        // terminal
                        new.entry(rule.name.clone())
                            .or_insert_with(|| HashSet::new())
                            .insert(Some(prod.clone()));
                        break;
                    } else {
                        // non-terminal
                        let first = new
                            .entry(prod.clone())
                            .or_insert_with(|| HashSet::new())
                            .clone();
                        let my = new
                            .entry(rule.name.clone())
                            .or_insert_with(|| HashSet::new());
                        *my = my.union(&first).cloned().collect();
                        if !first.contains(&None) {
                            // no eps
                            break;
                        }
                    }
                }
            } else {
                // eps
                new.entry(rule.name.clone())
                    .or_insert_with(|| HashSet::new())
                    .insert(None);
            }
        }
        if new == first_set {
            break;
        }
        first_set = new;
    }
    if config.verbose {
        eprintln!("FIRST SET:");
        for (nt, set) in &first_set {
            eprint!("{}:", nt);
            for s in set.iter() {
                match s {
                    Some(s) => eprint!(" {}", s),
                    None => eprint!(" Eps"),
                }
            }
            eprintln!();
        }
    }

    // FOLLOW set
    let mut follow_set: HashMap<String, HashSet<Option<String>>> = HashMap::new();
    follow_set
        .entry(start_symbol.clone())
        .or_insert(HashSet::new())
        .insert(Some(String::from("End")));
    loop {
        let mut new = follow_set.clone();
        for rule in &rules {
            for i in 0..rule.prod.len() {
                if non_terminals.contains(&rule.prod[i]) {
                    let mut stop = false;
                    for j in (i + 1)..rule.prod.len() {
                        if non_terminals.contains(&rule.prod[j]) {
                            // non-terminal
                            let follow = first_set
                                .entry(rule.prod[j].clone())
                                .or_insert_with(|| HashSet::new())
                                .clone();
                            let my = new
                                .entry(rule.prod[i].clone())
                                .or_insert_with(|| HashSet::new());
                            *my = my.union(&follow).cloned().collect();
                            my.remove(&None);
                            if !first_set[&rule.prod[j]].contains(&None) {
                                stop = true;
                                break;
                            }
                        } else {
                            // terminal
                            new.entry(rule.prod[i].clone())
                                .or_insert_with(|| HashSet::new())
                                .insert(Some(rule.prod[j].clone()));
                            stop = true;
                            break;
                        }
                    }
                    if !stop {
                        let follow = new
                            .entry(rule.name.clone())
                            .or_insert_with(|| HashSet::new())
                            .clone();
                        let my = new
                            .entry(rule.prod[i].clone())
                            .or_insert_with(|| HashSet::new());
                        *my = my.union(&follow).cloned().collect();
                    }
                }
            }
        }
        if new == follow_set {
            break;
        }
        follow_set = new;
    }
    if config.verbose {
        eprintln!("FOLLOW SET:");
        for (nt, set) in &follow_set {
            eprint!("{}:", nt);
            for s in set.iter() {
                match s {
                    Some(s) => eprint!(" {}", s),
                    None => eprint!(" Eps"),
                }
            }
            eprintln!();
        }
    }

    // labels
    let mut labels = String::new();
    let mut label_first = String::new();
    let mut label_end = String::new();
    for non_terminal in &non_terminals {
        write!(&mut labels, "\t\tL{},\n", non_terminal).unwrap();
    }
    for (rule_index, rule) in rules.iter().enumerate() {
        write!(
            &mut labels,
            "\t\t/// {} -> . {}\n",
            rule.name,
            rule.prod.join(" ")
        )
        .unwrap();
        write!(&mut labels, "\t\tL{}_{},\n", rule.name, rule_index,).unwrap();
        if rule.prod.len() > 0 {
            // not eps
            for prod_index in 1..rule.prod.len() + 1 {
                write!(
                    &mut labels,
                    "\t\t/// {} -> {} . {}\n",
                    rule.name,
                    rule.prod[0..prod_index].join(" "),
                    rule.prod[prod_index..].join(" ")
                )
                .unwrap();
                write!(
                    &mut labels,
                    "\t\tL{}_{}_{},\n",
                    rule.name, rule_index, prod_index,
                )
                .unwrap();
            }
            write!(
                &mut label_end,
                "\t\t\t\tL{}_{}_{} => Some(NT_{}),\n",
                rule.name,
                rule_index,
                rule.prod.len(),
                rule.name,
            )
            .unwrap();
        } else {
            // eps
            write!(
                &mut label_end,
                "\t\t\t\tL{}_{} => Some(NT_{}),\n",
                rule.name, rule_index, rule.name,
            )
            .unwrap();
        }

        // X ::= a . b
        // a is terminal or a non-nullable nonterminal and if b != eps
        if rule.prod.len() >= 2
            && (terminals.contains(&rule.prod[0]) || !first_set[&rule.prod[0]].contains(&None))
        {
            write!(&mut label_first, "L{}_{}_{},", rule.name, rule_index, 1,).unwrap();
        }
    }

    // symbols
    let mut symbol_terminals = String::new();
    for terminal in &terminals {
        write!(&mut symbol_terminals, "\t\tT_{},\n", terminal).unwrap();
    }
    let mut symbol_non_terminals = String::new();
    for non_terminal in &non_terminals {
        write!(&mut symbol_non_terminals, "\t\tNT_{},\n", non_terminal).unwrap();
    }

    // states
    let mut states = String::new();
    let indent = "\t\t\t\t";
    // LS
    for nt in &non_terminals {
        let current_label = format!("L{}", nt);
        write!(&mut states, "{}Label::{} => {{\n", indent, current_label).unwrap();
        for (rule_index, rule) in rules.iter().enumerate() {
            if rule.name == *nt {
                let first = first_set_rhs(rule, 0, &terminals, &first_set, &follow_set);
                if first.contains(&None) {
                    // eps
                    write!(&mut states, "{}\tif true {{\n", indent).unwrap();
                } else {
                    // no eps
                    write!(&mut states, "{}\tif [", indent).unwrap();
                    for t in first {
                        write!(&mut states, "{}::{}, ", token, t.unwrap()).unwrap();
                    }
                    write!(
                        &mut states,
                        "].contains(&input[state.current_position].kind) {{\n"
                    )
                    .unwrap();
                }
                write!(&mut states, "{}\t\tstate.add(\n", indent).unwrap();
                write!(
                    &mut states,
                    "{}\t\t\tLabel::L{}_{},\n",
                    indent, nt, rule_index
                )
                .unwrap();
                write!(&mut states, "{}\t\t\tstate.current_node_index,\n", indent).unwrap();
                write!(&mut states, "{}\t\t\tstate.current_position,\n", indent).unwrap();
                write!(&mut states, "{}\t\t\t0, // dummy\n", indent).unwrap();
                write!(&mut states, "{}\t\t);\n", indent).unwrap();
                write!(&mut states, "{}\t}}\n", indent).unwrap();
            }
        }
        write!(&mut states, "{}\tcurrent_label = Label::L0;\n", indent).unwrap();
        write!(&mut states, "{}}}\n", indent).unwrap();
    }
    // LS_0
    for (rule_index, rule) in rules.iter().enumerate() {
        let current_label = format!("L{}_{}", rule.name, rule_index);
        write!(&mut states, "{}Label::{} => {{\n", indent, current_label).unwrap();
        if rule.prod.len() == 0 {
            // eps
            write!(
                &mut states,
                "{}\tlet right = state.get_node_t(Symbol::Eps, state.current_position);\n",
                indent
            )
            .unwrap();
            write!(&mut states, "{}\tstate.current_sppf_node = state.get_node_p(Label::{}, state.current_sppf_node, right);\n", indent, current_label).unwrap();
            write!(&mut states, "{}\tcurrent_label = Label::Ret;\n", indent).unwrap();
        } else {
            if terminals.contains(&rule.prod[0]) {
                write!(
                    &mut states,
                    "{}\tlet right = state.get_node_t(Symbol::T_{}, state.current_position);\n",
                    indent, rule.prod[0]
                )
                .unwrap();
                write!(&mut states, "{}\tstate.current_position += 1;\n", indent).unwrap();
                write!(&mut states, "{}\tstate.current_sppf_node = state.get_node_p(Label::{}_1, state.current_sppf_node, right);\n", indent, current_label).unwrap();
                write!(
                    &mut states,
                    "{}\tcurrent_label = Label::{}_1;\n",
                    indent, current_label
                )
                .unwrap();
            } else {
                write!(
                    &mut states,
                    "{}\tstate.current_node_index = state.create(\n",
                    indent
                )
                .unwrap();
                write!(&mut states, "{}\t\tLabel::{}_1,\n", indent, current_label).unwrap();
                write!(&mut states, "{}\t\tstate.current_node_index,\n", indent).unwrap();
                write!(&mut states, "{}\t\tstate.current_position,\n", indent).unwrap();
                write!(&mut states, "{}\t\tstate.current_sppf_node,\n", indent).unwrap();
                write!(&mut states, "{}\t);\n", indent).unwrap();
                write!(
                    &mut states,
                    "{}\tcurrent_label = Label::L{};\n",
                    indent, rule.prod[0]
                )
                .unwrap();
            }
        }
        write!(&mut states, "{}}}\n", indent).unwrap();
    }
    // LS_0_0
    for (rule_index, rule) in rules.iter().enumerate() {
        for prod_index in 0..rule.prod.len() {
            let current_label = format!("L{}_{}_{}", rule.name, rule_index, prod_index + 1);
            let next_label = format!("L{}_{}_{}", rule.name, rule_index, prod_index + 2);
            write!(&mut states, "{}Label::{} => {{\n", indent, current_label).unwrap();
            if prod_index == rule.prod.len() - 1 {
                write!(&mut states, "{}\tcurrent_label = Label::Ret;\n", indent).unwrap();
            } else {
                let prod = &rule.prod[prod_index + 1];
                if terminals.contains(prod) {
                    write!(
                        &mut states,
                        "{}\tif input[state.current_position].kind == {}::{} {{\n",
                        indent, token, prod
                    )
                    .unwrap();
                    write!(&mut states, "{}\t\tlet right = state.get_node_t(Symbol::T_{}, state.current_position);\n", indent, prod).unwrap();
                    write!(&mut states, "{}\t\tstate.current_position += 1;\n", indent).unwrap();
                    write!(&mut states, "{}\t\tstate.current_sppf_node = state.get_node_p(Label::{}, state.current_sppf_node, right);\n", indent, next_label).unwrap();
                    write!(
                        &mut states,
                        "{}\t\tcurrent_label = Label::{};\n",
                        indent, next_label
                    )
                    .unwrap();
                    write!(&mut states, "{}\t}} else {{\n", indent).unwrap();
                    write!(&mut states, "{}\t\tcurrent_label = Label::L0;\n", indent).unwrap();
                    write!(&mut states, "{}\t}}\n", indent).unwrap();
                } else {
                    let first =
                        first_set_rhs(rule, prod_index + 1, &terminals, &first_set, &follow_set);
                    if first.contains(&None) {
                        write!(&mut states, "{}\tif true {{\n", indent).unwrap();
                    } else {
                        write!(&mut states, "{}\tif [", indent).unwrap();
                        for t in first {
                            write!(&mut states, "{}::{}, ", token, t.unwrap()).unwrap();
                        }
                        write!(
                            &mut states,
                            "].contains(&input[state.current_position].kind) {{\n"
                        )
                        .unwrap();
                    }
                    write!(
                        &mut states,
                        "{}\t\tstate.current_node_index = state.create(\n",
                        indent
                    )
                    .unwrap();
                    write!(&mut states, "{}\t\t\tLabel::{},\n", indent, next_label).unwrap();
                    write!(&mut states, "{}\t\t\tstate.current_node_index,\n", indent).unwrap();
                    write!(&mut states, "{}\t\t\tstate.current_position,\n", indent).unwrap();
                    write!(&mut states, "{}\t\t\tstate.current_sppf_node,\n", indent).unwrap();
                    write!(&mut states, "{}\t\t);\n", indent).unwrap();
                    write!(
                        &mut states,
                        "{}\t\tcurrent_label = Label::L{};\n",
                        indent, prod
                    )
                    .unwrap();
                    write!(&mut states, "{}\t}} else {{\n", indent).unwrap();
                    write!(&mut states, "{}\t\tcurrent_label = Label::L0;\n", indent).unwrap();
                    write!(&mut states, "{}\t}}\n", indent).unwrap();
                }
            }
            write!(&mut states, "{}}}\n", indent).unwrap();
        }
    }

    let mut derive = String::new();
    let indent = "\t\t";
    // deriveS
    for nt in &non_terminals {
        write!(
            &mut derive,
            "{}fn derive{}(&mut self, node: SPPFNodeIndex, mut derivation: usize) -> usize {{\n",
            indent, nt
        )
        .unwrap();
        write!(
            &mut derive,
            "{}\tif let Some(vec) = self.storage.map_{}.get(&node) {{\n",
            indent, nt
        )
        .unwrap();
        write!(
            &mut derive,
            "{}\t\tif let Some(res) = vec.get(derivation) {{\n",
            indent
        )
        .unwrap();
        write!(&mut derive, "{}\t\t\t return *res;\n", indent).unwrap();
        write!(&mut derive, "{}\t\t}}\n", indent).unwrap();
        write!(&mut derive, "{}\t}}\n", indent).unwrap();
        write!(
            &mut derive,
            "{}\tlet symbol_node = &self.state.sppf_nodes[node];\n",
            indent
        )
        .unwrap();
        write!(
            &mut derive,
            "{}\tlet children = symbol_node.children().unwrap();\n",
            indent
        )
        .unwrap();
        write!(&mut derive, "{}\tfor child in children {{\n", indent).unwrap();
        write!(
            &mut derive,
            "{}\t\tlet current_child = &self.state.sppf_nodes[*child];\n",
            indent
        )
        .unwrap();
        write!(
            &mut derive,
            "{}\t\tif derivation < self.possible_derivations[child] {{\n",
            indent
        )
        .unwrap();
        write!(
            &mut derive,
            "{}\t\t\tif let gll_pg_core::SPPFNode::Packed(l, k, c) = current_child {{\n",
            indent
        )
        .unwrap();
        write!(
            &mut derive,
            "{}\t\t\t\tlet leaves = self.state.collect_symbols(*child);\n",
            indent
        )
        .unwrap();
        let indent = "\t\t\t\t\t\t";
        write!(&mut derive, "{}match l {{\n", indent).unwrap();
        for (rule_index, rule) in rules.iter().enumerate() {
            if rule.name == *nt {
                if rule.prod.len() > 0 {
                    // not eps
                    write!(
                        &mut derive,
                        "{}\tLabel::L{}_{}_{} => {{\n",
                        indent,
                        nt,
                        rule_index,
                        rule.prod.len()
                    )
                    .unwrap();
                } else {
                    // eps
                    write!(
                        &mut derive,
                        "{}\tLabel::L{}_{} => {{\n",
                        indent, nt, rule_index
                    )
                    .unwrap();
                };
                for i in 0..rule.prod.len() {
                    write!(
                        &mut derive,
                        "{}\t\tlet count{} = self.possible_derivations[&leaves[{}]];\n",
                        indent, i, i
                    )
                    .unwrap();
                }
                for i in 0..rule.prod.len() {
                    if non_terminals.contains(&rule.prod[i]) {
                        write!(
                            &mut derive,
                            "{}\t\tlet index{} = self.derive{}(leaves[{}], (derivation",
                            indent, i, rule.prod[i], i
                        )
                        .unwrap();
                        for j in (i + 1)..rule.prod.len() {
                            write!(&mut derive, " / count{}", j).unwrap();
                        }
                        write!(&mut derive, ") % count{});\n", i).unwrap();
                    }
                }
                for i in 0..rule.prod.len() {
                    if non_terminals.contains(&rule.prod[i]) {
                        write!(
                            &mut derive,
                            "{}\t\tlet arg{} = &self.storage.arena_{}[index{}];\n",
                            indent, i, rule.prod[i], i
                        )
                        .unwrap();
                    } else {
                        write!(
                            &mut derive,
                            "{}\t\tlet arg{} = &self.input[self.state.sppf_nodes[leaves[{}]].left_extent()];\n",
                            indent, i, i,
                        )
                        .unwrap();
                    }
                }
                write!(
                    &mut derive,
                    "{}\t\tlet index = self.storage.arena_{}.len();\n",
                    indent, nt
                )
                .unwrap();
                write!(&mut derive, "{}\t\tself.storage.map_{}.entry(node).or_insert_with(|| Vec::new()).push(index);\n", indent, nt).unwrap();
                write!(
                    &mut derive,
                    "{}\t\tlet res = self.parser.parse{}_{}(",
                    indent, nt, rule_index
                )
                .unwrap();
                for arg in 0..rule.arg.len() {
                    write!(&mut derive, "arg{}, ", arg).unwrap();
                }
                write!(&mut derive, ");\n").unwrap();
                write!(
                    &mut derive,
                    "{}\t\tself.storage.arena_{}.push(res);\n",
                    indent, nt
                )
                .unwrap();
                write!(&mut derive, "{}\t\treturn index;\n", indent).unwrap();
                write!(&mut derive, "{}\t}}\n", indent).unwrap();
            }
        }
        let indent = "\t\t";
        write!(
            &mut derive,
            "{}\t\t\t\t\t_ => panic!(\"Impossible packed node for {}: {{:?}}\", l)\n",
            indent, nt
        )
        .unwrap();
        write!(&mut derive, "{}\t\t\t\t}}\n", indent).unwrap();
        write!(&mut derive, "{}\t\t\t}}\n", indent).unwrap();
        write!(&mut derive, "{}\t\t}} else {{\n", indent).unwrap();
        write!(
            &mut derive,
            "{}\t\t\tderivation -= self.possible_derivations[child];\n",
            indent
        )
        .unwrap();
        write!(&mut derive, "{}\t\t}}\n", indent).unwrap();
        write!(&mut derive, "{}\t}}\n", indent).unwrap();
        write!(&mut derive, "{}\tunimplemented!()\n", indent).unwrap();
        write!(&mut derive, "{}}}\n", indent).unwrap();
    }

    // arenas and maps
    let mut arenas = String::new();
    let mut maps = String::new();
    for nt in &non_terminals {
        let ty = &rules.iter().find(|rule| rule.name == *nt).unwrap().ty;
        write!(&mut arenas, "\t\tarena_{}: Vec<{}>,\n", nt, ty).unwrap();
        write!(&mut maps, "\t\tmap_{}: BTreeMap<usize, Vec<usize>>,\n", nt).unwrap();
    }

    let template = include_str!("template/gll.rs.template");
    let pattern = [
        "{parser_type}",
        "{labels}",
        "{token}",
        "{source}",
        "{res_type}",
        "{symbol_terminals}",
        "{symbol_non_terminals}",
        "{label_first}",
        "{label_end}",
        "{start_symbol}",
        "{states}",
        "{arenas}",
        "{maps}",
        "{derive}",
    ];
    let replace = [
        // "{parser_type}"
        &parser_def,
        // "{labels}"
        &labels,
        // "{token}"
        &token,
        // "{source}"
        "&str",
        // "{res_type}"
        &start_type,
        // "{symbol_terminals}"
        &symbol_terminals,
        // "{symbol_non_terminals}"
        &symbol_non_terminals,
        // "{label_first}"
        &label_first,
        // "{label_end}"
        &label_end,
        // "{start_symbol}"
        &start_symbol,
        // "{states}"
        &states,
        // "{arenas}"
        &arenas,
        // "{maps}"
        &maps,
        // "{derive}"
        &derive,
    ];

    let str = AhoCorasick::new(&pattern)
        .unwrap()
        .replace_all(template, &replace);
    let mut stream: TokenStream = str.parse().unwrap();
    let mut parsers = TokenStream::new();

    // parsers
    // parseS_0
    for (rule_index, rule) in rules.iter().enumerate() {
        let name = format_ident!("parse{}_{}", rule.name, rule_index);
        let parser = format_ident!("{}", parser_def);
        let args = rule.arg.iter().map(|(_name, _ty, pat)| pat);
        let body = rule.body.clone();
        let output = rule.sig.output.clone();
        let parser = quote! {
            impl #parser {
                fn #name(&mut self, #(#args),* ) #output {
                    #body
                }
            }
        };
        parsers.extend::<TokenStream>(parser.into());
    }
    if config.verbose {
        let mut file = File::create("gll-gen.rs").unwrap();
        write!(file, "{}", str).unwrap();
        write!(file, "{}", parsers).unwrap();
    }
    stream.extend(parsers);

    stream
}

fn gen_token_stream(attr: proc_macro::TokenStream, parser_impl: syn::ItemImpl) -> TokenStream {
    let mut iter = attr.clone().into_iter();
    let start_symbol = match iter.next() {
        Some(proc_macro::TokenTree::Ident(ident)) => ident.to_string(),
        _ => panic!("Fail to parse start non-term, expect `#[lalr1(StartName, TokenClass)]."),
    };
    match iter.next() {
        Some(proc_macro::TokenTree::Punct(punct)) if punct.as_char() == ',' => {}
        _ => panic!("Fail to parse start non-term, expect `#[lalr1(StartName, TokenClass)]."),
    };
    let token = match iter.next() {
        Some(proc_macro::TokenTree::Ident(ident)) => ident.to_string(),
        _ => panic!("Fail to parse start non-term, expect `#[lalr1(StartName, TokenClass)]."),
    };
    let config = gen_config(&parser_impl);

    gen_template(start_symbol, token, &parser_impl, &config)
}

pub fn generate(
    attr: proc_macro::TokenStream,
    parser_impl: syn::ItemImpl,
) -> proc_macro::TokenStream {
    gen_token_stream(attr, parser_impl)
}
