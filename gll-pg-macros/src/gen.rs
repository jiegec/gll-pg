//! Much of code below is derived from MashPlant/lalr1.

use aho_corasick::AhoCorasick;
use quote::ToTokens;
use std::collections::{HashMap, HashSet};
use std::fmt::Write as FmtWrite;
use std::fs::File;
use std::io::Write;
use syn;

#[derive(Default)]
struct GenConfig {
    verbose: bool,
}

enum ArgInfo {
    Self_,
    Arg { name: String, ty: String },
}

fn parse_arg(arg: &syn::FnArg) -> ArgInfo {
    match arg {
        syn::FnArg::Receiver(_) => ArgInfo::Self_,
        syn::FnArg::Typed(arg) => ArgInfo::Arg {
            name: arg.pat.clone().into_token_stream().to_string(),
            ty: arg.ty.clone().into_token_stream().to_string(),
        },
    }
}

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

struct ProdRule {
    name: String,
    ty: String,
    // name, var, type
    prod: Vec<String>,
    arg: Vec<(String, String)>,
    body: String,
}

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

fn gen_template(start_symbol: String, parser_impl: &syn::ItemImpl, config: &GenConfig) -> String {
    let parser_type = parser_impl.self_ty.as_ref();
    let parser_def = parser_type.into_token_stream().to_string();
    let mut rules = vec![];
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
            let rhs_arg: Vec<(String, String)> = rhs_arg
                .into_iter()
                .skip(skip_self)
                .map(|arg| match arg {
                    ArgInfo::Self_ => panic!(
                        "Method `{}` takes self argument at illegal position.",
                        method.sig.ident
                    ),
                    ArgInfo::Arg { name, ty } => (name, ty),
                })
                .collect();
            if config.verbose {
                println!("lhs {:?} {:?}", lhs, lhs_ty);
                println!("rhs {:?} {:?}", rhs, rhs_arg);
            }
            rules.push(ProdRule {
                name: lhs,
                ty: lhs_ty,
                prod: rhs,
                arg: rhs_arg,
                body: method.block.to_token_stream().to_string(),
            });
        } else {
            panic!("Impl block of gll should only contain methods.");
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
        println!("T {:?}", terminals);
        println!("NT {:?}", non_terminals);
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
        println!("FIRST SET:");
        for (nt, set) in &first_set {
            print!("{}:", nt);
            for s in set.iter() {
                match s {
                    Some(s) => print!(" {}", s),
                    None => print!(" Eps"),
                }
            }
            println!();
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
        println!("FOLLOW SET:");
        for (nt, set) in &follow_set {
            print!("{}:", nt);
            for s in set.iter() {
                match s {
                    Some(s) => print!(" {}", s),
                    None => print!(" Eps"),
                }
            }
            println!();
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
            "\t\tL{}_{},// {} -> . {}\n",
            rule.name,
            rule_index,
            rule.name,
            rule.prod.join(" ")
        )
        .unwrap();
        if rule.prod.len() > 0 {
            // not eps
            for prod_index in 1..rule.prod.len() + 1 {
                write!(
                    &mut labels,
                    "\t\tL{}_{}_{}, // {} -> {} . {}\n",
                    rule.name,
                    rule_index,
                    prod_index,
                    rule.name,
                    rule.prod[0..prod_index].join(" "),
                    rule.prod[prod_index..].join(" ")
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

        if rule.prod.len() > 1 {
            // TODO: check nullable
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
                        write!(&mut states, "Token::{}, ", t.unwrap()).unwrap();
                    }
                    write!(
                        &mut states,
                        "].contains(&input[state.current_position]) {{\n"
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
                        "{}\tif input[state.current_position] == Token::{} {{\n",
                        indent, prod
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
                            write!(&mut states, "Token::{}, ", t.unwrap()).unwrap();
                        }
                        write!(
                            &mut states,
                            "].contains(&input[state.current_position]) {{\n"
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

    // parsers
    let mut parsers = String::new();
    let indent = "\t";
    // parseS
    for nt in &non_terminals {
        write!(&mut parsers, "{}fn parse{}(input: &Vec<Token>, state: &gll_pg_core::GSSState<gll_generated::Label>, node: gll_pg_core::SPPFNodeIndex) -> Vec<i32> {{\n", indent, nt).unwrap();
        write!(&mut parsers, "{}\tlet mut res = vec![];\n", indent).unwrap();
        write!(
            &mut parsers,
            "{}\tfor child in state.sppf_nodes[node].children().unwrap() {{\n",
            indent
        )
        .unwrap();
        write!(
            &mut parsers,
            "{}\t\tlet node = &state.sppf_nodes[*child];\n",
            indent
        )
        .unwrap();
        write!(
            &mut parsers,
            "{}\t\tif let gll_pg_core::SPPFNode::Packed(l, k, c) = node {{\n",
            indent
        )
        .unwrap();
        write!(
            &mut parsers,
            "{}\t\t\tlet leaves = state.collect_symbols(*child);\n",
            indent
        )
        .unwrap();
        write!(&mut parsers, "{}\t\t\tmatch l {{\n", indent).unwrap();
        for (rule_index, rule) in rules.iter().enumerate() {
            if rule.name == *nt {
                if rule.prod.len() > 0 {
                    // not eps
                    write!(
                        &mut parsers,
                        "{}\t\t\t\tgll_generated::Label::L{}_{}_{} => {{\n",
                        indent,
                        nt,
                        rule_index,
                        rule.prod.len()
                    )
                    .unwrap();
                } else {
                    // eps
                    write!(
                        &mut parsers,
                        "{}\t\t\t\tgll_generated::Label::L{}_{} => {{\n",
                        indent, nt, rule_index
                    )
                    .unwrap();
                };
                let mut more_indent = String::new();
                for i in 0..rule.prod.len() {
                    more_indent.push_str("\t");
                    if terminals.contains(&rule.prod[i]) {
                        write!(
                            &mut parsers,
                            "{}\t\t\t\t{}let arg{} = input[state.sppf_nodes[leaves[{}]].left_extent()].clone(); {{\n",
                            indent, more_indent, i, i
                        )
                        .unwrap();
                    } else {
                        write!(
                            &mut parsers,
                            "{}\t\t\t\t{}for arg{} in Self::parse{}(input, state, leaves[{}]) {{\n",
                            indent, more_indent, i, rule.prod[i], i
                        )
                        .unwrap();
                    }
                }
                write!(
                    &mut parsers,
                    "{}\t\t\t\t\t{}res.push(Self::parse{}_{}(",
                    indent, more_indent, rule.name, rule_index,
                )
                .unwrap();
                for i in 0..rule.prod.len() {
                    write!(&mut parsers, "arg{}, ", i).unwrap();
                }
                write!(&mut parsers, "));\n",).unwrap();
                for _ in 0..rule.prod.len() {
                    write!(&mut parsers, "{}\t\t\t\t{}}}\n", indent, more_indent).unwrap();
                    more_indent.split_off(more_indent.len() - "\t".len());
                }
                write!(&mut parsers, "{}\t\t\t\t}}\n", indent).unwrap();
            }
        }
        write!(
            &mut parsers,
            "{}\t\t\t\t_ => panic!(\"Impossible packed node for {}: {{:?}}\", l)\n",
            indent, nt
        )
        .unwrap();
        write!(&mut parsers, "{}\t\t\t}}\n", indent).unwrap();
        write!(&mut parsers, "{}\t\t}}\n", indent).unwrap();
        write!(&mut parsers, "{}\t}}\n", indent).unwrap();
        write!(&mut parsers, "{}\tres\n", indent).unwrap();
        write!(&mut parsers, "{}}}\n", indent).unwrap();
    }
    // parseS_0
    for (rule_index, rule) in rules.iter().enumerate() {
        write!(
            &mut parsers,
            "{}fn parse{}_{}(",
            indent, rule.name, rule_index
        )
        .unwrap();
        for arg in &rule.arg {
            write!(&mut parsers, "{}: {},", arg.0, arg.1).unwrap();
        }
        write!(&mut parsers, ") -> {} {{\n", rule.ty).unwrap();
        write!(&mut parsers, "{}{}\n", indent, rule.body).unwrap();
        write!(&mut parsers, "{}}}\n", indent).unwrap();
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
        "{parsers}",
    ];
    let replace = [
        // "{parser_type}"
        &parser_def,
        // "{labels}"
        &labels,
        // "{token}"
        "Token",
        // "{source}"
        "&str",
        // "{res_type}"
        "i32",
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
        // "{parsers}"
        &parsers,
    ];

    AhoCorasick::new(&pattern).replace_all(template, &replace)
}

fn gen_string(attr: proc_macro::TokenStream, input: proc_macro::TokenStream) -> String {
    // handle attrs
    let parser_impl = match syn::parse::<syn::ItemImpl>(input) {
        Ok(parser_impl) => parser_impl,
        Err(_) => panic!("Attribute `gll` can only be applied to an impl block."),
    };
    let start = match attr.clone().into_iter().next() {
        Some(proc_macro::TokenTree::Ident(ident)) => ident.to_string(),
        _ => panic!("Fail to parse start non-term, expect `#[lalr1(StartName)]."),
    };
    let config = gen_config(&parser_impl);

    let res = gen_template(start, &parser_impl, &config);
    // replace
    if config.verbose {
        let mut file = File::create("gll-gen.rs").unwrap();
        write!(file, "{}", res).unwrap();
    }
    res
}

pub fn generate(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let string = gen_string(attr, input);
    string.parse().unwrap()
}
