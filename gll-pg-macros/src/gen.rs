//! Much of code below is derived from MashPlant/lalr1.

use aho_corasick::AhoCorasick;
use quote::ToTokens;
use std::collections::HashSet;
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
    Arg { name: Option<String>, ty: String },
}

fn parse_arg(arg: &syn::FnArg) -> ArgInfo {
    match arg {
        syn::FnArg::SelfRef(_) => ArgInfo::Self_,
        syn::FnArg::SelfValue(_) => ArgInfo::Self_,
        syn::FnArg::Captured(arg) => ArgInfo::Arg {
            name: Some(arg.pat.clone().into_token_stream().to_string()),
            ty: arg.ty.clone().into_token_stream().to_string(),
        },
        // what is this?
        syn::FnArg::Inferred(_) => unimplemented!("syn::FnArg::Inferred"),
        syn::FnArg::Ignored(ty) => ArgInfo::Arg {
            name: None,
            ty: ty.into_token_stream().to_string(),
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
    arg: Vec<(Option<String>, String)>,
}

fn gen_template(start: String, parser_impl: &syn::ItemImpl, config: &GenConfig) -> String {
    let parser_type = parser_impl.self_ty.as_ref();
    let parser_def = parser_type.into_token_stream().to_string();
    let mut rules = vec![];
    for item in &parser_impl.items {
        if let syn::ImplItem::Method(method) = item {
            let attr = method.attrs.get(0).unwrap();
            let rule = attr.tts.to_string();
            let rule = rule[1..rule.len() - 1].trim();
            let mut rule_split = rule.split_whitespace();
            let lhs = match rule_split.next() {
                Some(lhs) => lhs.to_owned(),
                None => panic!(
                    "The rule `{}` method `{}` defined doesn't have a valid lhs.",
                    rule, method.sig.ident
                ),
            };
            let lhs_ty = match &method.sig.decl.output {
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
            let rhs_arg = method
                .sig
                .decl
                .inputs
                .iter()
                .map(parse_arg)
                .collect::<Vec<_>>();
            let skip_self = match rhs_arg.get(0) {
                Some(ArgInfo::Self_) => 1,
                _ => 0,
            };
            let rhs_arg: Vec<(Option<String>, String)> = rhs_arg
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

    // labels
    let mut labels = String::new();
    let mut label_first = String::new();
    let mut label_end = String::new();
    for terminal in &terminals {
        write!(&mut labels, "\t\tL{},\n", terminal).unwrap();
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
        "isize",
        // "{symbol_terminals}"
        &symbol_terminals,
        // "{symbol_non_terminals}"
        &symbol_non_terminals,
        // "{label_first}"
        &label_first,
        // "{label_end}"
        &label_end,
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
