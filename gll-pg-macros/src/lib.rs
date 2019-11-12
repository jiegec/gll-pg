#![feature(proc_macro_diagnostic)]

extern crate proc_macro;

mod gen;

#[proc_macro_attribute]
pub fn gll(attr: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    gen::generate(attr, input)
}