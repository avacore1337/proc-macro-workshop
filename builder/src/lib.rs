extern crate proc_macro;

use syn::{parse_macro_input, DeriveInput};

use proc_macro::TokenStream;
use quote::quote;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    eprintln!("TOKENS: {:#?}", ast);
    TokenStream::new()
}
