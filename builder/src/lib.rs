extern crate proc_macro;

use syn::{parse_macro_input, DeriveInput, Ident};

use proc_macro::TokenStream;
// use proc_macro2::{Ident, Span};
use quote::quote;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    eprintln!("TOKENS: {:#?}", ast);

    let name = &ast.ident;
    let bname = format!("{}Builder", name);
    let bident = Ident::new(&bname, name.span());
    let expanded = quote! {
        pub struct #bident {
            executable: Option<String>,
            args: Option<Vec<String>>,
            env: Option<Vec<String>>,
            current_dir: Option<String>,
        }
        impl #name {
            pub fn builder() -> #bident{
                #bident {
                    executable: None,
                    args: None,
                    env: None,
                    current_dir: None,
                }
            }
        }
        impl #bident {
            pub fn executable(&mut self, executable: String) -> &mut Self {
                self.executable = Some(executable);
                self
            }
            pub fn args(&mut self, args: Vec<String>) -> &mut Self {
                self.args = Some(args);
                self
            }
            pub fn env(&mut self, env: Vec<String>) -> &mut Self {
                self.env = Some(env);
                self
            }
            pub fn current_dir(&mut self, current_dir: String) -> &mut Self {
                self.current_dir = Some(current_dir);
                self
            }
            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name{
                    executable: self.executable.clone().ok_or("executable is not set")?,
                    args: self.args.clone().ok_or("args is not set")?,
                    env: self.env.clone().ok_or("env is not set")?,
                    current_dir: self.current_dir.clone().ok_or("current_dir is not set")?,
                })
            }
        }
    };

    expanded.into()
}
