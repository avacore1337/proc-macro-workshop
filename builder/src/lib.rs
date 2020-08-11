#![recursion_limit = "256"]

extern crate proc_macro;

use syn::{parse_macro_input, DeriveInput, Ident};

use proc_macro::TokenStream;
use quote::quote;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    eprintln!("TOKENS: {:#?}", ast);

    let name = &ast.ident;
    let bname = format!("{}Builder", name);
    let bident = Ident::new(&bname, name.span());
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!();
        // panic!("Struct has no fields");
    };
    let builder_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if let Some(_) = option_type(ty) {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    let builder_init = fields.iter().map(|f| {
        let name = &f.ident;
        quote! { #name: None }
    });

    let builder_methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if let Some(i_ty) = option_type(ty) {
            quote! {
                pub fn #name(&mut self, #name: #i_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        }
    });

    let builder_params = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if option_type(ty).is_some() {
            quote! { #name: self.#name.clone()}
        } else {
            quote! { #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))? }
        }
    });

    let expanded = quote! {
        pub struct #bident {
            #(#builder_fields,)*
        }
        impl #name {
            pub fn builder() -> #bident{
                #bident {
                    #(#builder_init,)*
                }
            }
        }
        impl #bident {
            #(#builder_methods)*
            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name{
                    #(#builder_params,)*
                })
            }
        }
    };

    expanded.into()
}

fn option_type<'a>(ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() == 1 {
            let p_seg = p.path.segments.first().unwrap();
            if p_seg.ident == "Option" {
                if let syn::PathArguments::AngleBracketed(ref angle) = p_seg.arguments {
                    if angle.args.len() != 1 {
                        return None;
                    }
                    if let syn::GenericArgument::Type(ref inner_type) = angle.args[0] {
                        return Some(inner_type);
                    }
                }
            }
        }
    }
    None
}
