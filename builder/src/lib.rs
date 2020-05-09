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
        quote! { #name: std::option::Option<#ty> }
    });

    let builder_init = fields.iter().map(|f| {
        let name = &f.ident;
        quote! { #name: None }
    });

    let builder_methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        quote! {
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });

    let builder_params = fields.iter().map(|f| {
        let name = &f.ident;
        quote! { #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))? }
        // let ty = &f.ty;
        // if option_type(ty).is_some() {
        //     quote! { #name: self.#name.clone()}
        // } else {
        //     quote! { #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))? }
        // }
        // ty: Path(
        //                         TypePath {
        //                             qself: None,
        //                             path: Path {
        //                                 leading_colon: None,
        //                                 segments: [
        //                                     PathSegment {
        //                                         ident: Ident {
        //                                             ident: "Option",
        //                                             span: #0 bytes(2884..2890),
        //                                         }},
        //                                         arguments: AngleBracketed(
        //                                             AngleBracketedGenericArguments {
        //                                                 colon2_token: None,
        //                                                 lt_token: Lt,
        //                                                 args: [
        //                                                     Type(
        //                                                         Path(
        //                                                             TypePath {
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
                    // executable: self.executable.clone().ok_or("executable is not set")?,
                    // args: self.args.clone().ok_or("args is not set")?,
                    // env: self.env.clone().ok_or("env is not set")?,
                    // current_dir: self.current_dir.clone().ok_or("current_dir is not set")?,
                })
            }
        }
    };

    expanded.into()
}

fn option_type<'a>(ty: &'a syn::Type) -> Option<&'a syn::Type> {
    // let Path{TypePath segment PathSegment { Ident { ident "Option", args }
    //             = ty { args.get(0)}
    //             else {
    //                 ty
    //             }

    Some(ty)
}

// fn ty_inner_type<'a>(wrapper: &str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
//     None
// }
