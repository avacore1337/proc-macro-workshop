#![recursion_limit = "256"]

extern crate proc_macro;

use syn::{parse_macro_input, DeriveInput, Ident};

use proc_macro::TokenStream;
use quote::quote;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    // eprintln!("TOKENS: {:#?}", ast);

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
        if builder_of(f).is_some() {
            if let Some(v_ty) = vec_type(ty) {
                quote! { #name: std::vec::Vec<#v_ty> }
            } else {
                unimplemented!();
            }
        } else if let Some(_) = option_type(ty) {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    let builder_init = fields.iter().map(|f| {
        let name = &f.ident;
        if builder_of(f).is_some() {
            quote! { #name: std::vec::Vec::new() }
        } else {
            quote! { #name: std::option::Option::None }
        }
    });

    let builder_methods = fields.iter().map(|f| {
        let name = f.ident.as_ref().expect("all fields must be named");
        let ty = &f.ty;
        if let Some(attr) = builder_of(f) {
            extend_method(attr, name, ty)
        } else if let Some(i_ty) = option_type(ty) {
            quote! {
                pub fn #name(&mut self, #name: #i_ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            }
        }
    });

    let builder_params = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if option_type(ty).is_some() || builder_of(f).is_some() {
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
            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                Ok(#name{
                    #(#builder_params,)*
                })
            }
        }
    };

    expanded.into()
}

fn extend_method(
    a: &syn::Attribute,
    name: &syn::Ident,
    ty: &syn::Type,
) -> syn::export::TokenStream2 {
    if let Ok(syn::Meta::List(meta)) = a.parse_meta() {
        // eprintln!("META: {:#?}", meta);
        if meta.nested.len() != 1 {
            panic!("bad code");
        }

        if let syn::NestedMeta::Meta(syn::Meta::NameValue(ref name_value_meta)) = meta.nested[0] {
            if a.path.segments.len() != 1 || name_value_meta.path.segments[0].ident != "each" {
                return syn::Error::new_spanned(meta, "expected `builder(each = \"...\")`")
                    .to_compile_error();
            }
            if let syn::Lit::Str(ref s) = name_value_meta.lit {
                let method_name = Ident::new(&s.value(), s.span());
                if let Some(v_ty) = vec_type(ty) {
                    return quote! {
                        pub fn #method_name(&mut self, #name: #v_ty) -> &mut Self {
                            self.#name.push(#name);
                            self
                        }
                    };
                }
            }
        }
    }
    panic!("bad code");
}

fn builder_of(f: &syn::Field) -> Option<&syn::Attribute> {
    for attr in &f.attrs {
        let segment = attr.path.segments.first()?;
        if segment.ident == "builder" {
            return Some(attr);
        }
    }
    None
}

// META: MetaList {
//     path: Path {
//         leading_colon: None,
//         segments: [
//             PathSegment {
//                 ident: Ident {
//                     ident: "builder",
//                     span: #0 bytes(1466..1473),
//                 },
//                 arguments: None,
//             },
//         ],
//     },
//     paren_token: Paren,
//     nested: [
//         Meta(
//             NameValue(
//                 MetaNameValue {
//                     path: Path {
//                         leading_colon: None,
//                         segments: [
//                             PathSegment {
//                                 ident: Ident {
//                                     ident: "each",
//                                     span: #0 bytes(1474..1478),
//                                 },
//                                 arguments: None,
//                             },
//                         ],
//                     },
//                     eq_token: Eq,
//                     lit: Str(
//                         LitStr {
//                             token: "env",
//                         },
//                     ),
//                 },
//             ),
//         ),
//     ],
// }

fn get_subtype<'a>(ty: &'a syn::Type, name: &str) -> Option<&'a syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() == 1 {
            let p_seg = p.path.segments.first().unwrap();
            if p_seg.ident == name {
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
fn option_type<'a>(ty: &'a syn::Type) -> Option<&'a syn::Type> {
    get_subtype(ty, "Option")
}

fn vec_type<'a>(ty: &'a syn::Type) -> Option<&'a syn::Type> {
    get_subtype(ty, "Vec")
}
