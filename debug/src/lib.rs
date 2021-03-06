extern crate proc_macro;

use proc_macro::TokenStream;

use syn::{parse_macro_input, parse_quote, DeriveInput, GenericParam, Generics};

use quote::quote;

fn extract_fields(data: syn::Data) -> syn::punctuated::Punctuated<syn::Field, syn::token::Comma> {
    match data {
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
            ..
        }) => named.clone(),
        _ => panic!("Struct has no fields"),
    }
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    eprintln!("TOKENS: {:#?}", ast);

    let struct_name = &ast.ident;
    let generics = ast.generics;

    let fields = extract_fields(ast.data);
    let debug_fields = fields.iter().map(|f| {
        let name = &f.ident;
        // let ty = &f.ty;
        // .field("y", &self.y)
        if let Some(attr) = debug_of(f) {
            let formating = extract_formating(attr);
            quote! { .field(stringify!(#name), &format_args!(#formating, &self.#name)) }
        } else {
            quote! { .field(stringify!(#name), &self.#name) }
        }
    });
    let expanded = if generics.gt_token.is_some() {
        let bounded_generics = add_trait_bounds(generics);
        let (impl_generics, ty_generics, where_clause) = bounded_generics.split_for_impl();
        eprintln!("generics: {:#?}", impl_generics);
        quote! {
        impl #impl_generics std::fmt::Debug for #struct_name #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!(#struct_name))
                #(#debug_fields)*
                .finish()
            }
        }
        }
    } else {
        quote! {
        impl std::fmt::Debug for #struct_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!(#struct_name))
                #(#debug_fields)*
                .finish()
            }
        }
        }
    };

    expanded.into()
}

// Add a bound `T: Debug` to every type parameter T.
fn add_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }
    generics
}

fn debug_of(f: &syn::Field) -> Option<&syn::Attribute> {
    for attr in &f.attrs {
        let segment = attr.path.segments.first()?;
        if segment.ident == "debug" {
            return Some(attr);
        }
    }
    None
}

fn extract_formating(a: &syn::Attribute) -> syn::LitStr {
    match a.parse_meta() {
        Ok(syn::Meta::NameValue(syn::MetaNameValue {
            lit: syn::Lit::Str(ref s),
            ..
        })) => return s.clone(),
        _ => panic!("bad code"),
    }
}
