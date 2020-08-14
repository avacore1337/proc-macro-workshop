extern crate proc_macro;

use proc_macro::TokenStream;

use syn::{parse_macro_input, DeriveInput};

use quote::quote;

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    eprintln!("TOKENS: {:#?}", ast);

    let struct_name = &ast.ident;

    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        panic!("Struct has no fields");
    };
    let debug_fields = fields.iter().map(|f| {
        let name = &f.ident;
        // let ty = &f.ty;
        // .field("y", &self.y)
        quote! { .field(stringify!(#name), &self.#name) }
    });
    let expanded = quote! {
        impl std::fmt::Debug for #struct_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    // stringify!(#name)
                f.debug_struct(stringify!(#struct_name))
                #(#debug_fields)*
                .finish()
            }
        }
        };

    expanded.into()
}
