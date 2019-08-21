extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as syn::DeriveInput);
    let debug_impl = generate_debug_impl(&ast).unwrap();

    debug_impl.into()
}

fn generate_debug_impl(
    ast: &syn::DeriveInput,
) -> std::result::Result<proc_macro2::TokenStream, syn::Error> {
    let struct_name = &ast.ident;
    let struct_name_string = struct_name.to_string();

    if let syn::Data::Struct(syn::DataStruct { ref fields, .. }) = ast.data {
        let field_idents = fields.iter().map(|field| &field.ident);
        let field_names = fields
            .iter()
            .map(|field| field.ident.as_ref().unwrap().to_string());

        return Ok(quote! {
            impl std::fmt::Debug for #struct_name {
                fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                    fmt.debug_struct(#struct_name_string)
                      #(.field(#field_names, &self.#field_idents))*
                       .finish()
                }
            }
        });
    }

    Err(syn::Error::new_spanned(&struct_name, "Unknown data fields"))
}
