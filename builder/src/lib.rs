extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;
use syn::DeriveInput;
use proc_macro2::Ident;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let struct_name: &Ident = &ast.ident;
    let fields = get_fields(&ast);
    let builder_fields = generate_builder_fields(fields);
    let constructor_fields = generate_constructor_fields(fields);
    let builder_fields_empty = generate_emtpy_fields(fields);
    let struct_builder_name = get_builder_name(&struct_name);
    let struct_builder = syn::Ident::new(&struct_builder_name, struct_name.span());
    let methods = generate_builder_functions(fields);

    unimplemented!()
}
