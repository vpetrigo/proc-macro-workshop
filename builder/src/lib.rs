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

    let expanded = quote! {
        impl #struct_name {
            pub fn builder() -> #struct_builder {
                #struct_builder {
                    #(#builder_fields_empty),*
                }
            }
        }

        pub struct #struct_builder {
            #(#builder_fields),*
        }

        impl #struct_builder {
            #(#methods)*

            pub fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                // ..
                Ok(#struct_name {
                    #(#constructor_fields),*
                })
            }
        }
    };

    TokenStream::from(expanded)
}

fn get_fields(ast: &DeriveInput) -> &syn::punctuated::Punctuated<syn::Field, syn::Token![,]> {
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!("Can not handle {:?}", ast.data);
    };

    fields
}

fn get_builder_name(structure: &syn::Ident) -> String {
    format!("{}Builder", structure.to_string())
}

fn type_of(type_name: &str, ty: &syn::Type) -> bool {
    if let syn::Type::Path(typath) = &ty {
        if typath.path.segments.len() == 1 && typath.path.segments[0].ident == type_name {
            true
        } else {
            false
        }
    } else {
        unimplemented!()
    }
}

fn get_option_inner_type(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { segments, .. },
        ..
    }) = &ty
    {
        if segments.len() != 1 {
            return None;
        }

        if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
            ref args,
            ..
        }) = segments[0].arguments
        {
            if args.len() != 1 {
                return None;
            }

            if let syn::GenericArgument::Type(ref inner_type) = args[0] {
                return Some(inner_type);
            }
        }
    }

    None
}

fn generate_fun_internal(name: &Option<syn::Ident>) -> proc_macro2::TokenStream {
    quote! {
        self.#name = Some(#name);
        self
    }
}

fn generate_builder_functions<'a, P>(
    fields: &'a syn::punctuated::Punctuated<syn::Field, P>,
) -> impl Iterator<Item = proc_macro2::TokenStream> + 'a {
    fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        let test = generate_fun_internal(name);

        if type_of("Option", ty) {
            let inner_type = get_option_inner_type(ty);
            quote! {
                fn #name(&mut self, #name: #inner_type) -> &mut Self {
                    #test
                }
            }
        } else {
            quote! {
                fn #name(&mut self, #name: #ty) -> &mut Self {
                    #test
                }
            }
        }
    })
}

fn generate_emtpy_fields<'a, P>(
    fields: &'a syn::punctuated::Punctuated<syn::Field, P>,
) -> impl Iterator<Item = proc_macro2::TokenStream> + 'a {
    fields.iter().map(|field| {
        let name = &field.ident;

        quote! {
        #name: None
        }
    })
}

fn generate_builder_fields<'a, P>(
    fields: &'a syn::punctuated::Punctuated<syn::Field, P>,
) -> impl Iterator<Item = proc_macro2::TokenStream> + 'a {
    fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        let optional = if type_of("Option", ty) {
            quote! {
                #name: #ty
            }
        } else {
            quote! {
                #name: Option<#ty>
            }
        };

        optional
    })
}

fn generate_constructor_fields<'a, P>(
    fields: &'a syn::punctuated::Punctuated<syn::Field, P>,
) -> impl Iterator<Item = proc_macro2::TokenStream> + 'a {
    fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;

        if type_of("Option", ty) {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    })
}
