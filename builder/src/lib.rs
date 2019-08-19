extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::parse_macro_input;
use syn::spanned::Spanned;
use syn::DeriveInput;

#[proc_macro_derive(Builder, attributes(builder))]
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

            pub fn build(&mut self) -> std::result::Result<#struct_name, std::boxed::Box<dyn std::error::Error>> {
                // ..
                std::result::Result::Ok(#struct_name {
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

fn is_builder_field(field: &syn::Field) -> bool {
    for attr in &field.attrs {
        if attr.path.segments.len() != 0 && attr.path.segments[0].ident == "builder" {
            return true;
        }
    }

    false
}

fn get_inner_type(ty: &syn::Type) -> Option<&syn::Type> {
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

enum BuilderAttr {
    Ok(syn::Ident),
    Err(proc_macro2::TokenStream),
}

fn get_builder_attr(field: &syn::Field) -> Option<BuilderAttr> {
    if is_builder_field(field) && field.attrs.len() > 0 {
        for attr in &field.attrs {
            match attr.parse_meta() {
                Ok(syn::Meta::List(list)) => {
                    assert!(
                        list.path
                            .segments
                            .iter()
                            .any(|segment| segment.ident == "builder"),
                        "Does not have #[builder]"
                    );

                    if list.nested.len() != 1 {
                        return Some(BuilderAttr::Err(
                            syn::Error::new_spanned(list, "expected `builder(each = \"...\")`")
                                .to_compile_error(),
                        ));
                    }

                    if let syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue {
                        path: syn::Path { ref segments, .. },
                        lit: syn::Lit::Str(ref litstr),
                        ..
                    })) = list.nested[0]
                    {
                        if segments[0].ident != "each" {
                            return Some(BuilderAttr::Err(
                                syn::Error::new_spanned(list, "expected `builder(each = \"...\")`")
                                    .to_compile_error(),
                            ));
                        }

                        return Some(BuilderAttr::Ok(syn::Ident::new(
                            litstr.value().as_str(),
                            field.span(),
                        )));
                    }
                }
                _ => (),
            }
        }
    }

    None
}

fn generate_fun_internal(name: &Option<syn::Ident>) -> proc_macro2::TokenStream {
    quote! {
        self.#name = std::option::Option::Some(#name);
        self
    }
}

fn generate_builder_functions<'a, P>(
    fields: &'a syn::punctuated::Punctuated<syn::Field, P>,
) -> impl Iterator<Item = proc_macro2::TokenStream> + 'a {
    fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        let fun_internal = generate_fun_internal(name);

        if type_of("Option", ty) || is_builder_field(field) {
            let inner_type = get_inner_type(ty);

            if is_builder_field(field) {
                let attr_name = get_builder_attr(field);

                match attr_name {
                    Some(BuilderAttr::Ok(attr_name)) => quote! {
                        fn #attr_name(&mut self, #attr_name: #inner_type) -> &mut Self {
                            self.#name.push(#attr_name);
                            self
                        }
                    },
                    Some(BuilderAttr::Err(error)) => quote! {
                        #error
                    },
                    None => quote! {},
                }
            } else {
                quote! {
                    fn #name(&mut self, #name: #inner_type) -> &mut Self {
                        #fun_internal
                    }
                }
            }
        } else {
            quote! {
                fn #name(&mut self, #name: #ty) -> &mut Self {
                    #fun_internal
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

        if is_builder_field(field) {
            quote! {
                #name: std::vec::Vec::new()
            }
        } else {
            quote! {
                #name: std::option::Option::None
            }
        }
    })
}

fn generate_builder_fields<'a, P>(
    fields: &'a syn::punctuated::Punctuated<syn::Field, P>,
) -> impl Iterator<Item = proc_macro2::TokenStream> + 'a {
    fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        let optional = if type_of("Option", ty) || is_builder_field(field) {
            quote! {
                #name: #ty
            }
        } else {
            quote! {
                #name: std::option::Option<#ty>
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

        if type_of("Option", ty) || is_builder_field(field) {
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
