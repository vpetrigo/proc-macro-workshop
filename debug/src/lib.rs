extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, parse_quote};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as syn::DeriveInput);
    let debug_impl = generate_debug_impl(&ast).unwrap();

    debug_impl.into()
}

fn add_trait_bound(
    generics: &syn::Generics,
    ty: syn::TypeParamBound,
    pred: impl Fn(&syn::TypeParam) -> bool,
) -> syn::Generics {
    let mut cloned_generics = generics.clone();

    for param in &mut cloned_generics.params {
        if let syn::GenericParam::Type(type_param) = param {
            if pred(type_param) {
                type_param.bounds.push(ty.clone());
            }
        }
    }

    cloned_generics
}

fn extract_phantom_data_ty(field: &syn::Field) -> Option<syn::Ident> {
    if let syn::Type::Path(syn::TypePath { ref path, .. }) = field.ty {
        let first_segment = &path.segments[0];

        if first_segment.ident == "PhantomData" {
            if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                ref args,
                ..
            }) = first_segment.arguments
            {
                if let syn::GenericArgument::Type(syn::Type::Path(syn::TypePath {
                    ref path, ..
                })) = args[0]
                {
                    return Some(path.segments[0].ident.clone());
                }
            }
        }
    }

    None
}

fn generate_debug_impl(
    ast: &syn::DeriveInput,
) -> std::result::Result<proc_macro2::TokenStream, syn::Error> {
    let struct_name = &ast.ident;
    let struct_name_string = struct_name.to_string();

    if let syn::Data::Struct(syn::DataStruct { ref fields, .. }) = ast.data {
        let non_debug_ty = fields
            .iter()
            .map(|field| extract_phantom_data_ty(field))
            .filter(|x| x.is_some())
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();
        let non_debug_pred = |x: &syn::TypeParam| {
            !non_debug_ty
                .iter()
                .any(|val| val.to_string() == x.ident.to_string())
        };
        let generics =
            add_trait_bound(&ast.generics, parse_quote!(std::fmt::Debug), non_debug_pred);
        let (impl_generics, ty_generics, where_clauses) = generics.split_for_impl();
        println!("{:?}", non_debug_ty);

        let field_combine = fields.iter().map(|field| {
            let ident = &field.ident;
            let name = ident.as_ref().unwrap().to_string();
            match has_debug_attr(field) {
                Some((true, debug_fmt)) => {
                    quote! {
                        .field(#name, &format_args!(#debug_fmt, &self.#ident))
                    }
                }
                _ => quote! {
                    .field(#name, &self.#ident)
                },
            }
        });

        return Ok(quote! {
            impl #impl_generics std::fmt::Debug for #struct_name #ty_generics #where_clauses {
                fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                    fmt.debug_struct(#struct_name_string)
                      #(#field_combine)*
                       .finish()
                }
            }
        });
    }

    Err(syn::Error::new_spanned(&struct_name, "Unknown data fields"))
}

fn has_debug_attr(field: &syn::Field) -> Option<(bool, String)> {
    for attr in &field.attrs {
        assert_eq!(1, attr.path.segments.len());

        if let Ok(syn::Meta::NameValue(nv)) = attr.parse_meta() {
            assert_eq!(1, nv.path.segments.len());

            if nv.path.segments.first().unwrap().ident == "debug" {
                if let syn::Lit::Str(lstr) = nv.lit {
                    return Some((true, lstr.value()));
                }
            }
        }
    }

    None
}
