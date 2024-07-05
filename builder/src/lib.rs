use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, punctuated::Punctuated, spanned::Spanned, AngleBracketedGenericArguments,
    Attribute, Data, DeriveInput, Expr, ExprLit, Fields, GenericArgument, Lit, Meta, MetaNameValue,
    PathArguments, Token, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let builder_name = format_ident!("{}Builder", name);
    let fields = extract_fields(&input);

    let builder_fields = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        if is_option(ty) || find_attr(&field.attrs, "each").is_some() {
            quote! {
                #name: #ty
            }
        } else {
            quote! {
                #name: ::core::option::Option<#ty>
            }
        }
    });

    let build_fields = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        if find_attr(&field.attrs, "each").is_some() {
            quote! {
                #name: ::core::mem::take(&mut self.#name)
            }
        } else if is_option(ty) {
            quote! {
                #name: self.#name.take()
            }
        } else {
            let field_name = name.as_ref().unwrap().to_string();
            quote! {
                #name: self.#name.take().ok_or_else(|| format!("field '{}' is not set", #field_name))?
            }
        }
    });

    let build_fields_none = fields.iter().map(|field| {
        let name = &field.ident;
        if find_attr(&field.attrs, "each").is_some() {
            quote! {
                #name: ::std::vec::Vec::new()
            }
        } else {
            quote! {
                #name: ::core::option::Option::None
            }
        }
    });

    let setters: Result<Vec<_>, syn::Error> = fields
        .iter()
        .map(|field| {
            let name = &field.ident;
            let ty = &field.ty;

            if let Some(attr_info) = find_attr(&field.attrs, "each") {
                match attr_info {
                    AttributeInfo::Correct { value } => {
                        let each_ident = format_ident!("{value}");
                        let vec_inner_ty = extract_inner_ty(ty, "Vec").unwrap();
                        Ok(quote! {
                            pub fn #each_ident(&mut self, #each_ident: #vec_inner_ty) -> &mut Self {
                                self.#name.push(#each_ident);
                                self
                            }
                        })
                    }
                    AttributeInfo::Misspelled { span } => {
                        Err(syn::Error::new(span, "expected `builder(each = \"...\")`"))
                    }
                }
            } else {
                let setter_ty = extract_inner_ty(ty, "Option").unwrap_or(ty);
                Ok(quote! {
                    pub fn #name(&mut self, #name: #setter_ty) -> &mut Self {
                        self.#name = ::core::option::Option::Some(#name);
                        self
                    }
                })
            }
        })
        .collect();

    let setters = match setters {
        Ok(s) => s,
        Err(e) => return e.into_compile_error().into(),
    };

    let tokens = quote! {
        struct #builder_name {
            #(#builder_fields),*
        }

        impl #builder_name {
            #(#setters)*

            pub fn build(&mut self) -> ::core::result::Result<#name, ::std::boxed::Box<dyn ::std::error::Error>> {
                Ok(#name {
                    #(#build_fields),*
                })
            }
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#build_fields_none),*
                }
            }
        }
    };

    tokens.into()
}

fn extract_fields(input: &DeriveInput) -> Punctuated<syn::Field, Token![,]> {
    if let Data::Struct(data_struct) = &input.data {
        if let Fields::Named(fields_named) = &data_struct.fields {
            fields_named.named.clone()
        } else {
            unimplemented!("handles only named fields");
        }
    } else {
        unimplemented!("handles only struct data");
    }
}

fn is_option(ty: &Type) -> bool {
    if let Type::Path(TypePath { path, .. }) = ty {
        path.segments.len() == 1 && path.segments[0].ident == "Option"
    } else {
        false
    }
}

fn extract_inner_ty<'a>(ty: &'a Type, container: &str) -> Option<&'a Type> {
    if let Type::Path(TypePath { path, .. }) = ty {
        if path.segments.len() == 1 && path.segments[0].ident == container {
            if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =
                &path.segments[0].arguments
            {
                if let Some(GenericArgument::Type(inner_ty)) = args.first() {
                    return Some(inner_ty);
                }
            }
        }
    }
    None
}

const LEVENSHTEIN_THRESHOLD: usize = 1;

enum AttributeInfo {
    Correct { value: String },
    Misspelled { span: proc_macro2::Span },
}

fn find_attr<'a>(attrs: &'a [Attribute], attr_key: &str) -> Option<AttributeInfo> {
    for attr in attrs {
        let span = attr.meta.span();
        if attr.path().is_ident("builder") {
            let nested = attr
                .parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)
                .expect("attributes parsing failed");
            for meta in nested {
                if let Meta::NameValue(MetaNameValue { path, value, .. }) = meta {
                    if let Expr::Lit(ExprLit { lit, .. }) = value {
                        if let Lit::Str(lit_str) = lit {
                            let cur_attr_key = path.get_ident().unwrap().to_string();
                            let attr_value = lit_str.value();
                            let dist = levenshtein_distance(&cur_attr_key, attr_key);
                            if dist == 0 {
                                return Some(AttributeInfo::Correct { value: attr_value });
                            } else if dist <= LEVENSHTEIN_THRESHOLD {
                                return Some(AttributeInfo::Misspelled { span });
                            } else {
                                return None;
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let len1 = s1.len();
    let len2 = s2.len();
    let mut matrix = vec![vec![0; len2 + 1]; len1 + 1];

    for i in 0..=len1 {
        matrix[i][0] = i;
    }

    for j in 0..=len2 {
        matrix[0][j] = j;
    }

    for i in 1..=len1 {
        for j in 1..=len2 {
            let cost = if s1.chars().nth(i - 1) == s2.chars().nth(j - 1) {
                0
            } else {
                1
            };

            matrix[i][j] = std::cmp::min(
                std::cmp::min(matrix[i - 1][j] + 1, matrix[i][j - 1] + 1),
                matrix[i - 1][j - 1] + cost,
            );
        }
    }

    matrix[len1][len2]
}
