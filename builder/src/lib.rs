use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, punctuated::Punctuated, AngleBracketedGenericArguments, Attribute, Data,
    DeriveInput, Expr, ExprLit, Fields, GenericArgument, Lit, Meta, MetaNameValue, PathArguments,
    Token, Type, TypePath,
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
        if is_option(ty) || find_attr_each(&field.attrs).is_some() {
            quote! {
                #name: #ty
            }
        } else {
            quote! {
                #name: Option<#ty>
            }
        }
    });

    let build_fields = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        if find_attr_each(&field.attrs).is_some() {
            quote! {
                #name: std::mem::take(&mut self.#name)
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
        if find_attr_each(&field.attrs).is_some() {
            quote! { 
                #name: Vec::new() 
            }
        } else {
            quote! { 
                #name: None 
            }
        }
    });

    let setters = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;

        if let Some(each_name) = find_attr_each(&field.attrs) {
            let each_ident = format_ident!("{each_name}");
            let vec_inner_ty = extract_inner_ty(ty, "Vec").unwrap();
            quote! {
                pub fn #each_ident(&mut self, #each_ident: #vec_inner_ty) -> &mut Self {
                    self.#name.push(#each_ident);
                    self
                }
            }
        } else {
            let setter_ty = extract_inner_ty(ty, "Option").unwrap_or(ty);
            quote! {
                pub fn #name(&mut self, #name: #setter_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        }
    });

    let tokens = quote! {
        struct #builder_name {
            #(#builder_fields),*
        }

        impl #builder_name {
            #(#setters)*

            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
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

fn find_attr_each(attrs: &[Attribute]) -> Option<String> {
    for attr in attrs {
        if attr.path().is_ident("builder") {
            let nested = attr
                .parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)
                .expect("attributes parsing failed");
            for meta in nested {
                if let Meta::NameValue(MetaNameValue { path, value, .. }) = meta {
                    // #[builder(each = "arg")]
                    if path.is_ident("each") {
                        if let Expr::Lit(ExprLit { lit, .. }) = value {
                            if let Lit::Str(lit_str) = lit {
                                return Some(lit_str.value());
                            }
                        }
                    }
                }
            }
        }
    }
    None
}
