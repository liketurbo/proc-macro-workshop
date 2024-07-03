use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Data, DeriveInput, Fields, GenericArgument,
    PathArguments, Type, TypePath,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let builder_name = format_ident!("{}Builder", name);

    let fields = if let Data::Struct(data_struct) = input.data {
        if let Fields::Named(fields_named) = data_struct.fields {
            fields_named.named
        } else {
            unimplemented!("handles only named fields");
        }
    } else {
        unimplemented!("handles only struct data");
    };

    fn is_option(ty: &Type) -> bool {
        if let Type::Path(TypePath { path, .. }) = ty {
            path.segments.len() == 1 && path.segments[0].ident == "Option"
        } else {
            false
        }
    }

    fn extract_option_inner_ty(ty: &Type) -> Option<&Type> {
        if let Type::Path(TypePath { path, .. }) = ty {
            if path.segments.len() == 1 && path.segments[0].ident == "Option" {
                if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    args, ..
                }) = &path.segments[0].arguments
                {
                    if let Some(GenericArgument::Type(inner_ty)) = args.first() {
                        return Some(inner_ty);
                    }
                }
            }
        }
        None
    }

    let builder_fields = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        if is_option(ty) {
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
        if is_option(ty) {
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
        quote! {
            #name: None
        }
    });

    let setters = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        if let Some(inner_ty) = extract_option_inner_ty(ty) {
            quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
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
