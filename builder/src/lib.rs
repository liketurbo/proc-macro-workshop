use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput, Fields, Type};

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

    let builder_fields = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        if let Type::Path(type_path) = ty {
            if type_path.path.segments.len() == 1 && type_path.path.segments[0].ident == "Option" {
                quote! {
                    #name: #ty
                }
            } else {
                quote! {
                    #name: Option<#ty>
                }
            }
        } else {
            quote! {
                #name: Option<#ty>
            }
        }
    });

    let build_fields = fields.iter().map(|field| {
        let name = &field.ident;
        quote! {
            #name: None
        }
    });

    let setters = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        quote! {
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });

    let tokens = quote! {
        struct #builder_name {
            #(#builder_fields),*
        }

        impl #builder_name {
            #(#setters)*
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#build_fields),*
                }
            }
        }
    };

    tokens.into()
}
