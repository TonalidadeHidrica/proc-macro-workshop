use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields, Ident};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let builder_name = Ident::new(&format!("{}Builder", name), Span::call_site());

    let str = match input.data {
        Data::Struct(s) => s,
        Data::Enum(_) | Data::Union(_) => panic!("Enums and unions are not supported"),
    };
    let fields = match str.fields {
        Fields::Named(f) => f,
        Fields::Unnamed(_) | Fields::Unit => panic!("Tuple structs and units are not supported"),
    };
    let constructor = fields.named.iter().map(|x| {
        let ident = &x.ident;
        quote!( #ident: None )
    });
    let fields_option = fields.named.iter().map(|x| {
        let ident = &x.ident;
        let ty = &x.ty;
        quote!( #ident: Option<#ty> )
    });
    let setters = fields.named.iter().map(|x| {
        let ident = &x.ident;
        let ty = &x.ty;
        quote! {
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        }
    });
    let build_fields = fields.named.iter().map(|x| {
        let ident = &x.ident;
        quote! {
            #ident: self.#ident.as_ref()?.clone()  // :thinking_face:
        }
    });

    let ret = quote! {
        impl #name {
            fn builder() -> #builder_name {
                #builder_name {
                    #(#constructor),*
                }
            }
        }

        struct #builder_name {
            #(#fields_option),*
        }

        impl #builder_name {
            #(#setters)*

            fn build(&mut self) -> anyhow::Result<#name> {
                (|| -> Option<_> {
                    Some(#name {
                        #(#build_fields),*
                    })
                })().ok_or(|| anyhow!("Error!"))
            }
        }
    };

    // TokenStream::new()
    ret.into()
}
