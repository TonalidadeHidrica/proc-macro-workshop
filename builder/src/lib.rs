use itertools::Itertools;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse_macro_input, Data, DeriveInput, Fields, GenericArgument, Ident, PathArguments, Type,
};

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
    let option_types = fields.named.iter().map(|x| as_option(&x.ty)).collect_vec();
    let constructor = fields.named.iter().zip(&option_types).map(|(x, opt)| {
        let ident = &x.ident;
        match opt {
            None => quote!( #ident: None ),
            Some(_) => quote!( #ident: Some(None) ),
        }
    });
    let fields_option = fields.named.iter().map(|x| {
        let ident = &x.ident;
        let ty = &x.ty;
        quote!( #ident: Option<#ty> )
    });
    let setters = fields.named.iter().zip(&option_types).map(|(x, opt)| {
        let ident = &x.ident;
        let ty = &x.ty;
        if let Some(ty) = opt {
            quote! {
                fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = Some(Some(#ident));
                    self
                }
            }
        } else {
            quote! {
                fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
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

        #[derive(Debug)]
        struct #builder_name {
            #(#fields_option),*
        }

        impl #builder_name {
            #(#setters)*

            fn build(&mut self) -> ::anyhow::Result<#name> {
                dbg!(&self);
                (|| -> Option<_> {
                    Some(#name {
                        #(#build_fields),*
                    })
                })().ok_or_else(|| ::anyhow::anyhow!("Error!"))
            }
        }
    };

    // TokenStream::new()
    ret.into()
}

fn as_option(ty: &Type) -> Option<&Type> {
    let path = match ty {
        Type::Path(path) => path,
        _ => return None,
    };
    if path.qself.is_some() {
        return None;
    }
    let path = &path.path.segments;
    if path.len() != 1 {
        return None;
    }
    let ty = path.last()?;
    if ty.ident != "Option" {
        return None;
    }
    let args = match &ty.arguments {
        PathArguments::AngleBracketed(args) => &args.args,
        _ => return None,
    };
    if args.len() != 1 {
        return None;
    };
    match args.first()? {
        GenericArgument::Type(ty) => Some(ty),
        _ => None,
    }
}
