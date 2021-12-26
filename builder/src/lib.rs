use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse_macro_input, parse_str, spanned::Spanned, Data, DeriveInput, Field, Fields,
    GenericArgument, Ident, Lit, Meta, NestedMeta, Path, PathArguments, Type,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    derive_impl(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
fn derive_impl(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
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
    let option_types = fields.named.iter().map(|x| {
        syn::Result::Ok(match as_surrounding_type(&x.ty, "Option") {
            Some(ty) => ClassifyType::Option(ty),
            None => match check_attr(x) {
                Some(res) => res?,
                None => ClassifyType::Other(&x.ty),
            },
        })
    });
    let fields = fields
        .named
        .iter()
        .zip(option_types)
        .map(|(x, y)| syn::Result::Ok((x, y?)))
        .collect::<Result<Vec<_>, _>>()?;
    let fields_option = fields.iter().map(|(x, opt)| {
        let ident = &x.ident;
        let ty = &x.ty;
        match opt {
            ClassifyType::Vec(ty, _) => quote!( #ident: ::std::vec::Vec<#ty> ),
            _ => quote!( #ident: ::std::option::Option<#ty> ),
        }
    });
    let constructor = fields.iter().map(|(x, opt)| {
        let ident = &x.ident;
        match opt {
            ClassifyType::Option(_) => quote!( #ident: ::std::option::Option::Some(::std::option::Option::None) ),
            ClassifyType::Vec(..) => quote!( #ident: ::std::vec::Vec::new() ),
            ClassifyType::Other(_) => quote!( #ident: ::std::option::Option::None ),
        }
    });
    let setters = fields.iter().map(|(x, opt)| {
        let ident = &x.ident;
        match opt {
            ClassifyType::Option(ty) => {
                quote! {
                    fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = ::std::option::Option::Some(::std::option::Option::Some(#ident));
                        self
                    }
                }
            }
            ClassifyType::Other(ty) => {
                quote! {
                    fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = ::std::option::Option::Some(#ident);
                        self
                    }
                }
            }
            ClassifyType::Vec(ty, name) => {
                quote! {
                    fn #name(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident.push(#ident);
                        self
                    }
                }
            }
        }
    });
    let build_fields = fields.iter().map(|(x, opt)| {
        let ident = &x.ident;
        match opt {
            ClassifyType::Vec(..) => quote! {
                #ident: self.#ident.clone()
            },
            _ => quote! {
                #ident: self.#ident.as_ref()?.clone()  // :thinking_face:
            },
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
                (|| -> ::std::option::Option<_> {
                    ::std::option::Option::Some(#name {
                        #(#build_fields),*
                    })
                })().ok_or_else(|| ::anyhow::anyhow!("Error!"))
            }
        }
    };

    // Ok(ret.into())
    Ok(ret)
}

enum ClassifyType<'a> {
    Option(&'a Type),
    Vec(&'a Type, Ident),
    Other(&'a Type),
}

fn as_surrounding_type<'a, 'b>(ty: &'a Type, name: &'b str) -> Option<&'a Type> {
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
    if ty.ident != name {
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

fn check_attr(field: &Field) -> Option<syn::Result<ClassifyType>> {
    let attr = field
        .attrs
        .iter()
        .filter(|x| path_is_just(&x.path, "builder"))
        .last()?;
    let val = match attr.parse_meta().ok()? {
        Meta::List(v) => v.nested,
        _ => return None,
    };
    if val.len() != 1 {
        return None;
    }
    let val = match val.first()? {
        NestedMeta::Meta(Meta::NameValue(v)) if path_is_just(&v.path, "each") => v,
        _ => {
            return Some(Err(syn::Error::new(
                val.span(),
                r#"expected `builder(each = "...")`"#,
            )))
        }
    };
    match &val.lit {
        Lit::Str(s) => {
            let ty = as_surrounding_type(&field.ty, "Vec")?;
            let ident = parse_str::<Ident>(&s.value()).ok()?;
            Some(Ok(ClassifyType::Vec(ty, ident)))
        }
        _ => None,
    }
}

fn path_is_just(p: &Path, s: &str) -> bool {
    if p.leading_colon.is_some() {
        return false;
    }
    let p = &p.segments;
    if p.len() != 1 {
        return false;
    }
    let p = p.first().unwrap();
    match p.arguments {
        PathArguments::None => {}
        _ => return false,
    };
    p.ident == s
}
