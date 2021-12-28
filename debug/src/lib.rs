use itertools::{process_results, Itertools};
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Attribute, Data, DeriveInput, Fields, Lit,
    LitStr, Meta, Path, PathArguments, WhereClause, WherePredicate,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    derive_impl(parse_macro_input!(input as DeriveInput))
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
fn derive_impl(mut input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let input_span = input.span();
    let struct_ident = &input.ident;
    let generics = &mut input.generics;
    let type_params = generics
        .type_params()
        .cloned()
        .collect_vec()
        .into_iter()
        .map(|x| x.ident);
    let strct = match &input.data {
        Data::Struct(s) => s,
        Data::Enum(_) => {
            return Err(syn::Error::new(
                input_span,
                "Expected a struct, found an enum",
            ))
        }
        Data::Union(_) => {
            return Err(syn::Error::new(
                input_span,
                "Expected a struct, found a union",
            ))
        }
    };
    let fields = match &strct.fields {
        Fields::Named(f) => &f.named,
        Fields::Unnamed(_) => {
            return Err(syn::Error::new(
                input_span,
                "Expeted a named struct, found an unnamed struct",
            ))
        }
        Fields::Unit => {
            return Err(syn::Error::new(
                input_span,
                "Expeted a named struct, found a unit struct",
            ))
        }
    };
    let vec = fields.iter().map(|field| {
        let ident = field.ident.as_ref().unwrap();
        let ty = &field.ty;
        let debug_attr = extract_debug(&field.attrs)?;
        let value = match &debug_attr {
            Some(fmt) => quote!(&format_args!(#fmt, &self.#ident)),
            None => quote!(&self.#ident),
        };
        let quote = quote!( .field(stringify!(#ident), #value) );
        let constraint = match &debug_attr {
            Some(_) => None,
            None => Option::<WherePredicate>::Some(parse_quote!(#ty: ::std::fmt::Debug)),
        };
        Ok((quote, constraint))
    });
    let (builders, constraints): (Vec<_>, Vec<_>) =
        process_results::<_, _, _, syn::Error, _>(vec, |x| x.unzip())?;
    let mut constraints = constraints.into_iter().flatten().peekable();
    let where_clause = constraints.peek().is_some().then(|| {
        // We deprive of where clause, because it will not be serialized anyway
        let mut where_clause = generics
            .where_clause
            .take()
            .unwrap_or_else(|| -> WhereClause { parse_quote!(where) });
        for c in constraints {
            where_clause.predicates.push(c);
        }
        where_clause
    });
    let where_clause = where_clause.into_iter();
    Ok(quote! {
        impl <#(#type_params)*> ::std::fmt::Debug for #struct_ident #generics #(#where_clause)* {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                f.debug_struct(stringify!(#struct_ident))
                    #(#builders)*
                    .finish()
            }
        }
    })
}

fn extract_debug(attrs: &[Attribute]) -> syn::Result<Option<LitStr>> {
    let mut attrs = attrs.iter().filter(|a| path_is_just(&a.path, "debug"));
    let attr = match attrs.next() {
        Some(a) => a,
        None => return Ok(None),
    };
    if let Some(attr) = attrs.next() {
        return Err(syn::Error::new(
            attr.span(),
            r#"Too many `debug = ".."` attributes"#,
        ));
    }
    let value = match attr.parse_meta() {
        Ok(Meta::NameValue(v)) => v,
        _ => {
            return Err(syn::Error::new(attr.span(), r#"Expected `debug = ".."`"#));
        }
    };
    match value.lit {
        Lit::Str(s) => Ok(Some(s)),
        _ => Err(syn::Error::new(
            value.lit.span(),
            r#"Expected string literal"#,
        )),
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
