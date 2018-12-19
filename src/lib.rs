extern crate quote;
extern crate syn;
#[macro_use]
extern crate synstructure;
extern crate proc_macro2;

use proc_macro2::{Literal, Span, TokenStream};
use syn::spanned::Spanned;
use syn::{
    AngleBracketedGenericArguments, AttrStyle, Fields, GenericArgument, Ident, Lit, Meta,
    NestedMeta, PathArguments, Type, TypePath,
};
use synstructure::{BindStyle, Structure};

decl_derive!([AutoAccessor, attributes(access)] => impl_auto_accessor);

fn impl_auto_accessor(mut s: Structure) -> TokenStream {
    s.binding_name(|bi, i| {
        bi.ident
            .clone()
            .unwrap_or_else(|| Ident::new(&format!("binding{}", i), bi.ident.span()))
    });

    let name = &s.ast().ident;

    let (impl_generics, ty_generics, where_clause) = s.ast().generics.split_for_impl();

    let num_variants = s.variants().len();

    let mut done: Vec<String> = Vec::new();

    // For every enum variant
    let accessors = s.variants().iter().flat_map(|variant_info| {
        let fields = variant_info.ast().fields;

        let is_struct = match fields {
            Fields::Named(_) => true,
            _ => false,
        };

        if !is_struct {
            panic!("Only works on struct enums");
        }

        let mut field_accessors = Vec::new();

        for field in fields.iter() {
            let ident: Ident = field.ident.clone().unwrap();

            let ident_str = ident.to_string();

            if done.contains(&ident_str) {
                continue;
            } else {
                let ty = &field.ty;

                let mut clonable = false;
                let mut copyable = false;

                let mut presence = 0;

                let mut docs = Vec::new();

                // for every variant
                for other_variant_info in s.variants().iter() {
                    // for every field of every variant
                    for other_field in other_variant_info.ast().fields.iter() {
                        // if current field found,
                        // check for same-type and increase presence
                        if other_field.ident == field.ident {
                            if other_field.ty != *ty {
                                panic!("Same-name fields must have the same type!");
                            }

                            presence += 1;

                            for attr in &other_field.attrs {
                                if attr.style != AttrStyle::Outer {
                                    continue;
                                }

                                match attr.parse_meta().ok() {
                                    Some(Meta::List(ref meta_list))
                                        if meta_list.ident == "access" =>
                                    {
                                        for meta in &meta_list.nested {
                                            match *meta {
                                                NestedMeta::Meta(Meta::Word(ref ident)) => {
                                                    if ident == "clone" {
                                                        clonable = true;
                                                    } else if ident == "copy" {
                                                        copyable = true;
                                                    }
                                                }
                                                _ => continue,
                                            }
                                        }
                                    }
                                    Some(Meta::NameValue(ref meta)) if meta.ident == "doc" => {
                                        if !docs.contains(&attr) {
                                            docs.push(attr);
                                        } else {
                                            println!("{:?}", attr);
                                        }
                                    }
                                    _ => {}
                                }
                            }
                        }
                    }
                }

                done.push(ident_str);

                docs.dedup();

                let flattened_type = flatten(ty);

                copyable = copyable
                    || flattened_type
                        .as_ref()
                        .map(is_trivially_copyable)
                        .unwrap_or(false)
                    || is_trivially_copyable(ty);

                let is_in_all = presence == num_variants;

                // clone structure for use on this field
                let mut s = s.clone();

                // Don't bother binding other fields
                s.filter(|bi| bi.ast().ident.as_ref() == Some(&ident));

                /*
                 * Possible combinations:
                 *  If the type was flattened (was an Option):
                 *      If the field is copyable or clonable:
                 *          If the field is present in all:
                 * 1            Return Option<T>
                 *          Else:
                 * 2            Return Option<T>
                 *      Else:
                 *          If the field is present in all:
                 * 3            Return Option<&T>
                 *          Else:
                 * 4            Return Option<&T>
                 *  Else:
                 *      If the field is copyable or clonable:
                 *          If the field is present in all:
                 * 5            Return T
                 *          Else:
                 * 6            Return Option<T>
                 *      Else:
                 *          If the field is present in all:
                 * 7            Return &T
                 *          Else:
                 * 8            Return Option<&T>
                 */

                if !is_in_all {
                    s.filter_variants(|v| {
                        v.bindings()
                            .iter()
                            .any(|bi| bi.ast().ident.as_ref() == Some(&ident))
                    });
                }

                let (ty, body) = match (copyable, clonable, is_in_all, flattened_type) {
                    // variant 1, 2 copy
                    (true, _, _, Some(ty)) => (
                        quote!(Option<#ty>),
                        s.bind_with(|_| BindStyle::Move)
                            .fold(quote!(None), |_, bi| quote!(#bi)),
                    ),
                    // variant 1, 2 clone
                    (_, true, _, Some(ty)) => (
                        quote!(Option<#ty>),
                        s.bind_with(|_| BindStyle::Ref)
                            .fold(quote!(None), |_, bi| quote!(#bi.as_ref().cloned())),
                    ),
                    // variant 3, 4
                    (false, false, _, Some(ty)) => (
                        quote!(Option<&#ty>),
                        s.bind_with(|_| BindStyle::Ref)
                            .fold(quote!(None), |_, bi| quote!(#bi.as_ref())),
                    ),
                    // variant 5 copy
                    (true, _, true, None) => (
                        quote!(#ty),
                        s.bind_with(|_| BindStyle::Move).each(|bi| quote!(#bi)),
                    ),
                    // variant 5 clone
                    (_, true, true, None) => (
                        quote!(#ty),
                        s.bind_with(|_| BindStyle::Ref)
                            .each(|bi| quote!(#bi.clone())),
                    ),
                    // variant 6 copy
                    (true, _, false, None) => (
                        quote!(Option<#ty>),
                        s.bind_with(|_| BindStyle::Move)
                            .fold(quote!(None), |_, bi| quote!(Some(#bi))),
                    ),
                    // variant 6 clone
                    (_, true, false, None) => (
                        quote!(Option<#ty>),
                        s.bind_with(|_| BindStyle::Ref)
                            .fold(quote!(None), |_, bi| quote!(Some(#bi.clone()))),
                    ),
                    // variant 7
                    (_, _, true, None) => (
                        quote!(&#ty),
                        s.bind_with(|_| BindStyle::Ref).each(|bi| quote!(#bi)),
                    ),
                    // variant 8
                    (_, _, false, None) => (
                        quote!(Option<&#ty>),
                        s.bind_with(|_| BindStyle::Ref)
                            .fold(quote!(None), |_, bi| quote!(Some(#bi))),
                    ),
                };

                field_accessors.push(quote! {
                    #(#docs)*
                    pub fn #ident(&self) -> #ty {
                        match *self { #body }
                    }
                });

                field_accessors.push(quote!());
            }
        }

        field_accessors.into_iter()
    });

    let is_variant = s.variants().iter().map(|variant_info| {
        // clone structure for use on this variant
        let mut s = s.clone();

        // Don't bother binding any fields
        s.filter(|_| false);

        let ident_str = variant_info.ast().ident.to_string();

        let method = Ident::new(
            &format!("is_{}", ident_str.to_lowercase()),
            Span::call_site(),
        );

        let body = s.each_variant(|v| {
            if v.ast().ident == variant_info.ast().ident {
                quote!(true)
            } else {
                quote!(false)
            }
        });

        let doc = Lit::new(Literal::string(&format!(
            "Returns true if the enum is variant [`{0}`](#variant.{0})",
            ident_str
        )));

        quote! {
            #[doc = #doc]
            pub fn #method(&self) -> bool {
                match *self { #body }
            }
        }
    });

    quote! {
        #[allow(non_shorthand_field_patterns, unused_variables)]
        impl #impl_generics #name #ty_generics #where_clause {
            #(#is_variant)*

            #(#accessors)*
        }
    }
}

//fn reparse_lit_as_type(lit: Lit) -> Option<Type> {
//    match lit {
//        Lit::Str(ref s) => {
//            let s = s.value();
//
//            syn::parse_str(s.trim_matches('"')).ok()
//        }
//        _ => None,
//    }
//}

fn flatten(ty: &Type) -> Option<Type> {
    match ty {
        Type::Path(TypePath { ref path, .. }) => {
            let outer_type = path.segments.first().unwrap();

            if outer_type.value().ident == "Option" {
                match outer_type.value().arguments {
                    PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        ref args,
                        ..
                    }) => match args.first().unwrap().value() {
                        GenericArgument::Type(ref ty) => Some(ty.clone()),
                        _ => None,
                    },
                    _ => None,
                }
            } else {
                None
            }
        }
        _ => None,
    }
}

fn is_trivially_copyable(ty: &Type) -> bool {
    const PRIMITIVE_TYPES: &[&str] = &[
        "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "isize", "usize", "bool", "char",
        "f32", "f64",
    ];

    match ty {
        Type::Ptr(_) | Type::Reference(_) | Type::Never(_) => true,
        Type::Paren(ref ty) => is_trivially_copyable(&ty.elem),
        Type::Group(ref ty) => is_trivially_copyable(&ty.elem),
        Type::Path(TypePath { ref path, .. }) => {
            let segment = path.segments.first().unwrap();

            let ty = &segment.value().ident;

            PRIMITIVE_TYPES.iter().any(|prim| ty == prim)
        }
        _ => false,
    }
}
