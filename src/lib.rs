extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate quote;
extern crate syn;

use std::iter;

use proc_macro2::{Literal, Span, TokenStream};
use syn::DeriveInput;
use syn::{
    AngleBracketedGenericArguments, AttrStyle, Attribute, Data, DataEnum, DataStruct, Fields,
    GenericArgument, Ident, Lit, Meta, NestedMeta, Path, PathArguments, Type, TypePath, Visibility,
};

#[proc_macro_derive(AutoAccessor, attributes(access))]
pub fn my_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impl_auto_accessor(
        syn::parse(input).expect("Failed to parse input to `#[derive(AutoAccessor)]`"),
    )
    .into()
}

fn impl_auto_accessor(input: DeriveInput) -> TokenStream {
    match input.data {
        Data::Struct(ref s) => impl_struct_auto_accessor(&input, s),
        Data::Enum(ref e) => impl_enum_auto_accessor(&input, e),
        _ => panic!("AutoAccessor isn't available for non-Enum/Struct types"),
    }
}

fn impl_struct_auto_accessor(input: &DeriveInput, data: &DataStruct) -> TokenStream {
    let DeriveInput {
        ident: ref name,
        vis: ref struct_vis,
        attrs: ref struct_attrs,
        ..
    } = input;

    let is_struct = match data.fields {
        Fields::Named(_) => true,
        _ => false,
    };

    if !is_struct {
        panic!("AutoAccessor only works on structures with named fields");
    }

    let inherited_vis: Option<Visibility> =
        parse_visibility(struct_attrs, struct_vis.clone(), None);

    let accessors = data.fields.iter().filter_map(|field| {
        let ident = field.ident.as_ref().unwrap();

        if ident.to_string().starts_with('_') {
            return None;
        }

        let ty = &field.ty;

        let mut clonable = false;
        let mut copyable = false;

        let vis: Option<Visibility> = parse_visibility(
            &field.attrs,
            struct_vis.clone(),
            inherited_vis.clone().or_else(|| Some(field.vis.clone())),
        );

        let mut docs = Vec::new();

        for attr in &field.attrs {
            if attr.style != AttrStyle::Outer {
                continue;
            }

            match attr.parse_meta().ok() {
                Some(Meta::List(ref meta_list)) if meta_list.ident == "access" => {
                    for meta in &meta_list.nested {
                        match *meta {
                            NestedMeta::Meta(Meta::Word(ref ident)) => {
                                if ident == "clone" {
                                    clonable = true;
                                } else if ident == "copy" {
                                    copyable = true;
                                } else if ident == "ignore" {
                                    return None;
                                }
                            }
                            _ => continue,
                        }
                    }
                }
                Some(Meta::NameValue(ref meta)) if meta.ident == "doc" => {
                    if !docs.contains(&attr) {
                        docs.push(attr);
                    }
                }
                _ => {}
            }
        }

        let flattened_type = flatten(ty);

        copyable = copyable
            || flattened_type
                .as_ref()
                .map(is_trivially_copyable)
                .unwrap_or(false)
            || is_trivially_copyable(ty);

        let ty = match (copyable || clonable, &flattened_type) {
            (true, Some(ty)) => quote!(::std::option::Option<#ty>),
            (false, Some(ty)) => quote!(::std::option::Option<&#ty>),
            (true, None) => quote!(#ty),
            (false, None) => quote!(&#ty),
        };

        let body = match (copyable, clonable, &flattened_type) {
            (true, _, _) => quote!(self.#ident),
            (_, true, _) => quote!(self.#ident.clone()),
            (false, false, Some(_)) => quote!(self.#ident.as_ref()),
            (false, false, None) => quote!(&self.#ident),
        };

        Some(quote! {
            #(#docs)*
            #[inline]
            #vis fn #ident(&self) -> #ty { #body }
        })
    });

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    quote! {
        impl #impl_generics #name #ty_generics #where_clause {
            #(#accessors)*
        }
    }
}

fn impl_enum_auto_accessor(input: &DeriveInput, data: &DataEnum) -> TokenStream {
    let DeriveInput {
        ident: ref name,
        vis: ref enum_vis,
        attrs: ref enum_attrs,
        ..
    } = input;

    let num_variants = data.variants.len();

    let inherited_vis: Option<Visibility> =
        parse_visibility(enum_attrs, enum_vis.clone(), Some(enum_vis.clone()));

    let mut done: Vec<String> = Vec::new();

    let accessors = data.variants.iter().flat_map(|variant| {
        let is_struct = match variant.fields {
            Fields::Named(_) => true,
            _ => false,
        };

        if !is_struct {
            panic!("AutoAccessor only works on struct enum variants with named fields");
        }

        let mut field_accessors = Vec::new();

        'skip_field: for field in variant.fields.iter() {
            let ident: Ident = field.ident.clone().unwrap();

            let ident_str = ident.to_string();

            if ident_str.starts_with('_') {
                continue 'skip_field;
            }

            if !done.contains(&ident_str) {
                let ty = &field.ty;

                let mut clonable = false;
                let mut copyable = false;

                let mut presence = 0;

                let mut docs = Vec::new();

                let mut vis: Option<Visibility> = inherited_vis.clone();

                for other_variant in data.variants.iter() {
                    for other_field in other_variant.fields.iter() {
                        if other_field.ident == field.ident {
                            if other_field.ty != *ty {
                                panic!("Same-name fields must have the same type!");
                            }

                            presence += 1;

                            vis =
                                parse_visibility(&other_field.attrs, enum_vis.clone(), vis.clone());

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
                                                    } else if ident == "ignore" {
                                                        done.push(ident_str);

                                                        continue 'skip_field;
                                                    }
                                                }
                                                _ => continue,
                                            }
                                        }
                                    }
                                    Some(Meta::NameValue(ref meta)) if meta.ident == "doc" => {
                                        if !docs.contains(&attr) {
                                            docs.push(attr);
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

                let ty = match (copyable || clonable, is_in_all, &flattened_type) {
                    (true, _, Some(ty)) => quote!(::std::option::Option<#ty>),
                    (false, _, Some(ty)) => quote!(::std::option::Option<&#ty>),
                    (true, true, None) => quote!(#ty),
                    (false, true, None) => quote!(&#ty),
                    (true, false, None) => quote!(::std::option::Option<#ty>),
                    (false, false, None) => quote!(::std::option::Option<&#ty>),
                };

                let body = data
                    .variants
                    .iter()
                    .filter_map(|variant| {
                        let v = &variant.ident;

                        let has_binding = variant
                            .fields
                            .iter()
                            .any(|field| field.ident.as_ref() == Some(&ident));

                        if !has_binding {
                            None
                        } else {
                            let borrow = if !copyable { quote!(ref) } else { quote!() };

                            let body = match (copyable, clonable, is_in_all, &flattened_type) {
                                // variant 1, 2 copy
                                (true, _, _, Some(_)) => quote!(#ident),
                                // variant 1, 2 clone
                                (_, true, _, Some(_)) => quote!(#ident.as_ref().cloned()),
                                // variant 3, 4
                                (false, false, _, Some(_)) => quote!(#ident.as_ref()),
                                // variant 5 copy
                                (true, _, true, None) => quote!(#ident),
                                // variant 5 clone
                                (_, true, true, None) => quote!(#ident.clone()),
                                // variant 6 copy
                                (true, _, false, None) => quote!(Some(#ident)),
                                // variant 6 clone
                                (_, true, false, None) => quote!(Some(#ident.clone())),
                                // variant 7
                                (_, _, true, None) => quote!(#ident),
                                // variant 8
                                (_, _, false, None) => quote!(Some(#ident)),
                            };

                            Some(quote!(#name::#v {#borrow #ident, ..} => {#body},))
                        }
                    })
                    // if there were fewer bindings than variants,
                    // the return type will be optional, so we can
                    // automatically insert the fallthrough.
                    .chain(iter::once(quote!(_ => { None },)))
                    .take(num_variants);

                field_accessors.push(quote! {
                    #(#docs)*
                    #[inline]
                    #vis fn #ident(&self) -> #ty {
                        match *self { #(#body)* }
                    }
                });
            }
        }

        field_accessors.into_iter()
    });

    let is_variants = data.variants.iter().map(|variant| {
        let v = &variant.ident;

        let method = Ident::new(
            &format!("is_{}", v.to_string().to_lowercase()),
            Span::call_site(),
        );

        let doc = Lit::new(Literal::string(&format!(
            "Returns true if the enum is variant [`{0}`](#variant.{0})",
            v
        )));

        let mut variant_docs = variant.attrs.iter().filter(|attr| {
            if attr.style != AttrStyle::Outer {
                return false;
            }

            match attr.parse_meta().ok() {
                Some(Meta::NameValue(ref meta)) if meta.ident == "doc" => true,
                _ => false,
            }
        });

        let first_variant_doc = variant_docs.next();

        // if there is a variant doc comment, insert an extra newline
        let extra_doc_space = first_variant_doc.as_ref().map(|_| quote!(#[doc = ""]));

        quote! {
            #[doc = #doc]
            #extra_doc_space
            #first_variant_doc
            #(#variant_docs)*
            #[inline]
            #enum_vis fn #method(&self) -> bool {
                match *self {
                    #name::#v { .. } => { true },
                    _ => { false },
                }
            }
        }
    });

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    quote! {
        impl #impl_generics #name #ty_generics #where_clause {
            #(#is_variants)*

            #(#accessors)*
        }
    }
}

fn parse_visibility(
    attrs: &[Attribute],
    parent_vis: Visibility,
    initial_vis: Option<Visibility>,
) -> Option<Visibility> {
    let mut vis = initial_vis;

    for attr in attrs {
        if attr.style != AttrStyle::Outer {
            continue;
        }

        match attr.parse_meta().ok() {
            Some(Meta::List(ref meta_list)) if meta_list.ident == "access" => {
                for meta in &meta_list.nested {
                    match *meta {
                        NestedMeta::Meta(Meta::Word(ref ident)) => {
                            if ident == "hidden" || ident == "private" {
                                vis = None;
                            } else if ident == "public" {
                                vis = Some(Visibility::Public(syn::VisPublic {
                                    pub_token: syn::token::Pub {
                                        span: Span::call_site(),
                                    },
                                }));
                            }
                        }
                        NestedMeta::Meta(Meta::NameValue(ref meta)) if meta.ident == "vis" => {
                            if let Lit::Str(ref s) = meta.lit {
                                let value = s.value();

                                vis = match value.as_str() {
                                    "inherit" => Some(parent_vis.clone()),
                                    "private" => None,
                                    _ => Some(syn::parse_str(&value).unwrap()),
                                };
                            }
                        }
                        _ => continue,
                    }
                }
            }
            _ => {}
        }
    }

    vis
}

fn flatten(ty: &Type) -> Option<Type> {
    match ty {
        Type::Path(TypePath { ref path, .. }) => {
            let fully_qualified = syn::parse_str::<Path>("::std::option::Option").unwrap();

            let num_same_idents = fully_qualified
                .segments
                .iter()
                .zip(path.segments.iter())
                .filter(|(f, t)| f.ident == t.ident)
                .count();

            let outer_type = if num_same_idents == path.segments.len() {
                path.segments.last().unwrap()
            } else {
                path.segments.first().unwrap()
            };

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
