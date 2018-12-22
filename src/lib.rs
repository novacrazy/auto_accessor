extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate quote;
extern crate syn;

use std::iter;

use proc_macro2::{Literal, Span, TokenStream};
use syn::DeriveInput;
use syn::{
    AngleBracketedGenericArguments, AttrStyle, Attribute, Data, DataEnum, DataStruct, Fields, GenericArgument, Ident,
    Lit, Meta, NestedMeta, Path, PathArguments, Type, TypePath, Variant, Visibility,
};

#[proc_macro_derive(AutoAccessor, attributes(access))]
pub fn my_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impl_auto_accessor(syn::parse(input).expect("Failed to parse input to `#[derive(AutoAccessor)]`")).into()
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

    let inherited_vis: Option<Visibility> = parse_visibility(struct_attrs, struct_vis.clone(), None);

    let mut struct_prefix = None;

    visit_nested_attrs(struct_attrs, |meta, _| match meta {
        NestedMeta::Meta(Meta::NameValue(ref meta)) if meta.ident == "prefix" => {
            if let Lit::Str(ref s) = meta.lit {
                struct_prefix = Some(s.value());
            }
        }
        _ => (),
    });

    let accessors = data.fields.iter().filter_map(|field| {
        let field_ident = field.ident.as_ref();

        let mut accessor_name = field_ident.cloned().unwrap().to_string();

        if accessor_name.starts_with('_') {
            return None;
        }

        let ty = &field.ty;

        let mut prefix = None;

        let mut clonable = false;
        let mut copyable = false;

        let vis: Option<Visibility> = parse_visibility(
            &field.attrs,
            struct_vis.clone(),
            inherited_vis.clone().or_else(|| Some(field.vis.clone())),
        );

        // collect docs
        let docs = iter_docs(&field.attrs);

        if let Visited::Halted = visit_nested_attrs(&field.attrs, |meta, _| match meta {
            NestedMeta::Meta(Meta::Word(ref ident)) => {
                if ident == "clone" {
                    clonable = true;
                } else if ident == "copy" {
                    copyable = true;
                } else if ident == "ignore" {
                    return Visit::Halt;
                }

                Visit::Continue
            }
            NestedMeta::Meta(Meta::NameValue(ref meta)) if meta.ident == "prefix" => {
                if let Lit::Str(ref s) = meta.lit {
                    prefix = Some(s.value());
                }

                Visit::Continue
            }
            NestedMeta::Meta(Meta::NameValue(ref meta)) if meta.ident == "rename" => {
                if let Lit::Str(ref s) = meta.lit {
                    accessor_name = s.value();
                }

                Visit::Continue
            }
            _ => Visit::Continue,
        }) {
            // Halted only if the field was set to ignore, to skip it
            return None;
        }

        let flattened_type = flatten(ty);

        copyable = copyable
            || flattened_type.as_ref().map(is_trivially_copyable).unwrap_or(false)
            || is_trivially_copyable(ty);

        let ty = match (copyable || clonable, &flattened_type) {
            (true, Some(ty)) => quote!(::std::option::Option<#ty>),
            (false, Some(ty)) => quote!(::std::option::Option<&#ty>),
            (true, None) => quote!(#ty),
            (false, None) => quote!(&#ty),
        };

        let body = match (copyable, clonable, &flattened_type) {
            (true, _, _) => quote!(self.#field_ident),
            (_, true, _) => quote!(self.#field_ident.clone()),
            (false, false, Some(_)) => quote!(self.#field_ident.as_ref()),
            (false, false, None) => quote!(&self.#field_ident),
        };

        let full_name = match (&struct_prefix, prefix) {
            (Some(ref s), Some(ref p)) => format!("{}{}{}", s, p, accessor_name),
            (Some(ref p), None) | (None, Some(ref p)) => format!("{}{}", p, accessor_name),
            _ => accessor_name,
        };

        let ident = Ident::new(&full_name, Span::call_site());

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

    let inherited_vis: Option<Visibility> = parse_visibility(enum_attrs, enum_vis.clone(), Some(enum_vis.clone()));

    let mut enum_prefix = None;
    let mut fields_only_prefix = false;

    visit_nested_attrs(enum_attrs, |meta, _| match meta {
        NestedMeta::Meta(Meta::NameValue(ref meta)) if meta.ident == "prefix" => {
            if let Lit::Str(ref s) = meta.lit {
                enum_prefix = Some(s.value());
            }
        }
        NestedMeta::Meta(Meta::NameValue(ref meta)) if meta.ident == "prefix_fields" => {
            if let Lit::Str(ref s) = meta.lit {
                enum_prefix = Some(s.value());
                fields_only_prefix = true;
            }
        }
        _ => (),
    });

    let mut done: Vec<String> = Vec::new();
    let mut ignored: Vec<&Variant> = Vec::new();

    // quick run-through to get ignored variants and check for non-struct variants
    for variant in data.variants.iter() {
        let is_struct = match variant.fields {
            Fields::Named(_) => true,
            _ => false,
        };

        if !is_struct {
            panic!("AutoAccessor only works on struct enum variants with named fields");
        }

        if let Visited::Halted = visit_nested_attrs(&variant.attrs, |meta, _| match meta {
            NestedMeta::Meta(Meta::Word(ref ident)) => {
                if ident == "ignore" {
                    return Visit::Halt;
                }

                Visit::Continue
            }
            _ => Visit::Continue,
        }) {
            ignored.push(variant);
        }
    }

    let accessors = data.variants.iter().flat_map(|variant| {
        let mut field_accessors = Vec::new();

        'skip_field: for field in variant.fields.iter() {
            let ident: Ident = field.ident.clone().unwrap();

            let field_ident = ident.to_string();

            if field_ident.starts_with('_') {
                continue 'skip_field;
            }

            if !done.contains(&field_ident) {
                let ty = &field.ty;

                let mut rename = None;

                let mut clonable = false;
                let mut copyable = false;

                let mut prefix = None;

                let mut presence = 0;

                let mut docs: Vec<&Attribute> = Vec::new();

                let mut vis: Option<Visibility> = inherited_vis.clone();

                for other_variant in data.variants.iter().filter(|v| !ignored.contains(&v)) {
                    for other_field in other_variant.fields.iter() {
                        if other_field.ident == field.ident {
                            if other_field.ty != *ty {
                                panic!("Same-name fields must have the same type!");
                            }

                            presence += 1;

                            vis = parse_visibility(&other_field.attrs, enum_vis.clone(), vis.clone());

                            let new_docs = iter_docs(&other_field.attrs);

                            if let Visited::Halted = visit_nested_attrs(&other_field.attrs, |meta, _| match meta {
                                NestedMeta::Meta(Meta::Word(ref ident)) => {
                                    if ident == "clone" {
                                        clonable = true;
                                    } else if ident == "copy" {
                                        copyable = true;
                                    } else if ident == "ignore" {
                                        return Visit::Halt;
                                    }

                                    Visit::Continue
                                }
                                NestedMeta::Meta(Meta::NameValue(ref meta)) if meta.ident == "prefix" => {
                                    if let Lit::Str(ref s) = meta.lit {
                                        prefix = Some(s.value());
                                    }

                                    Visit::Continue
                                }
                                NestedMeta::Meta(Meta::NameValue(ref meta)) if meta.ident == "rename" => {
                                    if let Lit::Str(ref s) = meta.lit {
                                        rename = Some(s.value());
                                    }

                                    Visit::Continue
                                }
                                _ => Visit::Continue,
                            }) {
                                // Halted only if the field was set to ignore, to skip it
                                done.push(field_ident);

                                continue 'skip_field;
                            }

                            for new_doc in new_docs {
                                if !docs.contains(&new_doc) {
                                    docs.push(new_doc);
                                }
                            }
                        }
                    }
                }

                docs.dedup();

                let flattened_type = flatten(ty);

                copyable = copyable
                    || flattened_type.as_ref().map(is_trivially_copyable).unwrap_or(false)
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
                    .filter(|v| !ignored.contains(&v))
                    .filter_map(|variant| {
                        let v = &variant.ident;

                        let has_binding = variant.fields.iter().any(|field| field.ident.as_ref() == Some(&ident));

                        if !has_binding {
                            None
                        } else {
                            let borrow = if !copyable { quote!(ref) } else { quote!() };

                            let body = match (copyable, clonable, is_in_all, &flattened_type) {
                                // variant 1, 2 copy
                                (true, _, _, Some(_)) => quote!(#ident),
                                // variant 1, 2 clone
                                (_, true, _, Some(_)) => quote!(#ident.clone()),
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

                let field_name = rename.unwrap_or_else(|| field_ident.clone());

                let method = match (&enum_prefix, prefix) {
                    (Some(ref e), Some(ref p)) => format!("{}{}{}", e, p, field_name),
                    (Some(ref p), _) | (_, Some(ref p)) => format!("{}{}", p, field_name),
                    _ => field_name,
                };

                let method_ident = Ident::new(&method, Span::call_site());

                field_accessors.push(quote! {
                    #(#docs)*
                    #[inline]
                    #vis fn #method_ident(&self) -> #ty {
                        match *self { #(#body)* }
                    }
                });

                done.push(field_ident);
            }
        }

        field_accessors.into_iter()
    });

    let is_variants = data.variants.iter().filter(|v| !ignored.contains(&v)).map(|variant| {
        let v = &variant.ident;

        let mut variant_name = v.to_string().to_lowercase();

        let mut prefix = None;

        let vis = parse_visibility(&variant.attrs, enum_vis.clone(), inherited_vis.clone());

        visit_nested_attrs(&variant.attrs, |meta, _| match meta {
            NestedMeta::Meta(Meta::NameValue(ref meta)) if meta.ident == "prefix" => {
                if let Lit::Str(ref s) = meta.lit {
                    prefix = Some(s.value());
                }
            }
            NestedMeta::Meta(Meta::NameValue(ref meta)) if meta.ident == "rename" => {
                if let Lit::Str(ref s) = meta.lit {
                    variant_name = s.value();
                }
            }
            _ => (),
        });

        let doc = Lit::new(Literal::string(&format!(
            "Returns true if the enum is variant [`{0}`](#variant.{0})",
            v
        )));

        let mut variant_docs = iter_docs(&variant.attrs);

        let first_variant_doc = variant_docs.next();

        // if there is a variant doc comment, insert an extra newline
        let extra_doc_space = first_variant_doc.as_ref().map(|_| quote!(#[doc = ""]));

        let method = match (&enum_prefix, prefix) {
            (Some(ref e), Some(ref p)) if !fields_only_prefix => format!("{}{}{}", e, p, variant_name),
            (Some(ref p), None) if !fields_only_prefix => format!("{}is_{}", p, variant_name),
            (None, Some(ref p)) => format!("{}{}", p, variant_name),
            _ => format!("is_{}", variant_name),
        };

        let method = Ident::new(&method, Span::call_site());

        quote! {
            #[doc = #doc]
            #extra_doc_space
            #first_variant_doc
            #(#variant_docs)*
            #[inline]
            #vis fn #method(&self) -> bool {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Visit {
    Continue,
    Halt,
}

impl From<()> for Visit {
    fn from(_: ()) -> Visit {
        Visit::Continue
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Visited {
    Completed,
    Halted,
}

fn visit_attrs<V: Into<Visit>>(attrs: &[Attribute], mut f: impl FnMut(&Meta, &Attribute) -> V) -> Visited {
    for attr in attrs {
        if attr.style == AttrStyle::Outer {
            if let Ok(ref meta) = attr.parse_meta() {
                if let Visit::Halt = f(meta, attr).into() {
                    return Visited::Halted;
                }
            }
        }
    }

    Visited::Completed
}

fn visit_nested_attrs<V: Into<Visit>>(attrs: &[Attribute], mut f: impl FnMut(&NestedMeta, &Attribute) -> V) -> Visited {
    visit_attrs(attrs, |meta, attr| match meta {
        Meta::List(ref meta_list) if meta_list.ident == "access" => {
            for meta in &meta_list.nested {
                if f(meta, attr).into() == Visit::Halt {
                    return Visit::Halt;
                }
            }

            Visit::Continue
        }
        _ => Visit::Continue,
    })
}

/// Lazily collect all "doc" attributes
fn iter_docs(attrs: &[Attribute]) -> impl Iterator<Item = &Attribute> {
    attrs.iter().filter(|attr| {
        if attr.style != AttrStyle::Outer {
            return false;
        }

        match attr.parse_meta().ok() {
            Some(Meta::NameValue(ref meta)) if meta.ident == "doc" => true,
            _ => false,
        }
    })
}

fn parse_visibility(
    attrs: &[Attribute],
    parent_vis: Visibility,
    initial_vis: Option<Visibility>,
) -> Option<Visibility> {
    let mut vis = initial_vis;

    visit_nested_attrs(attrs, |meta, _| match meta {
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
                    "private" | "hidden" => None,
                    _ => Some(syn::parse_str(&value).unwrap()),
                };
            }
        }
        _ => (),
    });

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
                    PathArguments::AngleBracketed(AngleBracketedGenericArguments { ref args, .. }) => {
                        match args.first().unwrap().value() {
                            GenericArgument::Type(ref ty) => Some(ty.clone()),
                            _ => None,
                        }
                    }
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
        "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "isize", "usize", "bool", "char", "f32", "f64",
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
