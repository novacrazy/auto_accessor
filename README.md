auto_accessor
============

**This README is WIP and incomplete**

## Overview

`AutoAccessor` us a custom procedural macro derive for automatically generating efficient accessor methods on structures with named fields and enums with structure variants with named fields. Named fields are required (for now).

## Limitations

Before getting too far, it should be noted that procedural macros are limited to syntactic-only analysis, therefore `AutoAccessor` has no real knowledge of the types used in your structures or enums. Based on the parsed types as text, it will attempt to identify things as well as possible, but it may need explicit help as detailed in the Attributes section.

## "Automatic" Behavior

### `Copy`/`Clone` types:

Rust primitives and explicitely annotated `Copy`/`Clone` types can be returned via copy/cloning rather than by reference, so no more `&i32` or `&bool` accessors.

### `Option` types:

`AutoAccessor` will attempt to parse the outer type of fields, and if it is found to be either `Option`, `::std::option::Option` or `std::option::Option`, will attempt to "flatten" the type.

This is to avoid return types like `Option<&Option<Vec<i32>>>`.

Instead, it will return `Option<&Vec<i32>>`, much simpler.

### Visibility

`AutoAccessor` attempts to deduce the desired visibility of the resulting accessor methods based on the visibility of the `struct`/`enum` and the individual fields.

The default visibility can be overruled by top-level attributes or per-field attributes.

### Ignored Fields

**Fields that begin with `_` are automatically ignored!**

## Attributes

### `#[access(copy)]` and `#[access(clone)]`

Marks the type as either copyable or clonable, generating accessors that return owned values rather than references.

This is required for non-obvious

### `#[access(ignore)]`

Ignores

### Visibility Attributes

#### `#[access(private)]`, `#[access(public)]`, `#[access(inherit)]` and `#[access(vis = "some visibility")]`

These can be applied to the top-level `enum`/`struct`, or individual fields.



## Examples

```rust
#[macro_use]
extern crate auto_accessor;

#[derive(AutoAccessor)]
pub enum Example {
    A {
        /// some documentation
        id: u64,
        name: String,
    },
    B {
        id: u64,
        codes: Option<Vec<String>>,
    }
}
```

it will generate these accessors (and is_variant checks),
carrying over documentation:

```rust
impl Example {
    pub fn is_a(&self) -> bool {
        match *self {
            Example::A { .. } => true,
            _ => false,
        }
    }

    pub fn is_b(&self) -> bool {
        match *self {
            Example::B { .. } => true,
            _ => false,
        }
    }

    /// some documentation
    pub fn id(&self) -> u64 {
        match *self {
            Example::A {id, ..} => id,
            Example::B {id, ..} => id,
        }
    }

    pub fn name(&self) -> Option<&String> {
        match *self {
            Example::A {ref name, ..} => Some(name),
            _ => None,
        }
    }

    pub fn codes(&self) -> Option<&Vec<String>> {
        match *self {
            Example::B {ref codes, ..} => codes.as_ref(),
            _ => None,
        }
    }
}
```

and generates similar code for regular structures with named fields, just without the `Option`s when a value isn't in all variants.

Furthermore, for both structs and enums, you can specify whether to clone, copy or ignore fields.

**Fields that begin with an underscore are automatically ignored.**

Example:

```rust
#[derive(AutoAccessor)]
pub struct Example {
    #[access(clone)]
    pub user: Arc<User>,

    _ignored: bool,

    #[access(ignore)]
    pub also_ignored: bool,
}

// generates
impl Example {
    pub user(&self) -> Arc<User> {
        self.user.clone()
    }
}
```

or:

```rust
type SomeInt = u64;

#[derive(AutoAccessor)]
pub enum Example {
    A {
        #[access(clone)]
        pub user: Option<Arc<User>>,
    },
    B {
        #[access(copy)]
        id: SomeInt,
    }
}

// generates
impl Example {
    pub user(&self) -> Option<Arc<User>> {
        match *self {
            Example::A {ref user, ..} => user.clone(),
            _ => None,
        }
    }

    pub id(&self) -> Option<SomeInt> {
        match *self {
            Example::B {id, ..} => Some(id),
            _ => None,
        }
    }
}
```