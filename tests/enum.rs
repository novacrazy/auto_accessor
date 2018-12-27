#[macro_use]
extern crate auto_accessor;

#[derive(AutoAccessor)]
#[access(private, prefix_fields = "hello_")]
pub enum Test<T: Clone> {
    A {
        /// Hello, there
        a: i32,
        b: T,
        l: char,
        n: Option<Vec<i32>>,
        #[access(clone)]
        j: ::std::option::Option<Vec<i32>>,
        #[access(clone)]
        w: Vec<bool>,
        o: Vec<char>,
        ignored: bool,
    },
    /// This is the second variant
    #[access(rename = "BType", prefix = "has_")]
    B {
        /// Also here
        a: i32,
        #[access(clone, prefix = "testing_")]
        b: T,
        /// Hmmm...
        l: char,
        n: Option<Vec<i32>>,
        #[access(public)]
        w: Vec<bool>,
        o: Vec<char>,
        ignored: bool,
    },
    #[access(vis = "pub(crate)")]
    C {
        /// One
        /// Two
        /// Three
        c: Option<String>,
        m: Option<i32>,
        #[access(default)]
        u: u32,
        #[access(rename = "aaaaaa")]
        a: i32,
        k: &'static str,
        l: char,
        n: Option<Vec<i32>>,
        o: Vec<char>,
        #[access(public)]
        e: Vec<&'static str>,
        _ignored: bool,
        #[access(ignore)]
        ignored: bool,
    },
}

fn my_string() -> &'static str {
    "Hello, World!"
}

#[derive(AutoAccessor)]
#[access(vis = "pub")]
pub enum Test2 {
    A {
        #[access(vis = "pub(crate)")]
        a: i32,
        k: char,
        #[access(default = "my_string")]
        m: &'static str,
    },
    #[access(ignore_all)]
    B { a: i32 },
    C {
        #[access(default)]
        k: char,
    },
}

fn main() {}
