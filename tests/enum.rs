#[macro_use]
extern crate auto_accessor;

#[derive(AutoAccessor)]
pub enum Test<T: Clone> {
    A {
        /// Hello, there
        a: i32,
        l: char,
        n: Option<Vec<i32>>,
        #[access(clone)]
        j: Option<Vec<i32>>,
        #[access(clone)]
        w: Vec<bool>,
        o: Vec<char>,
        ignored: bool,
    },
    B {
        /// Also here
        a: i32,
        #[access(clone)]
        b: T,
        /// Hmmm...
        l: char,
        n: Option<Vec<i32>>,
        w: Vec<bool>,
        o: Vec<char>,
        ignored: bool,
    },
    C {
        /// One
        /// Two
        /// Three
        c: Option<String>,
        m: Option<i32>,
        a: i32,
        k: &'static str,
        l: char,
        n: Option<Vec<i32>>,
        o: Vec<char>,
        e: Vec<&'static str>,
        _ignored: bool,
        #[access(ignore)]
        ignored: bool,
    },
}

#[derive(AutoAccessor)]
pub enum Test2 {
    A { a: i32 },
}

fn main() {}
