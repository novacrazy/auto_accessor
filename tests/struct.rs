#[macro_use]
extern crate auto_accessor;

#[derive(AutoAccessor)]
#[access(inherit, prefix = "hello_")]
pub struct Test<T: Clone> {
    /// Hello, there
    pub a: i32,
    pub l: char,
    pub n: Option<Vec<i32>>,

    #[access(clone, vis = "pub(self)")]
    pub j: Option<Vec<i32>>,

    #[access(clone, rename = "wumbo")]
    pub w: Vec<bool>,

    #[access(vis = "pub(crate)", prefix = "test_")]
    o: Vec<T>,

    #[access(ignore)]
    pub ignored: bool,
}
