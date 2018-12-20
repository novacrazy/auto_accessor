#[macro_use]
extern crate auto_accessor;

#[derive(AutoAccessor)]
pub struct Test<T: Clone> {
    /// Hello, there
    pub a: i32,
    pub l: char,
    pub n: Option<Vec<i32>>,

    #[access(clone)]
    pub j: Option<Vec<i32>>,

    #[access(clone)]
    pub w: Vec<bool>,
    pub o: Vec<T>,

    #[access(ignore)]
    pub ignored: bool,
}
