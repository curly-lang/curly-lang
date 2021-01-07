use std::collections::HashMap;

use super::types::Type;

#[derive(Debug)]
pub struct Scope
{
    pub variables: HashMap<String, Type>,
    pub funcs: HashMap<(String, Type), usize>,
    pub parent: Option<Box<Scope>>
}

