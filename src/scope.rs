use crate::ast::ASTNode;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Default, PartialEq)]
pub struct Scope {
    parent: Option<Rc<RefCell<Scope>>>,
    v: HashMap<String, ASTNode>,
}

impl Scope {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn from(parent: Rc<RefCell<Scope>>) -> Self {
        Scope {
            parent: Some(parent),
            v: HashMap::new(),
        }
    }

    pub fn update(&mut self, other: Rc<RefCell<Scope>>) {
        self.v
            .extend(other.borrow().v.iter().map(|(k, v)| (k.clone(), v.clone())))
    }

    pub fn get(&self, name: &str) -> Option<ASTNode> {
        match self.v.get(name) {
            Some(val) => Some(val.clone()),
            None => self.parent.as_ref().and_then(|v| v.borrow().get(name)),
        }
    }

    pub fn set(&mut self, name: &str, val: ASTNode) {
        self.v.insert(name.to_string(), val);
    }
}
