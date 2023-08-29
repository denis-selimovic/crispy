use crate::scope::Scope;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum ASTNode {
    Void,
    If,
    Keyword(String),
    BinaryOp(String),
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Symbol(String),
    DataList(Vec<ASTNode>),
    List(Rc<Vec<ASTNode>>),
    Lambda(Vec<String>, Rc<Vec<ASTNode>>, Rc<RefCell<Scope>>),
}

impl fmt::Display for ASTNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ASTNode::Void => write!(f, "Void"),
            ASTNode::If => write!(f, "If"),
            ASTNode::Keyword(k) => write!(f, "{}", k),
            ASTNode::BinaryOp(op) => write!(f, "{}", op),
            ASTNode::Bool(b) => write!(f, "{}", b),
            ASTNode::Int(i) => write!(f, "{}", i),
            ASTNode::Float(fl) => write!(f, "{}", fl),
            ASTNode::String(s) => write!(f, "{}", s),
            ASTNode::Symbol(s) => write!(f, "{}", s),
            ASTNode::DataList(l) => {
                let values = l
                    .iter()
                    .map(|ast| format!("{}", ast))
                    .collect::<Vec<String>>()
                    .join(" ");
                write!(f, "({})", values)
            }
            ASTNode::List(l) => {
                let values = l
                    .iter()
                    .map(|ast| format!("{}", ast))
                    .collect::<Vec<String>>()
                    .join(" ");
                write!(f, "({})", values)
            }
            ASTNode::Lambda(params, body, _) => {
                let pv = params.join(" ");
                let bv = body
                    .iter()
                    .map(|ast| format!("{}", ast))
                    .collect::<Vec<String>>()
                    .join(" ");
                write!(f, "Lambda({}) {}", pv, bv)
            }
        }
    }
}
