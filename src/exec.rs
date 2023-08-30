use crate::ast::ASTNode;
use crate::errors::CompileError;
use crate::parser::parse;
use crate::scope::Scope;
use std::cell::RefCell;
use std::rc::Rc;

pub fn exec(input: &str, env: Rc<RefCell<Scope>>) -> Result<ASTNode, CompileError> {
    let parsed = parse(input);

    match parsed {
        Err(e) => Err(CompileError::Parser(e.to_string())),
        Ok(ast) => eval_ast(&ast, env.clone()),
    }
}

fn eval_ast(ast: &ASTNode, env: Rc<RefCell<Scope>>) -> Result<ASTNode, CompileError> {
    match ast {
        ASTNode::Void => Ok(ASTNode::Void),
        ASTNode::Bool(_) => Ok(ast.clone()),
        ASTNode::Int(i) => Ok(ASTNode::Int(*i)),
        ASTNode::Float(f) => Ok(ASTNode::Float(*f)),
        ASTNode::String(s) => Ok(ASTNode::String(s.clone())),
        ASTNode::DataList(l) => Ok(ASTNode::DataList(l.to_vec())),
        ASTNode::Lambda(_, _, _) => Ok(ASTNode::Void),
        ASTNode::Symbol(s) => eval_symbol(s, env.clone()),
        ASTNode::List(l) => {
            let head = &l[0];

            match head {
                ASTNode::If => eval_if(l, env.clone()),
                ASTNode::Lambda(params, body, func_scope) => {
                    eval_lambda(params, body, func_scope, l, env.clone())
                }
                ASTNode::Symbol(s) => match env.borrow().get(&s) {
                    None => return Err(CompileError::UnboundLambda(s.to_string())),
                    Some(lambda) => match lambda {
                        ASTNode::Lambda(params, body, func_scope) => {
                            eval_lambda(&params, &body, &func_scope, l, env.clone())
                        }
                        _ => return Err(CompileError::TypeMismatch("lambda".to_string())),
                    },
                },
                ASTNode::BinaryOp(op) => eval_operator(&op, l, env.clone()),
                ASTNode::Keyword(keyword) => eval_keyword(&keyword, l, env.clone()),
                _ => {
                    let mut nlist = Vec::new();

                    for node in l.iter() {
                        let val = eval_ast(node, env.clone())?;

                        if val != ASTNode::Void {
                            nlist.push(val)
                        }
                    }

                    match &nlist[0] {
                        ASTNode::Lambda(_, _, _) => {
                            eval_ast(&ASTNode::List(Rc::new(nlist)), env.clone())
                        }
                        _ => Ok(ASTNode::List(Rc::new(nlist))),
                    }
                }
            }
        }
        _ => Err(CompileError::UnexpectedNode),
    }
}

fn eval_symbol(var: &str, env: Rc<RefCell<Scope>>) -> Result<ASTNode, CompileError> {
    let val = match var {
        "#f" => ASTNode::Bool(false),
        "#t" => ASTNode::Bool(true),
        "#nil" => ASTNode::Void,
        _ => match env.borrow().get(var) {
            None => return Err(CompileError::UnboundVariable(var.to_string())),
            Some(ast) => ast,
        },
    };

    Ok(val.clone())
}

fn eval_if(list: &Rc<Vec<ASTNode>>, env: Rc<RefCell<Scope>>) -> Result<ASTNode, CompileError> {
    if list.len() != 4 {
        return Err(CompileError::Syntax(
            "Invalid syntax for 'if' statement".to_string(),
        ));
    }

    let truth_val = match eval_ast(&list[1], env.clone())? {
        ASTNode::Bool(b) => b,
        _ => return Err(CompileError::TypeMismatch("boolean".to_string())),
    };

    if truth_val {
        eval_ast(&list[2], env.clone())
    } else {
        eval_ast(&list[3], env.clone())
    }
}

fn eval_lambda(
    params: &Vec<String>,
    body: &Rc<Vec<ASTNode>>,
    scope: &Rc<RefCell<Scope>>,
    list: &Rc<Vec<ASTNode>>,
    env: Rc<RefCell<Scope>>,
) -> Result<ASTNode, CompileError> {
    let new_scope = Scope::from(scope.clone());
    let new_env = Rc::new(RefCell::new(new_scope));

    for i in 0..params.len() {
        let val = eval_ast(&list[i + 1], env.clone())?;
        new_env.borrow_mut().set(&params[i], val);
    }

    eval_ast(&ASTNode::List(body.clone()), new_env.clone())
}

fn eval_operator(
    op: &str,
    list: &Rc<Vec<ASTNode>>,
    env: Rc<RefCell<Scope>>,
) -> Result<ASTNode, CompileError> {
    use ASTNode::*;

    if list.len() != 3 {
        return Err(CompileError::WrongParamNumber(op.to_string()));
    }

    let first = eval_ast(&list[1], env.clone())?;
    let second = eval_ast(&list[2], env.clone())?;

    match op {
        "+" => match (first, second) {
            (Int(lhs), Int(rhs)) => Ok(Int(lhs + rhs)),
            (Float(lhs), Float(rhs)) => Ok(Float(lhs + rhs)),
            (Int(lhs), Float(rhs)) => Ok(Float(lhs as f64 + rhs)),
            (Float(lhs), Int(rhs)) => Ok(Float(lhs + rhs as f64)),
            (String(lhs), String(rhs)) => Ok(String(format!("{}{}", lhs, rhs))),
            _ => return Err(CompileError::UnsupportedOperands(op.to_string())),
        },
        "-" => match (first, second) {
            (Int(lhs), Int(rhs)) => Ok(Int(lhs - rhs)),
            (Float(lhs), Float(rhs)) => Ok(Float(lhs - rhs)),
            (Int(lhs), Float(rhs)) => Ok(Float(lhs as f64 - rhs)),
            (Float(lhs), Int(rhs)) => Ok(Float(lhs - rhs as f64)),
            _ => return Err(CompileError::UnsupportedOperands(op.to_string())),
        },
        "*" => match (first, second) {
            (Int(lhs), Int(rhs)) => Ok(Int(lhs * rhs)),
            (Float(lhs), Float(rhs)) => Ok(Float(lhs * rhs)),
            (Int(lhs), Float(rhs)) => Ok(Float(lhs as f64 * rhs)),
            (Float(lhs), Int(rhs)) => Ok(Float(lhs * rhs as f64)),
            _ => return Err(CompileError::UnsupportedOperands(op.to_string())),
        },
        "/" => match (first, second) {
            (Int(lhs), Int(rhs)) => Ok(Int(lhs / rhs)),
            (Float(lhs), Float(rhs)) => Ok(Float(lhs / rhs)),
            (Int(lhs), Float(rhs)) => Ok(Float(lhs as f64 / rhs)),
            (Float(lhs), Int(rhs)) => Ok(Float(lhs / rhs as f64)),
            _ => return Err(CompileError::UnsupportedOperands(op.to_string())),
        },
        "%" => match (first, second) {
            (Int(lhs), Int(rhs)) => Ok(Int(lhs % rhs)),
            (Float(lhs), Float(rhs)) => Ok(Float(lhs % rhs)),
            (Int(lhs), Float(rhs)) => Ok(Float(lhs as f64 % rhs)),
            (Float(lhs), Int(rhs)) => Ok(Float(lhs % rhs as f64)),
            _ => return Err(CompileError::UnsupportedOperands(op.to_string())),
        },
        "<" => match (first, second) {
            (Int(lhs), Int(rhs)) => Ok(Bool(lhs < rhs)),
            (Float(lhs), Float(rhs)) => Ok(Bool(lhs < rhs)),
            (Int(lhs), Float(rhs)) => Ok(Bool((lhs as f64) < rhs)),
            (Float(lhs), Int(rhs)) => Ok(Bool(lhs < (rhs as f64))),
            _ => return Err(CompileError::UnsupportedOperands(op.to_string())),
        },
        ">" => match (first, second) {
            (Int(lhs), Int(rhs)) => Ok(Bool(lhs > rhs)),
            (Float(lhs), Float(rhs)) => Ok(Bool(lhs > rhs)),
            (Int(lhs), Float(rhs)) => Ok(Bool((lhs as f64) > rhs)),
            (Float(lhs), Int(rhs)) => Ok(Bool(lhs > (rhs as f64))),
            _ => return Err(CompileError::UnsupportedOperands(op.to_string())),
        },
        "=" => match (first, second) {
            (Int(lhs), Int(rhs)) => Ok(Bool(lhs == rhs)),
            (String(lhs), String(rhs)) => Ok(Bool(lhs == rhs)),
            _ => return Err(CompileError::UnsupportedOperands(op.to_string())),
        },
        "!=" => match (first, second) {
            (Int(lhs), Int(rhs)) => Ok(Bool(lhs != rhs)),
            (Float(lhs), Float(rhs)) => Ok(Bool(lhs != rhs)),
            (Int(lhs), Float(rhs)) => Ok(Bool((lhs as f64) != rhs)),
            (Float(lhs), Int(rhs)) => Ok(Bool(lhs != (rhs as f64))),
            (String(lhs), String(rhs)) => Ok(Bool(lhs != rhs)),
            _ => return Err(CompileError::UnsupportedOperands(op.to_string())),
        },
        "and" => match (first, second) {
            (Bool(lhs), Bool(rhs)) => Ok(Bool(lhs && rhs)),
            _ => return Err(CompileError::UnsupportedOperands(op.to_string())),
        },
        "or" => match (first, second) {
            (Bool(lhs), Bool(rhs)) => Ok(Bool(lhs || rhs)),
            _ => return Err(CompileError::UnsupportedOperands(op.to_string())),
        },
        _ => return Err(CompileError::UnsupportedOperator(op.to_string())),
    }
}

fn eval_keyword(
    keyword: &str,
    list: &Rc<Vec<ASTNode>>,
    env: Rc<RefCell<Scope>>,
) -> Result<ASTNode, CompileError> {
    match keyword {
        "print" => eval_print(list, env.clone()),
        "car" => eval_car(list, env.clone()),
        "cdr" => eval_cdr(list, env.clone()),
        "length" => eval_length(list, env.clone()),
        "null?" => eval_null(list, env.clone()),
        _ => {
            return Err(CompileError::Syntax(format!(
                "unknown keyword '{}'",
                keyword
            )))
        }
    }
}

fn eval_print(list: &Rc<Vec<ASTNode>>, env: Rc<RefCell<Scope>>) -> Result<ASTNode, CompileError> {
    let mut res = Vec::new();

    for ast in list[1..].iter() {
        res.push(eval_ast(ast, env.clone())?);
    }

    let output = res
        .iter()
        .map(|ast| format!("{}", ast))
        .collect::<Vec<String>>()
        .join(" ");

    println!("{}\n", output);

    Ok(ASTNode::Void)
}

fn eval_car(list: &Rc<Vec<ASTNode>>, env: Rc<RefCell<Scope>>) -> Result<ASTNode, CompileError> {
    if list.len() != 2 {
        return Err(CompileError::Syntax(
            "Invalid syntax for 'car' function".to_string(),
        ));
    }

    let res = eval_ast(&list[1], env.clone())?;

    match res {
        ASTNode::DataList(l) => Ok(l[0].clone()),
        _ => Err(CompileError::UnexpectedNode),
    }
}

fn eval_cdr(list: &Rc<Vec<ASTNode>>, env: Rc<RefCell<Scope>>) -> Result<ASTNode, CompileError> {
    if list.len() != 2 {
        return Err(CompileError::Syntax(
            "Invalid syntax for 'cdr' function".to_string(),
        ));
    }

    let res = eval_ast(&list[1], env.clone())?;

    match res {
        ASTNode::DataList(l) => Ok(ASTNode::DataList(l[1..].to_vec())),
        _ => Err(CompileError::UnexpectedNode),
    }
}

fn eval_length(list: &Rc<Vec<ASTNode>>, env: Rc<RefCell<Scope>>) -> Result<ASTNode, CompileError> {
    if list.len() != 2 {
        return Err(CompileError::Syntax(
            "Invalid syntax for 'length' function".to_string(),
        ));
    }

    let res = eval_ast(&list[1], env.clone())?;

    match res {
        ASTNode::DataList(l) => Ok(ASTNode::Int(l.len() as i64)),
        ASTNode::List(l) => Ok(ASTNode::Int(l.len() as i64)),
        _ => Err(CompileError::UnexpectedNode),
    }
}

fn eval_null(list: &Rc<Vec<ASTNode>>, env: Rc<RefCell<Scope>>) -> Result<ASTNode, CompileError> {
    if list.len() != 2 {
        return Err(CompileError::Syntax(
            "Invalid syntax for 'null?' function".to_string(),
        ));
    }

    let res = eval_ast(&list[1], env.clone())?;

    match res {
        ASTNode::DataList(l) => Ok(ASTNode::Bool(l.len() == 0)),
        ASTNode::List(l) => Ok(ASTNode::Bool(l.len() == 0)),
        _ => Err(CompileError::UnexpectedNode),
    }
}
