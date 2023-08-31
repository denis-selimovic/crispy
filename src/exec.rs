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
        "list" => eval_list(list, env.clone()),
        "lambda" => eval_lambda_def(list, env.clone()),
        "map" => eval_map(list, env.clone()),
        "filter" => eval_filter(list, env.clone()),
        "begin" => eval_begin(list, env.clone()),
        "define" => eval_define(list, env.clone()),
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

fn eval_list(list: &Rc<Vec<ASTNode>>, env: Rc<RefCell<Scope>>) -> Result<ASTNode, CompileError> {
    let mut res = Vec::new();

    for ast in list[1..].iter() {
        let val = eval_ast(ast, env.clone())?;
        res.push(val);
    }

    Ok(ASTNode::DataList(res))
}

fn eval_lambda_def(
    list: &Rc<Vec<ASTNode>>,
    env: Rc<RefCell<Scope>>,
) -> Result<ASTNode, CompileError> {
    if list.len() != 3 {
        return Err(CompileError::Syntax(
            "Invalid syntax for lambda function definition".to_string(),
        ));
    }

    let params = match &list[1] {
        ASTNode::List(l) => {
            let mut new_params = Vec::new();

            for p in l.iter() {
                match p {
                    ASTNode::Symbol(s) => new_params.push(s.clone()),
                    _ => return Err(CompileError::UnexpectedNode),
                }
            }

            new_params
        }
        _ => return Err(CompileError::UnexpectedNode),
    };

    let body = match &list[2] {
        ASTNode::List(l) => l.clone(),
        _ => return Err(CompileError::UnexpectedNode),
    };

    Ok(ASTNode::Lambda(params, Rc::new(body.to_vec()), env.clone()))
}

fn eval_map(list: &Rc<Vec<ASTNode>>, env: Rc<RefCell<Scope>>) -> Result<ASTNode, CompileError> {
    if list.len() != 3 {
        return Err(CompileError::Syntax(
            "Invalid syntax for 'map' function".to_string(),
        ));
    }

    let closure = eval_ast(&list[1], env.clone())?;
    let try_args = eval_ast(&list[2], env.clone())?;

    let (params, body, closure_env) = match closure {
        ASTNode::Lambda(p, b, e) => {
            if p.len() != 1 {
                return Err(CompileError::WrongParamNumber("map method".to_string()));
            }

            (p, b, e)
        }
        _ => return Err(CompileError::UnexpectedNode),
    };

    let args = match try_args {
        ASTNode::DataList(l) => l.clone(),
        _ => return Err(CompileError::UnexpectedNode),
    };
    let mut res = Vec::new();

    for arg in args.iter() {
        let val = eval_ast(arg, env.clone())?;
        let new_scope = Rc::new(RefCell::new(Scope::from(closure_env.clone())));
        new_scope.borrow_mut().set(&params[0], val);

        let final_val = eval_ast(&ASTNode::List(body.clone()), new_scope.clone())?;
        res.push(final_val);
    }

    Ok(ASTNode::DataList(res))
}

fn eval_filter(list: &Rc<Vec<ASTNode>>, env: Rc<RefCell<Scope>>) -> Result<ASTNode, CompileError> {
    if list.len() != 3 {
        return Err(CompileError::Syntax(
            "Invalid syntax for 'filter' function".to_string(),
        ));
    }

    let closure = eval_ast(&list[1], env.clone())?;
    let try_args = eval_ast(&list[2], env.clone())?;

    let (params, body, closure_env) = match closure {
        ASTNode::Lambda(p, b, e) => {
            if p.len() != 1 {
                return Err(CompileError::WrongParamNumber("filter method".to_string()));
            }

            (p, b, e)
        }
        _ => return Err(CompileError::UnexpectedNode),
    };

    let args = match try_args {
        ASTNode::DataList(l) => l.clone(),
        _ => return Err(CompileError::UnexpectedNode),
    };
    let mut res = Vec::new();

    for arg in args.iter() {
        let val = eval_ast(arg, env.clone())?;
        let new_scope = Rc::new(RefCell::new(Scope::from(closure_env.clone())));
        new_scope.borrow_mut().set(&params[0], val.clone());

        let boolean_check = eval_ast(&ASTNode::List(body.clone()), new_scope.clone())?;

        match boolean_check {
            ASTNode::Bool(b) => {
                if b {
                    res.push(val.clone());
                }
            }
            _ => {
                return Err(CompileError::TypeMismatch(
                    "lambda func in filter".to_string(),
                ))
            }
        }
    }

    Ok(ASTNode::DataList(res))
}

fn eval_begin(list: &Rc<Vec<ASTNode>>, env: Rc<RefCell<Scope>>) -> Result<ASTNode, CompileError> {
    let mut res = ASTNode::Void;
    let new_env = Rc::new(RefCell::new(Scope::from(env.clone())));

    for ast in list[1..].iter() {
        res = eval_ast(ast, new_env.clone())?;
    }

    Ok(res)
}

fn eval_define(list: &Rc<Vec<ASTNode>>, env: Rc<RefCell<Scope>>) -> Result<ASTNode, CompileError> {
    if list.len() != 3 {
        return Err(CompileError::Syntax(
            "Invalid syntax for 'define' expression".to_string(),
        ));
    }

    match &list[1] {
        ASTNode::Symbol(s) => {
            let val = eval_ast(&list[2], env.clone())?;
            env.borrow_mut().set(&s, val);
        }
        ASTNode::List(l) => {
            let var = match &l[0] {
                ASTNode::Symbol(s) => s.clone(),
                _ => return Err(CompileError::UnexpectedNode),
            };
            let params = ASTNode::List(Rc::new(l[1..].to_vec()));
            let body = list[2].clone();

            let lambda_conf = vec![ASTNode::Void, params, body];
            let lambda_def = eval_lambda_def(&Rc::new(lambda_conf), env.clone())?;
            env.borrow_mut().set(&var, lambda_def);
        }
        _ => return Err(CompileError::UnexpectedNode),
    }

    Ok(ASTNode::Void)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_op() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let result = exec("(+ 3 9)", env).unwrap();
        assert_eq!(result, ASTNode::Int(12));
    }

    #[test]
    fn test_mult_op() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let result = exec("(* 11 4)", env).unwrap();
        assert_eq!(result, ASTNode::Int(44));
    }

    #[test]
    fn test_str_eq_1() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let result = exec("(= \"Alice\" \"Bob\")", env).unwrap();
        assert_eq!(result, ASTNode::Bool(false));
    }

    #[test]
    fn test_str_eq_2() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let result = exec("(= \"Bob\" \"Bob\")", env).unwrap();
        assert_eq!(result, ASTNode::Bool(true));
    }

    #[test]
    fn test_string_concat() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (
                (define names \"Alice Bob Charlie Delta \")
                (define surnames \"Alyson Bobbie\")
                (+ names surnames)
            )
        ";

        let result = exec(prog, env).unwrap();
        assert_eq!(
            result,
            ASTNode::List(Rc::new(vec![ASTNode::String(
                "Alice Bob Charlie Delta Alyson Bobbie".to_string()
            )]))
        );
    }

    #[test]
    fn test_int_gt() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let result = exec("(> 1 3)", env).unwrap();
        assert_eq!(result, ASTNode::Bool(false));
    }

    #[test]
    fn test_int_float_gt() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let result = exec("(> 10 3.14)", env).unwrap();
        assert_eq!(result, ASTNode::Bool(true));
    }

    #[test]
    fn test_int_lt() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let result = exec("(< 1 3)", env).unwrap();
        assert_eq!(result, ASTNode::Bool(true));
    }

    #[test]
    fn test_int_float_lt() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let result = exec("(< 4 3.14)", env).unwrap();
        assert_eq!(result, ASTNode::Bool(false));
    }

    #[test]
    fn test_mod_1() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let result = exec("(% 10 2)", env).unwrap();
        assert_eq!(result, ASTNode::Int(0));
    }

    #[test]
    fn test_mod_2() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let result = exec("(% 42.0 22.0)", env).unwrap();
        assert_eq!(result, ASTNode::Float(20.0));
    }

    #[test]
    fn test_neq_1() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let result = exec("(!= 10 10)", env).unwrap();
        assert_eq!(result, ASTNode::Bool(false));
    }

    #[test]
    fn test_neq_2() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let result = exec("(!= 10 2)", env).unwrap();
        assert_eq!(result, ASTNode::Bool(true));
    }

    #[test]
    fn test_neq_3() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let result = exec("(!= 3.14 2.11)", env).unwrap();
        assert_eq!(result, ASTNode::Bool(true));
    }

    #[test]
    fn test_neq_4() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let result = exec("(!= \"Bob\" \"Charlie\")", env).unwrap();
        assert_eq!(result, ASTNode::Bool(true));
    }

    #[test]
    fn test_and_both() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let result = exec("(and (< 3 5) (< 2 4))", env).unwrap();
        assert_eq!(result, ASTNode::Bool(true));
    }

    #[test]
    fn test_and_one() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let result = exec("(and (< 3 5) (> 2 4))", env).unwrap();
        assert_eq!(result, ASTNode::Bool(false));
    }

    #[test]
    fn test_and_none() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let result = exec("(and (> 1 4) (> 10 100.1))", env).unwrap();
        assert_eq!(result, ASTNode::Bool(false));
    }

    #[test]
    fn test_or_both() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let result = exec("(or (< 3 5) (< 2 4))", env).unwrap();
        assert_eq!(result, ASTNode::Bool(true));
    }

    #[test]
    fn test_or_one() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let result = exec("(or (< 3 5) (> 2 4))", env).unwrap();
        assert_eq!(result, ASTNode::Bool(true));
    }

    #[test]
    fn test_or_none() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let result = exec("(or (> 3 5) (> 1 3))", env).unwrap();
        assert_eq!(result, ASTNode::Bool(false));
    }

    #[test]
    fn test_quadratic_eq() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define a 2)
                (define b 4)
                (define c 1)
                (define x -3)
                (+ (+ (* a (* x x)) (* b x)) c)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Int(7));
    }

    #[test]
    fn test_lambda_quadratic_eq() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define qdr (lambda (a b c x) (+ (+ (* a (* x x)) (* b x)) c)))
                (qdr 2 4 1 -3)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Int(7));
    }

    #[test]
    fn test_lambda_closure_quadratic_eq() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define a 2)
                (define b 4)
                (define c 1)
                (define qdr (lambda (x) (+ (+ (* a (* x x)) (* b x)) c)))
                (qdr -3)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Int(7));
    }

    #[test]
    fn test_map() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define fn (lambda (el) (* 2 el)))
                (define l (list 1 3 5 7 9))
                (map fn l)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(
            res,
            ASTNode::DataList(vec![
                ASTNode::Int(2),
                ASTNode::Int(6),
                ASTNode::Int(10),
                ASTNode::Int(14),
                ASTNode::Int(18),
            ])
        );
    }

    #[test]
    fn test_filter() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define fn (lambda (el) (= 0 (% el 2))))
                (define l (list 1 2 3 4 5 6 7 8 9 10))
                (filter fn l)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(
            res,
            ASTNode::DataList(vec![
                ASTNode::Int(2),
                ASTNode::Int(4),
                ASTNode::Int(6),
                ASTNode::Int(8),
                ASTNode::Int(10),
            ])
        );
    }

    #[test]
    fn test_car() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define l (list 1 2 3 4 5 6 7 8 9 10))
                (car l)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Int(1));
    }

    #[test]
    fn test_cdr() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define l (list 1 2 3 4 5))
                (cdr l)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(
            res,
            ASTNode::DataList(vec![
                ASTNode::Int(2),
                ASTNode::Int(3),
                ASTNode::Int(4),
                ASTNode::Int(5),
            ])
        );
    }

    #[test]
    fn test_length() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define l (list 1 2 3 4 5 6 7 8 9 10))
                (length l)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Int(10));
    }

    #[test]
    fn test_is_null_false() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define l (list 1 2 3 4 5 6 7 8 9 10))
                (null? l)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Bool(false));
    }

    #[test]
    fn test_is_null_true() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define l (list))
                (null? l)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Bool(true));
    }

    #[test]
    fn test_create_list() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define l (list 1 2 3 4 5))
                l
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(
            res,
            ASTNode::DataList(vec![
                ASTNode::Int(1),
                ASTNode::Int(2),
                ASTNode::Int(3),
                ASTNode::Int(4),
                ASTNode::Int(5),
            ])
        );
    }

    #[test]
    fn test_create_list_empty() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define l (list))
                l
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::DataList(vec![]));
    }

    #[test]
    fn test_print_list() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define l (list 1 2 3 4 5))
                (print l)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Void);
    }

    #[test]
    fn test_inline_lambda() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                ((lambda (x y) (+ x y)) 10 20)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Int(30));
    }

    #[test]
    fn test_lambda_with_lambda_return() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define mltn
                    (lambda (n)
                        (lambda (x) (* x n))))
                (define mlt2 (mltn 2))
                (mlt2 20)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Int(40));
    }

    #[test]
    fn test_lambda_recursive() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define factorial
                    (lambda (n)
                        (if (= n 1) 1
                            (* n (factorial (- n 1))))))
                (factorial 5)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Int(120));
    }

    #[test]
    fn test_lambda_tail_recursive() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define fib
                  (lambda (n a b) 
                     (if (= n 0) a 
                       (if (= n 1) b 
                          (fib (- n 1) b (+ a b))))))
                  
                (fib 6 0 1)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Int(8));
    }

    #[test]
    fn test_circle_area_float_result() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define pi 3.14)
                (define r 1)
                (define sqr (lambda (r) (* r r)))
                (define (area r) (* pi (sqr r)))
                (area r)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Float(3.14));
    }

    #[test]
    fn test_function_define() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define (triple val) (* 3 val))
                (define (twice fn val) (fn (fn val)))
                (twice triple 5)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Int(45));
    }

    #[test]
    fn test_scope_with_begin_always_last_1() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define x 10)
                (define y 20)
                (define z 30)
                (begin
                    (define x 100)
                    (define y 200)
                    (define z 300)
                    (list x y z)
                )
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(
            res,
            ASTNode::DataList(vec![
                ASTNode::Int(100),
                ASTNode::Int(200),
                ASTNode::Int(300),
            ])
        );
    }

    #[test]
    fn test_scope_with_begin_always_last_2() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define i 10)
                (begin
                    (define i 20)
                    i
                )
                i
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Int(10));
    }

    #[test]
    fn test_if_with_signum_1() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define (signum n)
                    (if (> n 0) 1
                        (if (< n 0) -1 0)))
                (signum -5)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Int(-1));
    }

    #[test]
    fn test_if_with_signum_2() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define (signum n)
                    (if (> n 0) 1
                        (if (< n 0) -1 0)))
                (signum 10)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Int(1));
    }

    #[test]
    fn test_if_with_signum_3() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define (signum n)
                    (if (> n 0) 1
                        (if (< n 0) -1 0)))
                (signum 0)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Int(0));
    }

    #[test]
    fn test_symbol_1() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define t #t)
                t
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Bool(true));
    }

    #[test]
    fn test_symbol_2() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define f #f)
                f
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Bool(false));
    }

    #[test]
    fn test_symbol_3() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define n #nil)
                n
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Void);
    }

    #[test]
    fn test_symbol_4() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (define n 2.17)
                n
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Float(2.17));
    }

    #[test]
    fn test_if_simple_true_cond() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (if (< 2 3) -2 -3)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Int(-2));
    }

    #[test]
    fn test_if_simple_false_cond() {
        let env = Rc::new(RefCell::new(Scope::new()));
        let prog = "
            (begin
                (if (> 2 3) -2 -3)
            )
        ";

        let res = exec(prog, env).unwrap();
        assert_eq!(res, ASTNode::Int(-3));
    }
}
