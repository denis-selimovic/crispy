use crate::ast::ASTNode;
use crate::errors::ParserError;
use crate::lexer::{lex, Lexem};
use std::rc::Rc;

pub fn parse(input: &str) -> Result<ASTNode, ParserError> {
    let lex_analysis = lex(input);

    if let Err(err) = lex_analysis {
        return Err(ParserError::Lex(err.to_string()));
    }

    let mut lexems = lex_analysis
        .unwrap_or_default()
        .into_iter()
        .rev()
        .collect::<Vec<_>>();
    let parsed = parse_from_lexems(&mut lexems)?;

    Ok(parsed)
}

fn parse_from_lexems(lexems: &mut Vec<Lexem>) -> Result<ASTNode, ParserError> {
    let first = lexems.pop();

    if first != Some(Lexem::LPar) {
        return Err(ParserError::UnexpectedToken(
            format!("{}", Lexem::LPar),
            match first {
                Some(l) => format!("{}", l),
                None => "unkown token".to_string(),
            },
        ));
    }

    let mut data = Vec::new();

    while !lexems.is_empty() {
        let lexem = lexems.pop();

        if lexem == None {
            return Err(ParserError::EndOfFile);
        }

        match lexem.unwrap() {
            Lexem::If => data.push(ASTNode::If),
            Lexem::Int(i) => data.push(ASTNode::Int(i)),
            Lexem::Float(f) => data.push(ASTNode::Float(f)),
            Lexem::String(s) => data.push(ASTNode::String(s)),
            Lexem::BinaryOp(op) => data.push(ASTNode::BinaryOp(op)),
            Lexem::Keyword(k) => data.push(ASTNode::Keyword(k)),
            Lexem::Symbol(s) => data.push(ASTNode::Symbol(s)),
            Lexem::LPar => {
                lexems.push(Lexem::LPar);
                data.push(parse_from_lexems(lexems)?);
            }
            Lexem::RPar => return Ok(ASTNode::List(Rc::new(data))),
        }
    }

    Ok(ASTNode::List(Rc::new(data)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_multiply() {
        let prog = "(/ 3 2)";
        let ast = parse(prog).unwrap();

        assert_eq!(
            ast,
            ASTNode::List(Rc::new(vec![
                ASTNode::BinaryOp("/".to_string()),
                ASTNode::Int(3),
                ASTNode::Int(2),
            ]))
        )
    }

    #[test]
    fn test_calculate_area() {
        let prog = "
            (
                (define r 2)
                (define pi 3.14)
                (* pi (* r r))
            )
        ";
        let ast = parse(prog).unwrap();

        assert_eq!(
            ast,
            ASTNode::List(Rc::new(vec![
                ASTNode::List(Rc::new(vec![
                    ASTNode::Keyword("define".to_string()),
                    ASTNode::Symbol("r".to_string()),
                    ASTNode::Int(2),
                ])),
                ASTNode::List(Rc::new(vec![
                    ASTNode::Keyword("define".to_string()),
                    ASTNode::Symbol("pi".to_string()),
                    ASTNode::Float(3.14),
                ])),
                ASTNode::List(Rc::new(vec![
                    ASTNode::BinaryOp("*".to_string()),
                    ASTNode::Symbol("pi".to_string()),
                    ASTNode::List(Rc::new(vec![
                        ASTNode::BinaryOp("*".to_string()),
                        ASTNode::Symbol("r".to_string()),
                        ASTNode::Symbol("r".to_string()),
                    ])),
                ]))
            ]))
        )
    }
}
