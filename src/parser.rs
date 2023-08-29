use crate::ast::ASTNode;
use crate::errors::ParserError;
use crate::lexer::{lex, Lexem};
use std::rc::Rc;

pub fn parser(input: &str) -> Result<ASTNode, ParserError> {
    let lex_analysis = lex(input);

    if let Err(err) = lex_analysis {
        return Err(ParserError::Lex(err.to_string()));
    }

    let mut lexems = lex_analysis.unwrap_or_default();
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
