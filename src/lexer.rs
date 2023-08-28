use crate::errors::LexError;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Lexem {
    Int(i64),
    Float(f64),
    String(String),
    BinaryOp(String),
    Symbol(String),
    Keyword(String),
    If,
    LPar,
    RPar,
}

impl fmt::Display for Lexem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = match self {
            Lexem::Int(i) => format!("{}", i),
            Lexem::Float(f) => format!("{}", f),
            Lexem::String(s) => format!("{}", s),
            Lexem::BinaryOp(s) => format!("{}", s),
            Lexem::Symbol(s) => format!("{}", s),
            Lexem::Keyword(s) => format!("{}", s),
            Lexem::If => format!("if"),
            Lexem::LPar => format!("("),
            Lexem::RPar => format!(")"),
        };

        f.write_str(value.as_str())
    }
}

pub fn lex(input: &str) -> Result<Vec<Lexem>, LexError> {
    let mut lexems = Vec::new();
    let mut chars = input.chars().collect::<Vec<char>>();

    while chars.len() > 0 {
        let mut top = chars.remove(0);

        match top {
            '(' => lexems.push(Lexem::LPar),
            ')' => lexems.push(Lexem::RPar),
            '"' => {
                let mut s = String::new();

                while chars.len() > 0 && chars[0] != '"' {
                    s.push(chars.remove(0));
                }
                if chars.len() > 0 && chars[0] == '"' {
                    chars.remove(0);
                } else {
                    return Err(LexError::NotTerminated);
                }

                lexems.push(Lexem::String(s));
            }
            _ => {
                let mut s = String::new();

                while chars.len() > 0 && !top.is_whitespace() && top != '(' && top != ')' {
                    s.push(top);

                    let nxt = chars[0];
                    if nxt == '(' || nxt == ')' {
                        break;
                    }

                    top = chars.remove(0);
                }

                if !s.is_empty() {
                    let lexem = {
                        if let Ok(i) = s.parse::<i64>() {
                            Lexem::Int(i)
                        } else if let Ok(f) = s.parse::<f64>() {
                            Lexem::Float(f)
                        } else {
                            match s.as_str() {
                                "define" | "list" | "print" | "lambda" | "car" | "cdr"
                                | "length" | "null?" | "map" | "filter" => Lexem::Keyword(s),
                                "+" | "-" | "*" | "/" | "%" | "<" | ">" | "=" | "!=" | "or"
                                | "and" => Lexem::BinaryOp(s),
                                "if" => Lexem::If,
                                _ => Lexem::Symbol(s),
                            }
                        }
                    };
                    lexems.push(lexem);
                }
            }
        }
    }

    Ok(lexems)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_multiply() {
        let prog = "(* 4 8)";
        let tokens = lex(prog).unwrap_or(vec![]);

        assert_eq!(
            tokens,
            vec![
                Lexem::LPar,
                Lexem::BinaryOp("*".to_string()),
                Lexem::Int(4),
                Lexem::Int(8),
                Lexem::RPar,
            ]
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
        let tokens = lex(prog).unwrap_or(vec![]);

        assert_eq!(
            tokens,
            vec![
                Lexem::LPar,
                Lexem::LPar,
                Lexem::Keyword("define".to_string()),
                Lexem::Symbol("r".to_string()),
                Lexem::Int(2),
                Lexem::RPar,
                Lexem::LPar,
                Lexem::Keyword("define".to_string()),
                Lexem::Symbol("pi".to_string()),
                Lexem::Float(3.14),
                Lexem::RPar,
                Lexem::LPar,
                Lexem::BinaryOp("*".to_string()),
                Lexem::Symbol("pi".to_string()),
                Lexem::LPar,
                Lexem::BinaryOp("*".to_string()),
                Lexem::Symbol("r".to_string()),
                Lexem::Symbol("r".to_string()),
                Lexem::RPar,
                Lexem::RPar,
                Lexem::RPar
            ]
        )
    }
}
