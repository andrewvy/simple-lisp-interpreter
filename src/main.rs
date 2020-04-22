use std::io;

/*
 * Simple math lisp interpreter in Rust.
 *
 * This implements a very simple tokenizer/parser/interpreter for a simple lisp language.
 *
 * lisp>
 * (+ 1 2)
 * Number(3)
 *
 * lisp>
 * (- (+ (/ 100 5) (* 2 6)) 10)
 * Number(22)
 */

#[derive(Debug, Clone)]
pub enum LispExpr {
    Number(i64),
    Symbol(String),
    List(Vec<LispExpr>),
}

#[derive(Debug, Clone, Copy)]
enum TokenizerState {
    Start,
    LeftParen,
    RightParen,
    Number,
    Symbol,
    Whitespace,
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    Number(i64),
    Symbol(String),
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
}

pub fn tokenize(expr: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut start_index = 0;

    loop {
        let mut state = TokenizerState::Start;
        let mut end_index = start_index;

        for character in expr[start_index..].chars() {
            let next = match state {
                TokenizerState::Start => match character {
                    '(' => Some(TokenizerState::LeftParen),
                    ')' => Some(TokenizerState::RightParen),
                    '0'..='9' => Some(TokenizerState::Number),
                    'a'..='z' | 'A'..='Z' | '+' | '-' | '*' | '/' => Some(TokenizerState::Symbol),
                    character if character.is_whitespace() => Some(TokenizerState::Whitespace),
                    _ => None,
                },
                TokenizerState::LeftParen | TokenizerState::RightParen => None,
                TokenizerState::Number => match character {
                    '0'..='9' => Some(TokenizerState::Number),
                    _ => None,
                },
                TokenizerState::Symbol => match character {
                    'a'..='z' | 'A'..='Z' | '0'..='9' | '+' | '-' | '*' | '/' => {
                        Some(TokenizerState::Symbol)
                    }
                    _ => None,
                },
                TokenizerState::Whitespace => {
                    if character.is_whitespace() {
                        Some(TokenizerState::Whitespace)
                    } else {
                        None
                    }
                }
            };

            if let Some(next_state) = next {
                state = next_state;
                end_index += character.len_utf8();
            } else {
                break;
            }
        }

        let token_string = &expr[start_index..end_index];
        start_index = end_index;

        let token_type = match state {
            TokenizerState::Start => break,
            TokenizerState::LeftParen => TokenType::LeftParen,
            TokenizerState::RightParen => TokenType::RightParen,
            TokenizerState::Number => TokenType::Number(token_string.parse().unwrap()),
            TokenizerState::Symbol => TokenType::Symbol(token_string.into()),
            TokenizerState::Whitespace => continue,
        };

        tokens.push(Token {
            token_type: token_type,
        })
    }

    return tokens;
}

pub struct Parser {
    token_stream: std::iter::Peekable<std::vec::IntoIter<Token>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            token_stream: tokens.into_iter().peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<LispExpr, String> {
        if let Some(token) = self.token_stream.next() {
            match token.token_type {
                TokenType::LeftParen => self.parse_form(),
                TokenType::RightParen => {
                    return Err("Unexpected right paren found.".into());
                }
                TokenType::Number(number) => Ok(LispExpr::Number(number)),
                TokenType::Symbol(ref string) => {
                    let symbol = string.clone();

                    Ok(LispExpr::Symbol(symbol))
                }
            }
        } else {
            return Err("Invalid expression".into());
        }
    }

    fn parse_form(&mut self) -> Result<LispExpr, String> {
        if let Some(_) = self.token_stream.peek() {
            let mut list = Vec::new();

            while let Some(token) = self.token_stream.peek() {
                if token.token_type == TokenType::RightParen {
                    break;
                }

                match self.parse() {
                    Ok(value) => list.push(value),
                    error => return error,
                }
            }

            // Consume the closing right paren from the token stream.
            self.token_stream.next();

            Ok(LispExpr::List(list))
        } else {
            Err("Invalid expression".into())
        }
    }
}

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {}
    }

    pub fn evaluate(&self, ast: LispExpr) -> Result<LispExpr, String> {
        match ast {
            LispExpr::List(values) => match values[0] {
                LispExpr::Symbol(ref symbol) => match &symbol[..] {
                    "+" => {
                        let result = values[1..]
                            .iter()
                            .map(|ast| self.evaluate(ast.clone()))
                            .try_fold(0, |acc, value| match value {
                                Ok(LispExpr::Number(number)) => Ok(acc + number),
                                _ => Err("Invalid + operation".into()),
                            });

                        if let Ok(sum) = result {
                            Ok(LispExpr::Number(sum))
                        } else {
                            Err(result.unwrap_err())
                        }
                    }
                    "-" => {
                        if values.len() == 2 {
                            if let Ok(LispExpr::Number(initial_value)) =
                                self.evaluate(values[1].clone())
                            {
                                return Ok(LispExpr::Number(-initial_value));
                            }
                        }

                        if let Ok(LispExpr::Number(initial_value)) =
                            self.evaluate(values[1].clone())
                        {
                            let result = values[2..]
                                .iter()
                                .map(|ast| self.evaluate(ast.clone()))
                                .try_fold(initial_value, |acc, value| match value {
                                    Ok(LispExpr::Number(number)) => Ok(acc - number),
                                    _ => Err("Invalid - operation".into()),
                                });

                            if let Ok(sum) = result {
                                Ok(LispExpr::Number(sum))
                            } else {
                                Err(result.unwrap_err())
                            }
                        } else {
                            Err("Invalid - operation".into())
                        }
                    }
                    "/" => {
                        if values.len() < 3 {
                            return Err("Invalid / operation".into());
                        }

                        if let Ok(LispExpr::Number(initial_value)) =
                            self.evaluate(values[1].clone())
                        {
                            let result = values[2..]
                                .iter()
                                .map(|ast| self.evaluate(ast.clone()))
                                .try_fold(initial_value, |acc, value| match value {
                                    Ok(LispExpr::Number(number)) => Ok(acc / number),
                                    _ => Err("Invalid / operation".into()),
                                });

                            if let Ok(sum) = result {
                                Ok(LispExpr::Number(sum))
                            } else {
                                Err(result.unwrap_err())
                            }
                        } else {
                            Err("Invalid / operation".into())
                        }
                    }
                    "*" => {
                        if values.len() < 3 {
                            return Err("Invalid * operation".into());
                        }

                        if let Ok(LispExpr::Number(initial_value)) =
                            self.evaluate(values[1].clone())
                        {
                            let result = values[2..]
                                .iter()
                                .map(|ast| self.evaluate(ast.clone()))
                                .try_fold(initial_value, |acc, value| match value {
                                    Ok(LispExpr::Number(number)) => Ok(acc * number),
                                    _ => Err("Invalid * operation".into()),
                                });

                            if let Ok(sum) = result {
                                Ok(LispExpr::Number(sum))
                            } else {
                                Err(result.unwrap_err())
                            }
                        } else {
                            Err("Invalid * operation".into())
                        }
                    }
                    _ => Ok(LispExpr::List(values)),
                },
                _ => Ok(LispExpr::List(values)),
            },
            LispExpr::Number(_) => Ok(ast),
            LispExpr::Symbol(_) => Ok(ast),
        }
    }
}

fn main() {
    let interpreter = Interpreter::new();

    loop {
        println!("lisp> ");

        let mut expr = String::new();

        io::stdin()
            .read_line(&mut expr)
            .expect("Could not read from stdin.");

        let tokens = tokenize(&expr);
        let ast = Parser::new(tokens).parse();

        match ast {
            Ok(ast) => {
                let result = interpreter.evaluate(ast);

                if result.is_ok() {
                    println!("{:?}", result.unwrap());
                } else {
                    println!("ERROR: {}", result.unwrap_err());
                }
            }
            Err(error) => {
                println!("ERROR: {}", error);
            }
        }
    }
}
