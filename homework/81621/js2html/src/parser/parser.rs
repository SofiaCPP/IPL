use crate::lexer::token_type::TokenType;
use crate::lexer::tokenizer::Token;
use crate::parser::expression::*;

use std::rc::Rc;

pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    pos: u32,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.pos as usize)
    }

    fn prev(&self) -> Option<&Token> {
        self.tokens.get((self.pos - 1) as usize)
    }

    pub fn parse(&mut self) -> Rc<TopLevelExpression> {
        self.top_level_expression()
    }

    fn matches(&mut self, token_type: TokenType) -> bool {
        if let Some(token) = self.current() {
            if token.r#type == token_type {
                self.pos += 1;
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn identifier(&mut self, identifier: &mut String) -> bool {
        if !self.matches(TokenType::Identifier) {
            return false;
        }

        *identifier = self.prev().unwrap().clone().string.unwrap();
        true
    }

    fn parenthesized_args_defintion(&mut self, args: &mut Vec<String>) -> bool {
        args.clear();

        if !self.matches(TokenType::LParenthesis) {
            return false;
        }

        let mut identifier: String = String::new();

        while self.identifier(&mut identifier) {
            args.push(identifier.clone());
        }

        self.matches(TokenType::RParenthesis)
    }

    fn function_definition(&mut self) -> Option<Rc<FunctionExpression>> {
        if self.matches(TokenType::Function) {
            let mut function_name: String = String::new();

            if self.identifier(&mut function_name) {
                let mut args = Vec::new();

                if self.parenthesized_args_defintion(&mut args) {
                    if self.matches(TokenType::LCurlyBrace) {
                        if self.matches(TokenType::RCurlyBrace) {
                            return Some(Rc::new(FunctionExpression::new(function_name, args)));
                        }
                    }
                }
            }
        }

        None
    }

    fn top_level_expression(&mut self) -> Rc<TopLevelExpression> {
        let mut statements: Vec<Rc<dyn Expression>> = vec![];

        if let Some(def) = self.function_definition() {
            statements.push(def);
        }

        Rc::new(TopLevelExpression::new(statements))
    }
}
