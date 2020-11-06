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

    pub fn parse(&mut self) -> Rc<TopLevelExpressions> {
        self.top_level_expressions()
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

    fn function_declaration(&mut self) -> Option<Rc<FunctionDeclaration>> {
        if self.matches(TokenType::Function) {
            let mut function_name: String = String::new();

            if self.identifier(&mut function_name) {
                let mut args = Vec::new();

                if self.parenthesized_args_defintion(&mut args) {
                    if self.matches(TokenType::LCurlyBrace) {
                        let body = self.top_level_expressions();

                        if self.matches(TokenType::RCurlyBrace) {
                            return Some(Rc::new(FunctionDeclaration::new(
                                function_name,
                                args,
                                body,
                            )));
                        }
                    }
                }
            }
        }

        None
    }

    fn variable_declaration(&mut self) -> Option<Rc<VariableDeclaration>> {
        if self.matches(TokenType::Var) || self.matches(TokenType::Let) {
            let mut identifier = String::new();

            if self.identifier(&mut identifier) {
                if self.matches(TokenType::Assign) {
                    if self.matches(TokenType::Number) {
                        let number = self.prev().unwrap().number.unwrap();

                        if self.matches(TokenType::Semicolon) {
                            return Some(Rc::new(VariableDeclaration::new(
                                identifier,
                                Some(number),
                            )));
                        }
                    }
                }
                if self.matches(TokenType::Semicolon) {
                    return Some(Rc::new(VariableDeclaration::new(identifier, None)));
                }
            }
        }

        None
    }

    fn top_level_expression(&mut self) -> Option<Rc<dyn Expression>> {
        if let Some(func_def) = self.function_declaration() {
            return Some(func_def);
        }

        if let Some(var_decl) = self.variable_declaration() {
            return Some(var_decl);
        }

        None
    }

    fn top_level_expressions(&mut self) -> Rc<TopLevelExpressions> {
        let mut statements: Vec<Rc<dyn Expression>> = vec![];

        while let Some(expr) = self.top_level_expression() {
            statements.push(expr);
        }

        Rc::new(TopLevelExpressions::new(statements))
    }
}
