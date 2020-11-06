use crate::lexer::token_type::TokenType;
use crate::lexer::tokenizer::Token;
use crate::parser::expression::*;

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

    pub fn parse(&mut self) -> TopLevelExpressions {
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

    fn member(&mut self, member: &mut Box<dyn Expression>) -> bool {
        let mut object_name = String::new();

        if self.identifier(&mut object_name) {
            let mut object: Box<dyn Expression> = Box::new(Identifier::new(object_name));
            let mut property_name = String::new();

            if self.matches(TokenType::Dot) {
                loop {
                    if self.identifier(&mut property_name) {
                        let property = Identifier::new(property_name.clone());
                        object = Box::new(Member::new(object, property));
                    }

                    if !self.matches(TokenType::Dot) {
                        *member = object;
                        return true;
                    }
                }
            } else {
                *member = object;
                return true;
            }
        }

        false
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

    fn function_declaration(&mut self) -> Option<Box<FunctionDeclaration>> {
        if self.matches(TokenType::Function) {
            let mut function_name: String = String::new();

            if self.identifier(&mut function_name) {
                let mut args = Vec::new();

                if self.parenthesized_args_defintion(&mut args) {
                    if self.matches(TokenType::LCurlyBrace) {
                        let body = self.top_level_expressions();

                        if self.matches(TokenType::RCurlyBrace) {
                            return Some(Box::new(FunctionDeclaration::new(
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

    fn variable_declaration(&mut self) -> Option<Box<VariableDeclaration>> {
        if self.matches(TokenType::Var) || self.matches(TokenType::Let) {
            let mut identifier = String::new();

            if self.identifier(&mut identifier) {
                if self.matches(TokenType::Assign) {
                    if self.matches(TokenType::Number) {
                        let number = self.prev().unwrap().number.unwrap();

                        return Some(Box::new(VariableDeclaration::new(identifier, Some(number))));
                    }
                }
                if self.matches(TokenType::Semicolon) {
                    return Some(Box::new(VariableDeclaration::new(identifier, None)));
                }
            }
        }

        None
    }

    fn function_call(&mut self) -> Option<Box<FunctionCall>> {
        let mut member: Box<dyn Expression> = Box::new(Identifier::new("".to_string()));

        if self.member(&mut member) {
            if self.matches(TokenType::LParenthesis) {
                let mut arg = String::new();

                if self.identifier(&mut arg) {
                    if self.matches(TokenType::RParenthesis) {
                        return Some(Box::new(FunctionCall::new(member, vec![arg])));
                    }
                }
            }
        }

        None
    }

    fn top_level_expression(&mut self) -> Option<Box<dyn Expression>> {
        let mut res: Option<Box<dyn Expression>> = None;

        if let Some(func_def) = self.function_declaration() {
            res = Some(func_def);
        }

        if let Some(var_decl) = self.variable_declaration() {
            res = Some(var_decl);
        }

        if let Some(funct_call) = self.function_call() {
            res = Some(funct_call);
        }

        // Consume semicolon if there is one
        self.matches(TokenType::Semicolon);

        res
    }

    fn top_level_expressions(&mut self) -> TopLevelExpressions {
        let mut statements: Vec<Box<dyn Expression>> = vec![];

        while let Some(expr) = self.top_level_expression() {
            statements.push(expr);
        }

        TopLevelExpressions::new(statements)
    }
}
