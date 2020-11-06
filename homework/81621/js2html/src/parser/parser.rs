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

    fn matches_one_of(&mut self, types: Vec<TokenType>) -> bool {
        for t in types {
            if self.matches(t) {
                return true;
            }
        }

        false
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

    fn binary_expression_helper(
        &mut self,
        lhs: Box<dyn Expression>,
    ) -> Option<Box<dyn Expression>> {
        if self.matches_one_of(vec![TokenType::Plus]) {
            let operator = self.prev().unwrap().r#type;
            let mut identifier = String::new();

            if self.matches(TokenType::Number) {
                let num = self.prev().unwrap().number.unwrap();
                return Some(Box::new(BinaryExpression::new(
                    lhs,
                    operator,
                    Box::new(LiteralNumber::new(num)),
                )));
            } else if self.identifier(&mut identifier) {
                return Some(Box::new(BinaryExpression::new(
                    lhs,
                    operator,
                    Box::new(Identifier::new(identifier)),
                )));
            }
        }

        None
    }

    fn simple_expression(&mut self) -> Option<Box<dyn Expression>> {
        let mut member: Box<dyn Expression> = Box::new(Identifier::new("".to_string()));

        if self.member(&mut member) {
            if let Some(func_args) = self.function_call_args() {
                return Some(Box::new(FunctionCall::new(member, func_args)));
            } else {
                if let Some(bin_expr) = self.binary_expression_helper(member) {
                    return Some(bin_expr);
                }
            }
        }

        if self.matches(TokenType::Number) {
            let num = self.prev().unwrap().number.unwrap();

            if let Some(bin_expr) = self.binary_expression_helper(Box::new(LiteralNumber::new(num)))
            {
                return Some(bin_expr);
            }
        }

        None
    }

    fn function_call_args(&mut self) -> Option<Vec<Box<dyn Expression>>> {
        let mut args: Vec<Box<dyn Expression>> = vec![];

        if !self.matches(TokenType::LParenthesis) {
            return None;
        }

        loop {
            if self.matches(TokenType::RParenthesis) {
                break;
            }

            if let Some(expr) = self.simple_expression() {
                args.push(expr);
            }

            // Consume comma if there is one
            self.matches(TokenType::Comma);
        }

        Some(args)
    }

    fn top_level_expression(&mut self) -> Option<Box<dyn Expression>> {
        let mut res: Option<Box<dyn Expression>> = None;

        if let Some(func_def) = self.function_declaration() {
            res = Some(func_def);
        } else if let Some(var_decl) = self.variable_declaration() {
            res = Some(var_decl);
        } else if let Some(exp) = self.simple_expression() {
            res = Some(exp);
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
