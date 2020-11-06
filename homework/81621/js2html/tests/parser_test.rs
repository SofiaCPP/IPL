#[cfg(test)]
mod tests {
    use js2html::lexer::token_type::TokenType;
    use js2html::lexer::tokenizer::Token;
    use js2html::parser::expression::*;
    use js2html::parser::parser::Parser;
    use std::rc::Rc;

    #[test]
    fn test_empty_function() {
        let input_tokens = vec![
            Token {
                r#type: TokenType::Function,
                number: None,
                string: Some("function".to_string()),
            },
            Token {
                r#type: TokenType::Identifier,
                number: None,
                string: Some("foo".to_string()),
            },
            Token {
                r#type: TokenType::LParenthesis,
                number: None,
                string: Some("(".to_string()),
            },
            Token {
                r#type: TokenType::RParenthesis,
                number: None,
                string: Some(")".to_string()),
            },
            Token {
                r#type: TokenType::LCurlyBrace,
                number: None,
                string: Some("{".to_string()),
            },
            Token {
                r#type: TokenType::RCurlyBrace,
                number: None,
                string: Some("}".to_string()),
            },
        ];

        let mut parser = Parser::new(&input_tokens);
        let top_level_expression = parser.parse();
        let expected_expression = Rc::new(TopLevelExpressions::new(vec![Rc::new(
            FunctionDeclaration::new(
                "foo".to_string(),
                vec![],
                Rc::new(TopLevelExpressions::new(vec![])),
            ),
        )]));
        assert_eq!(top_level_expression, expected_expression);
    }
}
