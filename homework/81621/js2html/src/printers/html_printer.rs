use crate::lexer::keywords_parser::KEYWORDS;
use crate::lexer::token_type::TokenType;
use crate::lexer::tokenizer::Token;
use htmlescape::*;

pub fn tokens_to_html(tokens: Vec<Token>) -> String {
    let mut html: String = "<body>".to_string();

    for token in tokens {
        let formatted_string = match token.r#type {
            TokenType::String => format!(
                "<span style='color:green'>{}</span>",
                &token.get_escaped_string().unwrap()
            ),
            TokenType::Number => {
                format!("<span style='color:blue'>{}</span>", &token.number.unwrap())
            }
            TokenType::LParenthesis | TokenType::RParenthesis => format!(
                "<span style='color:purple'>{}</span>",
                &token.get_escaped_string().unwrap()
            ),
            TokenType::LCurlyBrace | TokenType::RCurlyBrace => format!(
                "<span style='color:orange'>{}</span>",
                &token.get_escaped_string().unwrap()
            ),
            TokenType::Whitespace => token
                .string
                .unwrap()
                .replace(" ", "&ensp;")
                .replace("\t", "&ensp;&ensp;&ensp;&ensp;"),
            TokenType::Unknown => format!(
                "<span style='border-bottom: 1px dotted #ff0000;'>{}</span>",
                &token.get_escaped_string().unwrap()
            ),
            TokenType::Comment => format!(
                "<span style='color:gray'>{}</span>",
                &token.get_escaped_string().unwrap()
            ),
            _ => {
                // If the token is a keyword, print it in red
                if KEYWORDS.values().any(|x| *x == token.r#type) {
                    format!(
                        "<span style='color:red'>{}</span>",
                        &token.get_escaped_string().unwrap()
                    )
                } else {
                    // Print unmatched tokens as just their string
                    encode_minimal(&token.string.unwrap())
                }
            }
        };

        html.push_str(&formatted_string);
    }

    html.push_str("</body>");
    html.replace("\n", "<br/>")
}
