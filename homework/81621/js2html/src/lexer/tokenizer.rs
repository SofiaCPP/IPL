use super::keywords_parser::check_matches_keyword;
use super::token_type::TokenType;
use htmlescape::encode_minimal;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub r#type: TokenType,
    pub number: Option<f64>,
    pub string: Option<String>,
}

impl Token {
    pub fn get_escaped_string(&self) -> Option<String> {
        self.string.clone().map(|s| encode_minimal(&s))
    }
}

pub struct Tokenizer {
    pub input: String,
    pub tokens: Vec<Token>,
}

impl Tokenizer {
    pub fn new(input_string: String) -> Self {
        Tokenizer {
            input: input_string,
            tokens: Vec::new(),
        }
    }

    pub fn populate_tokens(&mut self) {
        let mut chars = self.input.chars().peekable();

        while let Some(character) = chars.peek() {
            if character.is_alphabetic() {
                let word = Tokenizer::read_word(&mut chars);
                let keyword_match = check_matches_keyword(&word);

                if let Some(keyword_token_type) = keyword_match {
                    self.tokens.push(Token {
                        r#type: keyword_token_type,
                        string: Some(word),
                        number: None,
                    });
                } else {
                    self.tokens.push(Token {
                        r#type: TokenType::Identifier,
                        string: Some(word),
                        number: None,
                    });
                }
            } else if *character == '"' {
                let string = Tokenizer::read_string(&mut chars);

                self.tokens.push(Token {
                    r#type: TokenType::String,
                    string: Some(string),
                    number: None,
                });
            } else if character.is_numeric() {
                let number = Tokenizer::read_number(&mut chars);

                self.tokens.push(Token {
                    r#type: TokenType::Number,
                    string: None,
                    number: Some(number),
                });
            } else if *character == '(' {
                self.tokens.push(Token {
                    r#type: TokenType::LParenthesis,
                    string: Some(character.to_string()),
                    number: None,
                });

                chars.next();
            } else if *character == ')' {
                self.tokens.push(Token {
                    r#type: TokenType::RParenthesis,
                    string: Some(character.to_string()),
                    number: None,
                });

                chars.next();
            } else if *character == '{' {
                self.tokens.push(Token {
                    r#type: TokenType::LCurlyBrace,
                    string: Some(character.to_string()),
                    number: None,
                });

                chars.next();
            } else if *character == '}' {
                self.tokens.push(Token {
                    r#type: TokenType::RCurlyBrace,
                    string: Some(character.to_string()),
                    number: None,
                });

                chars.next();
            } else if *character == '[' {
                self.tokens.push(Token {
                    r#type: TokenType::LSquareBrace,
                    string: Some(character.to_string()),
                    number: None,
                });

                chars.next();
            } else if *character == ']' {
                self.tokens.push(Token {
                    r#type: TokenType::RSquareBrace,
                    string: Some(character.to_string()),
                    number: None,
                });

                chars.next();
            } else if *character == ';' {
                self.tokens.push(Token {
                    r#type: TokenType::Semicolon,
                    string: Some(character.to_string()),
                    number: None,
                });

                chars.next();
            } else if *character == ',' {
                self.tokens.push(Token {
                    r#type: TokenType::Comma,
                    string: Some(character.to_string()),
                    number: None,
                });

                chars.next();
            } else if *character == '_' {
                self.tokens.push(Token {
                    r#type: TokenType::Underscore,
                    string: Some(character.to_string()),
                    number: None,
                });

                chars.next();
            } else if *character == ':' {
                self.tokens.push(Token {
                    r#type: TokenType::Colon,
                    string: Some(character.to_string()),
                    number: None,
                });

                chars.next();
            } else if *character == '-' {
                let char_string = character.to_string();
                chars.next();

                self.tokens.push(Token {
                    r#type: TokenType::Minus,
                    string: Some(char_string),
                    number: None,
                });
            } else if *character == '\'' {
                let char_string = Tokenizer::read_char_string(&mut chars);

                self.tokens.push(Token {
                    r#type: TokenType::CharString,
                    string: Some(char_string),
                    number: None,
                });
            } else if *character == '+' {
                self.tokens.push(Token {
                    r#type: TokenType::Plus,
                    string: Some(character.to_string()),
                    number: None,
                });

                chars.next();
            } else if *character == '*' {
                self.tokens.push(Token {
                    r#type: TokenType::Multiply,
                    string: Some(character.to_string()),
                    number: None,
                });

                chars.next();
            } else if *character == '/' {
                let char_string = character.to_string();
                chars.next();

                if let Some(next_char) = chars.peek() {
                    if *next_char == '/' {
                        let comment: String = Tokenizer::read_single_line_comment(&mut chars);

                        self.tokens.push(Token {
                            r#type: TokenType::Comment,
                            string: Some(comment),
                            number: None,
                        });

                        continue;
                    }
                }

                self.tokens.push(Token {
                    r#type: TokenType::Divide,
                    string: Some(char_string),
                    number: None,
                });
            } else if *character == '.' {
                self.tokens.push(Token {
                    r#type: TokenType::Dot,
                    string: Some(character.to_string()),
                    number: None,
                });

                chars.next();
            } else if *character == '&' {
                let char_string = character.to_string();
                chars.next();

                if let Some(next_char) = chars.peek() {
                    if *next_char == '&' {
                        self.tokens.push(Token {
                            r#type: TokenType::And,
                            string: Some("&&".to_string()),
                            number: None,
                        });

                        chars.next();
                        continue;
                    }
                }

                self.tokens.push(Token {
                    r#type: TokenType::BitwiseAnd,
                    string: Some(char_string),
                    number: None,
                });
            } else if *character == '|' {
                let char_string = character.to_string();
                chars.next();

                if let Some(next_char) = chars.peek() {
                    if *next_char == '|' {
                        self.tokens.push(Token {
                            r#type: TokenType::Or,
                            string: Some("||".to_string()),
                            number: None,
                        });

                        chars.next();
                        continue;
                    }
                }

                self.tokens.push(Token {
                    r#type: TokenType::BitwiseOr,
                    string: Some(char_string),
                    number: None,
                });
            } else if *character == '^' {
                let char_string = character.to_string();
                chars.next();

                if let Some(next_char) = chars.peek() {
                    if *next_char == '^' {
                        self.tokens.push(Token {
                            r#type: TokenType::Xor,
                            string: Some("^^".to_string()),
                            number: None,
                        });

                        chars.next();
                        continue;
                    }
                }

                self.tokens.push(Token {
                    r#type: TokenType::BitwiseXor,
                    string: Some(char_string),
                    number: None,
                });
            } else if *character == '%' {
                self.tokens.push(Token {
                    r#type: TokenType::Modulus,
                    string: Some(character.to_string()),
                    number: None,
                });

                chars.next();
            } else if *character == '%' {
                self.tokens.push(Token {
                    r#type: TokenType::Modulus,
                    string: Some(character.to_string()),
                    number: None,
                });

                chars.next();
            } else if *character == '=' {
                let char_string = character.to_string();
                chars.next();

                if let Some(next_char) = chars.peek() {
                    if *next_char == '>' {
                        self.tokens.push(Token {
                            r#type: TokenType::Arrow,
                            string: Some("=>".to_string()),
                            number: None,
                        });

                        chars.next();
                        continue;
                    } else if *next_char == '=' {
                        self.tokens.push(Token {
                            r#type: TokenType::Equals,
                            string: Some("==".to_string()),
                            number: None,
                        });

                        chars.next();
                        continue;
                    }
                }

                self.tokens.push(Token {
                    r#type: TokenType::Assign,
                    string: Some(char_string),
                    number: None,
                });
            } else if *character == '!' {
                let char_string = character.to_string();
                chars.next();

                if let Some(next_char) = chars.peek() {
                    if *next_char == '=' {
                        self.tokens.push(Token {
                            r#type: TokenType::NotEquals,
                            string: Some("!=".to_string()),
                            number: None,
                        });

                        chars.next();
                        continue;
                    }
                }

                self.tokens.push(Token {
                    r#type: TokenType::Negate,
                    string: Some(char_string),
                    number: None,
                });
            } else if *character == '<' {
                let char_string = character.to_string();
                chars.next();

                if let Some(next_char) = chars.peek() {
                    if *next_char == '=' {
                        self.tokens.push(Token {
                            r#type: TokenType::LessOrEqual,
                            string: Some("<=".to_string()),
                            number: None,
                        });

                        chars.next();
                        continue;
                    }
                }

                self.tokens.push(Token {
                    r#type: TokenType::Less,
                    string: Some(char_string),
                    number: None,
                });
            } else if *character == '>' {
                let char_string = character.to_string();
                chars.next();

                if let Some(next_char) = chars.peek() {
                    if *next_char == '=' {
                        self.tokens.push(Token {
                            r#type: TokenType::MoreOrEqual,
                            string: Some(">=".to_string()),
                            number: None,
                        });

                        chars.next();
                        continue;
                    }
                }

                self.tokens.push(Token {
                    r#type: TokenType::More,
                    string: Some(char_string),
                    number: None,
                });
            } else if character.is_whitespace() {
                let whitespace = Tokenizer::read_whitespace(&mut chars);

                self.tokens.push(Token {
                    r#type: TokenType::Whitespace,
                    string: Some(whitespace.to_string()),
                    number: None,
                });
            } else {
                self.tokens.push(Token {
                    r#type: TokenType::Unknown,
                    string: Some(character.to_string()),
                    number: None,
                });

                chars.next();
            }
        }
    }

    fn read_word(chars: &mut Peekable<Chars>) -> String {
        let mut word = String::new();

        while let Some(peeked_character) = chars.peek() {
            if peeked_character.is_alphanumeric() || *peeked_character == '_' {
                word.push(chars.next().unwrap());
            } else {
                break;
            }
        }

        word
    }

    fn read_number(chars: &mut Peekable<Chars>) -> f64 {
        let mut number: f64 = 0f64;
        let mut positions_past_dot = 0;

        while let Some(peeked_character) = chars.peek() {
            if peeked_character.is_numeric() {
                let char_as_num = peeked_character.to_digit(10).unwrap() as f64;

                if positions_past_dot > 0 {
                    number += char_as_num / 10f64.powi(positions_past_dot);
                    positions_past_dot += 1;
                } else {
                    number *= 10f64;
                    number += char_as_num as f64;
                }

                chars.next();
            } else if *peeked_character == '.' {
                positions_past_dot = 1;
                chars.next();
            } else {
                break;
            }
        }

        number
    }

    fn read_char_string(chars: &mut Peekable<Chars>) -> String {
        let mut char_string = String::new();
        chars.next();

        let mut escape_next: bool = false;

        while let Some(peeked_character) = chars.peek() {
            if !escape_next {
                if *peeked_character == '\\' {
                    escape_next = true;
                    chars.next();
                    continue;
                }

                if *peeked_character == '\'' {
                    break;
                }
            } else {
                escape_next = false;
            }

            char_string.push(chars.next().unwrap());
        }

        chars.next();
        format!("'{}'", char_string)
    }

    fn read_string(chars: &mut Peekable<Chars>) -> String {
        let mut string = String::new();
        chars.next();

        let mut escape_next: bool = false;

        while let Some(peeked_character) = chars.peek() {
            if !escape_next {
                if *peeked_character == '\\' {
                    escape_next = true;
                    chars.next();
                    continue;
                }

                if *peeked_character == '\"' {
                    break;
                }
            } else {
                escape_next = false;
            }

            string.push(chars.next().unwrap());
        }

        chars.next();
        format!("\"{}\"", string)
    }

    fn read_whitespace(chars: &mut Peekable<Chars>) -> String {
        let mut word = String::new();

        while let Some(peeked_character) = chars.peek() {
            if peeked_character.is_whitespace() {
                word.push(chars.next().unwrap());
            } else {
                break;
            }
        }

        word
    }

    fn read_single_line_comment(chars: &mut Peekable<Chars>) -> String {
        // Assume the first / was already read from chars
        // and the first char in chars is // for sure.
        let mut comment = "//".to_string();
        chars.next();

        while let Some(peeked_character) = chars.peek() {
            if *peeked_character == '\n' {
                break;
            } else {
                comment.push(chars.next().unwrap());
            }
        }

        comment
    }
}
