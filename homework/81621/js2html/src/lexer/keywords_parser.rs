use super::token_type::TokenType;
use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    pub static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut h = HashMap::new();
        h.insert("abstract", TokenType::Abstract);
        h.insert("arguments", TokenType::Arguments);
        h.insert("await", TokenType::Await);
        h.insert("boolean", TokenType::Boolean);
        h.insert("break", TokenType::Break);
        h.insert("byte", TokenType::Byte);
        h.insert("case", TokenType::Case);
        h.insert("catch", TokenType::Catch);
        h.insert("char", TokenType::Char);
        h.insert("class", TokenType::Class);
        h.insert("const", TokenType::Const);
        h.insert("continue", TokenType::Continue);
        h.insert("debugger", TokenType::Debugger);
        h.insert("default", TokenType::Default);
        h.insert("delete", TokenType::Delete);
        h.insert("do", TokenType::Do);
        h.insert("double", TokenType::Double);
        h.insert("else", TokenType::Else);
        h.insert("enum", TokenType::Enum);
        h.insert("export", TokenType::Export);
        h.insert("extends", TokenType::Extends);
        h.insert("false", TokenType::False);
        h.insert("final", TokenType::Final);
        h.insert("finally", TokenType::Finally);
        h.insert("float", TokenType::Float);
        h.insert("for", TokenType::For);
        h.insert("function", TokenType::Function);
        h.insert("get", TokenType::Get);
        h.insert("goto", TokenType::Goto);
        h.insert("if", TokenType::If);
        h.insert("implements", TokenType::Implements);
        h.insert("import", TokenType::Import);
        h.insert("in", TokenType::In);
        h.insert("int", TokenType::Int);
        h.insert("interface", TokenType::Interface);
        h.insert("instanceof", TokenType::Instanceof);
        h.insert("let", TokenType::Let);
        h.insert("long", TokenType::Long);
        h.insert("native", TokenType::Native);
        h.insert("new", TokenType::New);
        h.insert("null", TokenType::Null);
        h.insert("package", TokenType::Package);
        h.insert("private", TokenType::Private);
        h.insert("protected", TokenType::Protected);
        h.insert("public", TokenType::Public);
        h.insert("return", TokenType::Return);
        h.insert("set", TokenType::Set);
        h.insert("short", TokenType::Short);
        h.insert("static", TokenType::Static);
        h.insert("super", TokenType::Super);
        h.insert("switch", TokenType::Switch);
        h.insert("synchronized", TokenType::Synchronized);
        h.insert("this", TokenType::This);
        h.insert("throw", TokenType::Throw);
        h.insert("throws", TokenType::Throws);
        h.insert("transient", TokenType::Transient);
        h.insert("true", TokenType::True);
        h.insert("try", TokenType::Try);
        h.insert("typeof", TokenType::Typeof);
        h.insert("var", TokenType::Var);
        h.insert("void", TokenType::Void);
        h.insert("volatile", TokenType::Volatile);
        h.insert("while", TokenType::While);
        h.insert("with", TokenType::With);
        h.insert("yield", TokenType::Yield);
        h
    };
}

pub fn check_matches_keyword(word: &str) -> Option<TokenType> {
    KEYWORDS.get(word).copied()
}
