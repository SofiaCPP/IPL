use super::token_type::TokenType;
use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    pub static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut h = HashMap::new();
        h.insert("abstract", TokenType::Abstract);
        h.insert("as", TokenType::As);
        h.insert("async", TokenType::Async);
        h.insert("await", TokenType::Await);
        h.insert("become", TokenType::Become);
        h.insert("box", TokenType::Box);
        h.insert("break", TokenType::Break);
        h.insert("const", TokenType::Const);
        h.insert("continue", TokenType::Continue);
        h.insert("crate", TokenType::Crate);
        h.insert("do", TokenType::Do);
        h.insert("dyn", TokenType::Dyn);
        h.insert("else", TokenType::Else);
        h.insert("enum", TokenType::Enum);
        h.insert("extern", TokenType::Extern);
        h.insert("false", TokenType::False);
        h.insert("final", TokenType::Final);
        h.insert("fn", TokenType::Fn);
        h.insert("for", TokenType::For);
        h.insert("if", TokenType::If);
        h.insert("impl", TokenType::Impl);
        h.insert("in", TokenType::In);
        h.insert("let", TokenType::Let);
        h.insert("loop", TokenType::Loop);
        h.insert("macro", TokenType::Macro);
        h.insert("match", TokenType::Match);
        h.insert("mod", TokenType::Mod);
        h.insert("move", TokenType::Move);
        h.insert("mut", TokenType::Mut);
        h.insert("override", TokenType::Override);
        h.insert("private", TokenType::Private);
        h.insert("pub", TokenType::Pub);
        h.insert("ref", TokenType::Ref);
        h.insert("return", TokenType::Return);
        h.insert("self", TokenType::SelfValue);
        h.insert("Self", TokenType::SelfType);
        h.insert("static", TokenType::Static);
        h.insert("struct", TokenType::Struct);
        h.insert("super", TokenType::Super);
        h.insert("trait", TokenType::Trait);
        h.insert("true", TokenType::True);
        h.insert("try", TokenType::Try);
        h.insert("type", TokenType::Type);
        h.insert("typeof", TokenType::Typeof);
        h.insert("unsafe", TokenType::Unsafe);
        h.insert("unsized", TokenType::Unsized);
        h.insert("use", TokenType::Use);
        h.insert("virtual", TokenType::Virtual);
        h.insert("where", TokenType::Where);
        h.insert("while", TokenType::While);
        h.insert("yield", TokenType::Yield);
        h.insert("union", TokenType::Union);
        h
    };
}

pub fn check_matches_keyword(word: &str) -> Option<TokenType> {
    KEYWORDS.get(word).copied()
}
