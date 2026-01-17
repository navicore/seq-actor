//! Lexer for seq-actor
//!
//! Tokenizes source text into a stream of tokens.

use crate::ast::Span;
use std::iter::Peekable;
use std::str::Chars;

/// Token types
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Literals
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),

    // Identifiers and keywords
    Ident(String),
    TypeIdent(String), // Capitalized identifiers

    // Keywords
    Let,
    If,
    Then,
    Else,
    Match,
    With,
    Type,
    Include,
    Receive,
    After,
    End,
    Become,
    Spawn,
    Send,
    SelfKw,
    And,
    Or,
    Not,
    True,
    False,
    Copy,
    Std,
    Ffi,
    Supervised,
    OneForOne,
    OneForAll,
    RestForOne,

    // Delimiters
    Colon,      // :
    Semicolon,  // ;
    Comma,      // ,
    Dot,        // .
    Pipe,       // |
    Arrow,      // ->
    FatArrow,   // =>
    LParen,     // (
    RParen,     // )
    LBrace,     // {
    RBrace,     // }
    LBracket,   // [
    RBracket,   // ]

    // Operators
    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    Percent,    // %
    Eq,         // =
    EqEq,       // ==
    BangEq,     // !=
    Lt,         // <
    LtEq,       // <=
    Gt,         // >
    GtEq,       // >=
    Underscore, // _

    // Special
    Eof,
    Error(String),
}

impl TokenKind {
    pub fn is_eof(&self) -> bool {
        matches!(self, TokenKind::Eof)
    }
}

/// A token with its span
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// Lexer state
pub struct Lexer<'a> {
    source: &'a str,
    chars: Peekable<Chars<'a>>,
    pos: usize,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars().peekable(),
            pos: 0,
            line: 1,
            column: 1,
        }
    }

    /// Tokenize the entire source
    pub fn tokenize(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            let is_eof = token.kind.is_eof();
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        tokens
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.pos += c.len_utf8();
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(c)
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.advance();
            } else if c == '#' {
                // Skip comment to end of line
                while let Some(c) = self.peek() {
                    if c == '\n' {
                        break;
                    }
                    self.advance();
                }
            } else {
                break;
            }
        }
    }

    fn make_span(&self, start_pos: usize, start_line: usize, start_col: usize) -> Span {
        Span::new(start_pos, self.pos, start_line, start_col)
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let start_pos = self.pos;
        let start_line = self.line;
        let start_col = self.column;

        let Some(c) = self.advance() else {
            return Token::new(TokenKind::Eof, self.make_span(start_pos, start_line, start_col));
        };

        let kind = match c {
            // Single-char tokens
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            ':' => TokenKind::Colon,
            ';' => TokenKind::Semicolon,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            '|' => TokenKind::Pipe,
            '+' => TokenKind::Plus,
            '*' => TokenKind::Star,
            '/' => TokenKind::Slash,
            '%' => TokenKind::Percent,
            '_' => {
                // Could be underscore or start of identifier
                if self.peek().map_or(true, |c| !c.is_alphanumeric() && c != '_') {
                    TokenKind::Underscore
                } else {
                    self.read_ident_from('_')
                }
            }

            // Multi-char tokens
            '-' => {
                if self.peek() == Some('>') {
                    self.advance();
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                }
            }
            '=' => {
                if self.peek() == Some('=') {
                    self.advance();
                    TokenKind::EqEq
                } else if self.peek() == Some('>') {
                    self.advance();
                    TokenKind::FatArrow
                } else {
                    TokenKind::Eq
                }
            }
            '!' => {
                if self.peek() == Some('=') {
                    self.advance();
                    TokenKind::BangEq
                } else {
                    TokenKind::Error(format!("Unexpected character '!'"))
                }
            }
            '<' => {
                if self.peek() == Some('=') {
                    self.advance();
                    TokenKind::LtEq
                } else {
                    TokenKind::Lt
                }
            }
            '>' => {
                if self.peek() == Some('=') {
                    self.advance();
                    TokenKind::GtEq
                } else {
                    TokenKind::Gt
                }
            }

            // Strings
            '"' => self.read_string(),

            // Numbers
            c if c.is_ascii_digit() => self.read_number(c),

            // Identifiers and keywords
            c if c.is_alphabetic() || c == '_' => self.read_ident_from(c),

            _ => TokenKind::Error(format!("Unexpected character '{}'", c)),
        };

        Token::new(kind, self.make_span(start_pos, start_line, start_col))
    }

    fn read_string(&mut self) -> TokenKind {
        let mut value = String::new();
        loop {
            match self.peek() {
                Some('"') => {
                    self.advance();
                    break;
                }
                Some('\\') => {
                    self.advance();
                    match self.advance() {
                        Some('n') => value.push('\n'),
                        Some('t') => value.push('\t'),
                        Some('r') => value.push('\r'),
                        Some('\\') => value.push('\\'),
                        Some('"') => value.push('"'),
                        Some(c) => return TokenKind::Error(format!("Invalid escape sequence \\{}", c)),
                        None => return TokenKind::Error("Unterminated string".to_string()),
                    }
                }
                Some(c) => {
                    self.advance();
                    value.push(c);
                }
                None => return TokenKind::Error("Unterminated string".to_string()),
            }
        }
        TokenKind::String(value)
    }

    fn read_number(&mut self, first: char) -> TokenKind {
        let mut num_str = String::from(first);
        let mut is_float = false;

        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                num_str.push(c);
                self.advance();
            } else if c == '.' && !is_float {
                // Look ahead to distinguish 1.0 from 1.method()
                let mut lookahead = self.chars.clone();
                lookahead.next(); // skip the '.'
                if lookahead.peek().map_or(false, |c| c.is_ascii_digit()) {
                    is_float = true;
                    num_str.push(c);
                    self.advance();
                } else {
                    break;
                }
            } else if c == '_' {
                // Allow underscores in numbers for readability
                self.advance();
            } else {
                break;
            }
        }

        if is_float {
            match num_str.parse::<f64>() {
                Ok(n) => TokenKind::Float(n),
                Err(e) => TokenKind::Error(format!("Invalid float: {}", e)),
            }
        } else {
            match num_str.parse::<i64>() {
                Ok(n) => TokenKind::Int(n),
                Err(e) => TokenKind::Error(format!("Invalid integer: {}", e)),
            }
        }
    }

    fn read_ident_from(&mut self, first: char) -> TokenKind {
        let mut ident = String::from(first);

        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                self.advance();
            } else {
                break;
            }
        }

        // Check for keywords
        match ident.as_str() {
            "let" => TokenKind::Let,
            "if" => TokenKind::If,
            "then" => TokenKind::Then,
            "else" => TokenKind::Else,
            "match" => TokenKind::Match,
            "with" => TokenKind::With,
            "type" => TokenKind::Type,
            "include" => TokenKind::Include,
            "receive" => TokenKind::Receive,
            "after" => TokenKind::After,
            "end" => TokenKind::End,
            "become" => TokenKind::Become,
            "spawn" => TokenKind::Spawn,
            "send" => TokenKind::Send,
            "self" => TokenKind::SelfKw,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "not" => TokenKind::Not,
            "true" => TokenKind::Bool(true),
            "false" => TokenKind::Bool(false),
            "copy" => TokenKind::Copy,
            "std" => TokenKind::Std,
            "ffi" => TokenKind::Ffi,
            "supervised" => TokenKind::Supervised,
            "one_for_one" => TokenKind::OneForOne,
            "one_for_all" => TokenKind::OneForAll,
            "rest_for_one" => TokenKind::RestForOne,
            _ => {
                // Capitalize idents are type names
                if ident.chars().next().map_or(false, |c| c.is_uppercase()) {
                    TokenKind::TypeIdent(ident)
                } else {
                    TokenKind::Ident(ident)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(source: &str) -> Vec<TokenKind> {
        Lexer::new(source)
            .tokenize()
            .into_iter()
            .map(|t| t.kind)
            .collect()
    }

    #[test]
    fn test_basic_tokens() {
        assert_eq!(
            lex(": + - * / ;"),
            vec![
                TokenKind::Colon,
                TokenKind::Plus,
                TokenKind::Minus,
                TokenKind::Star,
                TokenKind::Slash,
                TokenKind::Semicolon,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_numbers() {
        assert_eq!(
            lex("42 3.14 1_000"),
            vec![
                TokenKind::Int(42),
                TokenKind::Float(3.14),
                TokenKind::Int(1000),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_strings() {
        assert_eq!(
            lex(r#""hello" "world\n""#),
            vec![
                TokenKind::String("hello".to_string()),
                TokenKind::String("world\n".to_string()),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_identifiers() {
        assert_eq!(
            lex("foo bar_baz Int Option"),
            vec![
                TokenKind::Ident("foo".to_string()),
                TokenKind::Ident("bar_baz".to_string()),
                TokenKind::TypeIdent("Int".to_string()),
                TokenKind::TypeIdent("Option".to_string()),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_keywords() {
        assert_eq!(
            lex("let if else match receive spawn become"),
            vec![
                TokenKind::Let,
                TokenKind::If,
                TokenKind::Else,
                TokenKind::Match,
                TokenKind::Receive,
                TokenKind::Spawn,
                TokenKind::Become,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_operators() {
        assert_eq!(
            lex("-> == != <= >= | and or not"),
            vec![
                TokenKind::Arrow,
                TokenKind::EqEq,
                TokenKind::BangEq,
                TokenKind::LtEq,
                TokenKind::GtEq,
                TokenKind::Pipe,
                TokenKind::And,
                TokenKind::Or,
                TokenKind::Not,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_function_def() {
        assert_eq!(
            lex(": double (x) -> x * 2 ;"),
            vec![
                TokenKind::Colon,
                TokenKind::Ident("double".to_string()),
                TokenKind::LParen,
                TokenKind::Ident("x".to_string()),
                TokenKind::RParen,
                TokenKind::Arrow,
                TokenKind::Ident("x".to_string()),
                TokenKind::Star,
                TokenKind::Int(2),
                TokenKind::Semicolon,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_comments() {
        assert_eq!(
            lex("42 # this is a comment\n 43"),
            vec![TokenKind::Int(42), TokenKind::Int(43), TokenKind::Eof]
        );
    }
}
