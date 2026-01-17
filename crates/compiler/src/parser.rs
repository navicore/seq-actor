//! Parser for seq-actor
//!
//! Produces an AST from a token stream.

use crate::ast::*;
use crate::lexer::{Token, TokenKind};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Unexpected token {found:?} at line {line}, expected {expected}")]
    UnexpectedToken {
        found: TokenKind,
        expected: String,
        line: usize,
    },
    #[error("Unexpected end of input, expected {expected}")]
    UnexpectedEof { expected: String },
    #[error("Parse error at line {line}: {message}")]
    General { message: String, line: usize },
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut items = Vec::new();
        while !self.is_at_end() {
            items.push(self.parse_item()?);
        }
        Ok(Program { items })
    }

    // =========================================================================
    // Helper methods
    // =========================================================================

    fn is_at_end(&self) -> bool {
        self.peek().kind.is_eof()
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or_else(|| {
            self.tokens.last().expect("tokens should have at least EOF")
        })
    }

    fn peek_kind(&self) -> &TokenKind {
        &self.peek().kind
    }

    fn peek_ahead(&self, n: usize) -> &Token {
        self.tokens.get(self.pos + n).unwrap_or_else(|| {
            self.tokens.last().expect("tokens should have at least EOF")
        })
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.pos += 1;
        }
        self.tokens.get(self.pos - 1).unwrap()
    }

    fn check(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(self.peek_kind()) == std::mem::discriminant(kind)
    }

    fn consume(&mut self, kind: &TokenKind, expected: &str) -> Result<&Token, ParseError> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            Err(ParseError::UnexpectedToken {
                found: self.peek_kind().clone(),
                expected: expected.to_string(),
                line: self.peek().span.line,
            })
        }
    }

    fn match_token(&mut self, kind: &TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn current_span(&self) -> Span {
        self.peek().span
    }

    // =========================================================================
    // Top-level items
    // =========================================================================

    fn parse_item(&mut self) -> Result<Item, ParseError> {
        match self.peek_kind() {
            TokenKind::Type => self.parse_type_def().map(Item::TypeDef),
            TokenKind::Colon => self.parse_func_def().map(Item::FuncDef),
            TokenKind::Include => self.parse_include().map(Item::Include),
            _ => Err(ParseError::UnexpectedToken {
                found: self.peek_kind().clone(),
                expected: "type, function definition, or include".to_string(),
                line: self.peek().span.line,
            }),
        }
    }

    // =========================================================================
    // Type definitions
    // =========================================================================

    fn parse_type_def(&mut self) -> Result<TypeDef, ParseError> {
        let start_span = self.current_span();
        self.consume(&TokenKind::Type, "type")?;

        let name = self.parse_type_ident()?;
        let type_params = self.parse_optional_type_params()?;

        self.consume(&TokenKind::Eq, "=")?;

        let variants = self.parse_variants()?;

        self.consume(&TokenKind::Semicolon, ";")?;

        Ok(TypeDef {
            name,
            type_params,
            variants,
            span: start_span.merge(self.tokens[self.pos - 1].span),
        })
    }

    fn parse_optional_type_params(&mut self) -> Result<Vec<String>, ParseError> {
        if !self.match_token(&TokenKind::LBracket) {
            return Ok(Vec::new());
        }

        let mut params = Vec::new();
        loop {
            params.push(self.parse_type_ident()?);
            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        self.consume(&TokenKind::RBracket, "]")?;
        Ok(params)
    }

    fn parse_variants(&mut self) -> Result<Vec<Variant>, ParseError> {
        let mut variants = Vec::new();

        // First variant may or may not have leading |
        self.match_token(&TokenKind::Pipe);

        loop {
            variants.push(self.parse_variant()?);
            if !self.match_token(&TokenKind::Pipe) {
                break;
            }
        }

        Ok(variants)
    }

    fn parse_variant(&mut self) -> Result<Variant, ParseError> {
        let start_span = self.current_span();
        let name = self.parse_type_ident()?;

        let fields = if self.match_token(&TokenKind::LParen) {
            let fields = self.parse_variant_fields()?;
            self.consume(&TokenKind::RParen, ")")?;
            fields
        } else {
            Vec::new()
        };

        Ok(Variant {
            name,
            fields,
            span: start_span.merge(self.tokens[self.pos - 1].span),
        })
    }

    fn parse_variant_fields(&mut self) -> Result<Vec<Field>, ParseError> {
        let mut fields = Vec::new();

        if self.check(&TokenKind::RParen) {
            return Ok(fields);
        }

        loop {
            fields.push(self.parse_field()?);
            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        Ok(fields)
    }

    fn parse_field(&mut self) -> Result<Field, ParseError> {
        let start_span = self.current_span();

        // Check if this is a named field: name: Type
        let (name, typ) = if matches!(self.peek_kind(), TokenKind::Ident(_))
            && matches!(self.peek_ahead(1).kind, TokenKind::Colon)
        {
            let name = self.parse_ident()?;
            self.consume(&TokenKind::Colon, ":")?;
            let typ = self.parse_type_expr()?;
            (Some(name), typ)
        } else {
            // Anonymous field
            let typ = self.parse_type_expr()?;
            (None, typ)
        };

        Ok(Field {
            name,
            typ,
            span: start_span.merge(self.tokens[self.pos - 1].span),
        })
    }

    fn parse_type_expr(&mut self) -> Result<TypeExpr, ParseError> {
        let start_span = self.current_span();

        // Check for unit type ()
        if self.match_token(&TokenKind::LParen) {
            if self.match_token(&TokenKind::RParen) {
                return Ok(TypeExpr::Unit { span: start_span.merge(self.tokens[self.pos - 1].span) });
            }

            // Tuple or grouped type
            let first = self.parse_type_expr()?;
            if self.match_token(&TokenKind::Comma) {
                // Tuple type
                let mut elements = vec![first];
                loop {
                    elements.push(self.parse_type_expr()?);
                    if !self.match_token(&TokenKind::Comma) {
                        break;
                    }
                }
                self.consume(&TokenKind::RParen, ")")?;
                return Ok(TypeExpr::Tuple {
                    elements,
                    span: start_span.merge(self.tokens[self.pos - 1].span),
                });
            }

            // Check for function type (T) -> U
            self.consume(&TokenKind::RParen, ")")?;
            if self.match_token(&TokenKind::Arrow) {
                let ret = self.parse_type_expr()?;
                return Ok(TypeExpr::Function {
                    params: vec![first],
                    ret: Box::new(ret),
                    span: start_span.merge(self.tokens[self.pos - 1].span),
                });
            }

            // Just a grouped type
            return Ok(first);
        }

        // Named type
        let name = self.parse_type_ident()?;

        // Check for Actor special type
        if name == "Actor" {
            self.consume(&TokenKind::LBracket, "[")?;
            let message_type = self.parse_type_expr()?;
            self.consume(&TokenKind::RBracket, "]")?;
            return Ok(TypeExpr::Actor {
                message_type: Box::new(message_type),
                span: start_span.merge(self.tokens[self.pos - 1].span),
            });
        }

        // Type arguments
        let args = if self.match_token(&TokenKind::LBracket) {
            let mut args = Vec::new();
            loop {
                args.push(self.parse_type_expr()?);
                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
            self.consume(&TokenKind::RBracket, "]")?;
            args
        } else {
            Vec::new()
        };

        Ok(TypeExpr::Named {
            name,
            args,
            span: start_span.merge(self.tokens[self.pos - 1].span),
        })
    }

    // =========================================================================
    // Function definitions
    // =========================================================================

    fn parse_func_def(&mut self) -> Result<FuncDef, ParseError> {
        let start_span = self.current_span();
        self.consume(&TokenKind::Colon, ":")?;

        let name = self.parse_ident()?;
        let type_params = self.parse_optional_type_params()?;

        let clauses = self.parse_func_clauses()?;

        self.consume(&TokenKind::Semicolon, ";")?;

        Ok(FuncDef {
            name,
            type_params,
            clauses,
            span: start_span.merge(self.tokens[self.pos - 1].span),
        })
    }

    fn parse_func_clauses(&mut self) -> Result<Vec<FuncClause>, ParseError> {
        let mut clauses = Vec::new();

        // Check if we have multi-clause form with leading |
        if self.match_token(&TokenKind::Pipe) {
            loop {
                clauses.push(self.parse_func_clause()?);
                if !self.match_token(&TokenKind::Pipe) {
                    break;
                }
            }
        } else {
            // Single clause form: (params) -> body
            clauses.push(self.parse_func_clause()?);
        }

        Ok(clauses)
    }

    fn parse_func_clause(&mut self) -> Result<FuncClause, ParseError> {
        let start_span = self.current_span();

        self.consume(&TokenKind::LParen, "(")?;
        let params = if self.check(&TokenKind::RParen) {
            Vec::new()
        } else {
            self.parse_pattern_list()?
        };
        self.consume(&TokenKind::RParen, ")")?;

        self.consume(&TokenKind::Arrow, "->")?;

        let body = self.parse_expr()?;

        Ok(FuncClause {
            params,
            guard: None, // TODO: implement guards
            body,
            span: start_span.merge(self.tokens[self.pos - 1].span),
        })
    }

    fn parse_pattern_list(&mut self) -> Result<Vec<Pattern>, ParseError> {
        let mut patterns = Vec::new();
        loop {
            patterns.push(self.parse_pattern()?);
            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }
        Ok(patterns)
    }

    // =========================================================================
    // Patterns
    // =========================================================================

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        let start_span = self.current_span();

        match self.peek_kind().clone() {
            TokenKind::Underscore => {
                self.advance();
                Ok(Pattern::Wildcard { span: start_span })
            }
            TokenKind::Int(n) => {
                self.advance();
                Ok(Pattern::Literal {
                    value: Literal::Int(n),
                    span: start_span,
                })
            }
            TokenKind::Float(n) => {
                self.advance();
                Ok(Pattern::Literal {
                    value: Literal::Float(n),
                    span: start_span,
                })
            }
            TokenKind::Bool(b) => {
                self.advance();
                Ok(Pattern::Literal {
                    value: Literal::Bool(b),
                    span: start_span,
                })
            }
            TokenKind::String(s) => {
                self.advance();
                Ok(Pattern::Literal {
                    value: Literal::String(s),
                    span: start_span,
                })
            }
            TokenKind::Ident(name) => {
                self.advance();
                Ok(Pattern::Var { name, span: start_span })
            }
            TokenKind::TypeIdent(name) => {
                self.advance();
                // Constructor pattern
                let args = if self.match_token(&TokenKind::LParen) {
                    let args = if self.check(&TokenKind::RParen) {
                        Vec::new()
                    } else {
                        self.parse_pattern_list()?
                    };
                    self.consume(&TokenKind::RParen, ")")?;
                    args
                } else {
                    Vec::new()
                };
                Ok(Pattern::Constructor {
                    name,
                    args,
                    span: start_span.merge(self.tokens[self.pos - 1].span),
                })
            }
            TokenKind::LParen => {
                self.advance();
                // Check for unit
                if self.match_token(&TokenKind::RParen) {
                    return Ok(Pattern::Literal {
                        value: Literal::Unit,
                        span: start_span.merge(self.tokens[self.pos - 1].span),
                    });
                }
                // Tuple pattern
                let elements = self.parse_pattern_list()?;
                self.consume(&TokenKind::RParen, ")")?;
                if elements.len() == 1 {
                    Ok(elements.into_iter().next().unwrap())
                } else {
                    Ok(Pattern::Tuple {
                        elements,
                        span: start_span.merge(self.tokens[self.pos - 1].span),
                    })
                }
            }
            TokenKind::LBrace => {
                self.advance();
                // Tagged pattern {tag, ...}
                let tag = self.parse_ident()?;
                let elements = if self.match_token(&TokenKind::Comma) {
                    self.parse_pattern_list()?
                } else {
                    Vec::new()
                };
                self.consume(&TokenKind::RBrace, "}")?;
                Ok(Pattern::Tagged {
                    tag,
                    elements,
                    span: start_span.merge(self.tokens[self.pos - 1].span),
                })
            }
            TokenKind::LBracket => {
                self.advance();
                // List pattern
                if self.match_token(&TokenKind::RBracket) {
                    return Ok(Pattern::List {
                        elements: Vec::new(),
                        tail: None,
                        span: start_span.merge(self.tokens[self.pos - 1].span),
                    });
                }
                let first = self.parse_pattern()?;
                if self.match_token(&TokenKind::Pipe) {
                    // [h | t] form
                    let tail = self.parse_pattern()?;
                    self.consume(&TokenKind::RBracket, "]")?;
                    Ok(Pattern::List {
                        elements: vec![first],
                        tail: Some(Box::new(tail)),
                        span: start_span.merge(self.tokens[self.pos - 1].span),
                    })
                } else {
                    // [a, b, c] form
                    let mut elements = vec![first];
                    while self.match_token(&TokenKind::Comma) {
                        elements.push(self.parse_pattern()?);
                    }
                    self.consume(&TokenKind::RBracket, "]")?;
                    Ok(Pattern::List {
                        elements,
                        tail: None,
                        span: start_span.merge(self.tokens[self.pos - 1].span),
                    })
                }
            }
            _ => Err(ParseError::UnexpectedToken {
                found: self.peek_kind().clone(),
                expected: "pattern".to_string(),
                line: self.peek().span.line,
            }),
        }
    }

    // =========================================================================
    // Expressions
    // =========================================================================

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_let_expr()
    }

    fn parse_let_expr(&mut self) -> Result<Expr, ParseError> {
        if self.match_token(&TokenKind::Let) {
            let start_span = self.tokens[self.pos - 1].span;
            let pattern = self.parse_pattern()?;
            self.consume(&TokenKind::Eq, "=")?;
            let value = self.parse_expr()?;
            let body = self.parse_expr()?;
            let body_span = body.span();
            Ok(Expr::Let {
                pattern,
                value: Box::new(value),
                body: Box::new(body),
                span: start_span.merge(body_span),
            })
        } else {
            self.parse_if_expr()
        }
    }

    fn parse_if_expr(&mut self) -> Result<Expr, ParseError> {
        if self.match_token(&TokenKind::If) {
            let start_span = self.tokens[self.pos - 1].span;
            let cond = self.parse_or_expr()?;
            self.consume(&TokenKind::Then, "then")?;
            let then_branch = self.parse_expr()?;
            self.consume(&TokenKind::Else, "else")?;
            let else_branch = self.parse_expr()?;
            let end_span = else_branch.span();
            Ok(Expr::If {
                cond: Box::new(cond),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
                span: start_span.merge(end_span),
            })
        } else {
            self.parse_or_expr()
        }
    }

    fn parse_or_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_and_expr()?;

        while self.match_token(&TokenKind::Or) {
            let right = self.parse_and_expr()?;
            let span = left.span().merge(right.span());
            left = Expr::BinOp {
                op: BinOp::Or,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }

        Ok(left)
    }

    fn parse_and_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_comparison()?;

        while self.match_token(&TokenKind::And) {
            let right = self.parse_comparison()?;
            let span = left.span().merge(right.span());
            left = Expr::BinOp {
                op: BinOp::And,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }

        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_additive()?;

        loop {
            let op = match self.peek_kind() {
                TokenKind::EqEq => BinOp::Eq,
                TokenKind::BangEq => BinOp::Ne,
                TokenKind::Lt => BinOp::Lt,
                TokenKind::LtEq => BinOp::Le,
                TokenKind::Gt => BinOp::Gt,
                TokenKind::GtEq => BinOp::Ge,
                _ => break,
            };
            self.advance();
            let right = self.parse_additive()?;
            let span = left.span().merge(right.span());
            left = Expr::BinOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }

        Ok(left)
    }

    fn parse_additive(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_multiplicative()?;

        loop {
            let op = match self.peek_kind() {
                TokenKind::Plus => BinOp::Add,
                TokenKind::Minus => BinOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_multiplicative()?;
            let span = left.span().merge(right.span());
            left = Expr::BinOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }

        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_unary()?;

        loop {
            let op = match self.peek_kind() {
                TokenKind::Star => BinOp::Mul,
                TokenKind::Slash => BinOp::Div,
                TokenKind::Percent => BinOp::Mod,
                _ => break,
            };
            self.advance();
            let right = self.parse_unary()?;
            let span = left.span().merge(right.span());
            left = Expr::BinOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_token(&TokenKind::Minus) {
            let start_span = self.tokens[self.pos - 1].span;
            let operand = self.parse_unary()?;
            let span = start_span.merge(operand.span());
            return Ok(Expr::UnaryOp {
                op: UnaryOp::Neg,
                operand: Box::new(operand),
                span,
            });
        }

        if self.match_token(&TokenKind::Not) {
            let start_span = self.tokens[self.pos - 1].span;
            let operand = self.parse_unary()?;
            let span = start_span.merge(operand.span());
            return Ok(Expr::UnaryOp {
                op: UnaryOp::Not,
                operand: Box::new(operand),
                span,
            });
        }

        self.parse_call()
    }

    fn parse_call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary()?;

        while self.match_token(&TokenKind::LParen) {
            let start_span = expr.span();
            let args = if self.check(&TokenKind::RParen) {
                Vec::new()
            } else {
                self.parse_expr_list()?
            };
            self.consume(&TokenKind::RParen, ")")?;
            let span = start_span.merge(self.tokens[self.pos - 1].span);
            expr = Expr::Call {
                func: Box::new(expr),
                args,
                span,
            };
        }

        Ok(expr)
    }

    fn parse_expr_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut exprs = Vec::new();
        loop {
            exprs.push(self.parse_expr()?);
            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }
        Ok(exprs)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let start_span = self.current_span();

        match self.peek_kind().clone() {
            TokenKind::Int(n) => {
                self.advance();
                Ok(Expr::Literal {
                    value: Literal::Int(n),
                    span: start_span,
                })
            }
            TokenKind::Float(n) => {
                self.advance();
                Ok(Expr::Literal {
                    value: Literal::Float(n),
                    span: start_span,
                })
            }
            TokenKind::Bool(b) => {
                self.advance();
                Ok(Expr::Literal {
                    value: Literal::Bool(b),
                    span: start_span,
                })
            }
            TokenKind::String(s) => {
                self.advance();
                Ok(Expr::Literal {
                    value: Literal::String(s),
                    span: start_span,
                })
            }
            TokenKind::SelfKw => {
                self.advance();
                // self() - current actor reference
                if self.match_token(&TokenKind::LParen) {
                    self.consume(&TokenKind::RParen, ")")?;
                }
                Ok(Expr::SelfRef {
                    span: start_span.merge(self.tokens[self.pos - 1].span),
                })
            }
            TokenKind::Ident(name) => {
                self.advance();
                // Check for module-qualified call: module:function(args)
                if self.match_token(&TokenKind::Colon) {
                    let func = self.parse_ident()?;
                    self.consume(&TokenKind::LParen, "(")?;
                    let args = if self.check(&TokenKind::RParen) {
                        Vec::new()
                    } else {
                        self.parse_expr_list()?
                    };
                    self.consume(&TokenKind::RParen, ")")?;
                    return Ok(Expr::ModuleCall {
                        module: name,
                        func,
                        args,
                        span: start_span.merge(self.tokens[self.pos - 1].span),
                    });
                }
                Ok(Expr::Var { name, span: start_span })
            }
            TokenKind::TypeIdent(name) => {
                self.advance();
                // Constructor
                let args = if self.match_token(&TokenKind::LParen) {
                    let args = if self.check(&TokenKind::RParen) {
                        Vec::new()
                    } else {
                        self.parse_expr_list()?
                    };
                    self.consume(&TokenKind::RParen, ")")?;
                    args
                } else {
                    Vec::new()
                };
                Ok(Expr::Constructor {
                    name,
                    args,
                    span: start_span.merge(self.tokens[self.pos - 1].span),
                })
            }
            TokenKind::LParen => {
                self.advance();
                // Check for unit ()
                if self.match_token(&TokenKind::RParen) {
                    return Ok(Expr::Literal {
                        value: Literal::Unit,
                        span: start_span.merge(self.tokens[self.pos - 1].span),
                    });
                }
                // Tuple or grouped expression
                let first = self.parse_expr()?;
                if self.match_token(&TokenKind::Comma) {
                    let mut elements = vec![first];
                    loop {
                        elements.push(self.parse_expr()?);
                        if !self.match_token(&TokenKind::Comma) {
                            break;
                        }
                    }
                    self.consume(&TokenKind::RParen, ")")?;
                    Ok(Expr::Tuple {
                        elements,
                        span: start_span.merge(self.tokens[self.pos - 1].span),
                    })
                } else {
                    self.consume(&TokenKind::RParen, ")")?;
                    Ok(first)
                }
            }
            TokenKind::LBrace => {
                self.advance();
                // Tagged expression {tag, ...}
                let tag = self.parse_ident()?;
                let elements = if self.match_token(&TokenKind::Comma) {
                    self.parse_expr_list()?
                } else {
                    Vec::new()
                };
                self.consume(&TokenKind::RBrace, "}")?;
                Ok(Expr::Tagged {
                    tag,
                    elements,
                    span: start_span.merge(self.tokens[self.pos - 1].span),
                })
            }
            TokenKind::LBracket => {
                self.advance();
                // List expression
                if self.match_token(&TokenKind::RBracket) {
                    return Ok(Expr::List {
                        elements: Vec::new(),
                        tail: None,
                        span: start_span.merge(self.tokens[self.pos - 1].span),
                    });
                }
                let first = self.parse_expr()?;
                if self.match_token(&TokenKind::Pipe) {
                    // [h | t] form
                    let tail = self.parse_expr()?;
                    self.consume(&TokenKind::RBracket, "]")?;
                    Ok(Expr::List {
                        elements: vec![first],
                        tail: Some(Box::new(tail)),
                        span: start_span.merge(self.tokens[self.pos - 1].span),
                    })
                } else {
                    // [a, b, c] form
                    let mut elements = vec![first];
                    while self.match_token(&TokenKind::Comma) {
                        elements.push(self.parse_expr()?);
                    }
                    self.consume(&TokenKind::RBracket, "]")?;
                    Ok(Expr::List {
                        elements,
                        tail: None,
                        span: start_span.merge(self.tokens[self.pos - 1].span),
                    })
                }
            }
            TokenKind::Match => {
                self.advance();
                let expr = self.parse_expr()?;
                self.consume(&TokenKind::With, "with")?;
                let arms = self.parse_match_arms()?;
                self.consume(&TokenKind::End, "end")?;
                Ok(Expr::Match {
                    expr: Box::new(expr),
                    arms,
                    span: start_span.merge(self.tokens[self.pos - 1].span),
                })
            }
            TokenKind::Receive => {
                self.advance();
                let arms = self.parse_receive_arms()?;
                let timeout = if self.match_token(&TokenKind::After) {
                    let duration = self.parse_expr()?;
                    self.consume(&TokenKind::Arrow, "->")?;
                    let body = self.parse_expr()?;
                    Some(ReceiveTimeout {
                        duration: Box::new(duration),
                        body: Box::new(body.clone()),
                        span: start_span.merge(body.span()),
                    })
                } else {
                    None
                };
                self.consume(&TokenKind::End, "end")?;
                Ok(Expr::Receive {
                    arms,
                    timeout,
                    span: start_span.merge(self.tokens[self.pos - 1].span),
                })
            }
            TokenKind::Become => {
                self.advance();
                let func = self.parse_ident()?;
                self.consume(&TokenKind::LParen, "(")?;
                let args = if self.check(&TokenKind::RParen) {
                    Vec::new()
                } else {
                    self.parse_expr_list()?
                };
                self.consume(&TokenKind::RParen, ")")?;
                Ok(Expr::Become {
                    func,
                    args,
                    span: start_span.merge(self.tokens[self.pos - 1].span),
                })
            }
            TokenKind::Spawn => {
                self.advance();
                let func = self.parse_ident()?;
                self.consume(&TokenKind::LParen, "(")?;
                let args = if self.check(&TokenKind::RParen) {
                    Vec::new()
                } else {
                    self.parse_expr_list()?
                };
                self.consume(&TokenKind::RParen, ")")?;
                Ok(Expr::Spawn {
                    func,
                    args,
                    span: start_span.merge(self.tokens[self.pos - 1].span),
                })
            }
            TokenKind::Send => {
                self.advance();
                self.consume(&TokenKind::LParen, "(")?;
                let target = self.parse_expr()?;
                self.consume(&TokenKind::Comma, ",")?;
                let message = self.parse_expr()?;
                self.consume(&TokenKind::RParen, ")")?;
                Ok(Expr::Send {
                    target: Box::new(target),
                    message: Box::new(message),
                    span: start_span.merge(self.tokens[self.pos - 1].span),
                })
            }
            TokenKind::Copy => {
                self.advance();
                self.consume(&TokenKind::LParen, "(")?;
                let arg = self.parse_expr()?;
                self.consume(&TokenKind::RParen, ")")?;
                // copy(x) returns a tuple (x, x)
                Ok(Expr::Call {
                    func: Box::new(Expr::Var {
                        name: "copy".to_string(),
                        span: start_span,
                    }),
                    args: vec![arg],
                    span: start_span.merge(self.tokens[self.pos - 1].span),
                })
            }
            TokenKind::Supervised => {
                self.advance();
                // Check for optional strategy
                let strategy = if self.match_token(&TokenKind::OneForOne) {
                    SupervisionStrategy::OneForOne
                } else if self.match_token(&TokenKind::OneForAll) {
                    SupervisionStrategy::OneForAll
                } else if self.match_token(&TokenKind::RestForOne) {
                    SupervisionStrategy::RestForOne
                } else {
                    SupervisionStrategy::OneForOne // default
                };
                // Parse body expressions until 'end'
                let mut body = Vec::new();
                while !self.check(&TokenKind::End) && !self.is_at_end() {
                    body.push(self.parse_expr()?);
                }
                self.consume(&TokenKind::End, "end")?;
                Ok(Expr::Supervised {
                    strategy,
                    body,
                    span: start_span.merge(self.tokens[self.pos - 1].span),
                })
            }
            _ => Err(ParseError::UnexpectedToken {
                found: self.peek_kind().clone(),
                expected: "expression".to_string(),
                line: self.peek().span.line,
            }),
        }
    }

    fn parse_match_arms(&mut self) -> Result<Vec<MatchArm>, ParseError> {
        let mut arms = Vec::new();
        while self.match_token(&TokenKind::Pipe) {
            let start_span = self.current_span();
            let pattern = self.parse_pattern()?;
            self.consume(&TokenKind::Arrow, "->")?;
            let body = self.parse_expr()?;
            arms.push(MatchArm {
                pattern,
                guard: None,
                body: body.clone(),
                span: start_span.merge(body.span()),
            });
        }
        Ok(arms)
    }

    fn parse_receive_arms(&mut self) -> Result<Vec<ReceiveArm>, ParseError> {
        let mut arms = Vec::new();
        while self.match_token(&TokenKind::Pipe) {
            let start_span = self.current_span();
            let pattern = self.parse_pattern()?;
            self.consume(&TokenKind::Arrow, "->")?;
            let body = self.parse_expr()?;
            arms.push(ReceiveArm {
                pattern,
                guard: None,
                body: body.clone(),
                span: start_span.merge(body.span()),
            });
        }
        Ok(arms)
    }

    // =========================================================================
    // Includes
    // =========================================================================

    fn parse_include(&mut self) -> Result<Include, ParseError> {
        let start_span = self.current_span();
        self.consume(&TokenKind::Include, "include")?;

        let kind = match self.peek_kind().clone() {
            TokenKind::String(path) => {
                self.advance();
                IncludeKind::File(path)
            }
            TokenKind::Std => {
                self.advance();
                self.consume(&TokenKind::Colon, ":")?;
                let name = self.parse_ident()?;
                IncludeKind::Std(name)
            }
            TokenKind::Ffi => {
                self.advance();
                self.consume(&TokenKind::Colon, ":")?;
                let name = self.parse_ident()?;
                IncludeKind::Ffi(name)
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    found: self.peek_kind().clone(),
                    expected: "string path or std: or ffi:".to_string(),
                    line: self.peek().span.line,
                });
            }
        };

        Ok(Include {
            kind,
            span: start_span.merge(self.tokens[self.pos - 1].span),
        })
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    fn parse_ident(&mut self) -> Result<String, ParseError> {
        match self.peek_kind().clone() {
            TokenKind::Ident(name) => {
                self.advance();
                Ok(name)
            }
            _ => Err(ParseError::UnexpectedToken {
                found: self.peek_kind().clone(),
                expected: "identifier".to_string(),
                line: self.peek().span.line,
            }),
        }
    }

    fn parse_type_ident(&mut self) -> Result<String, ParseError> {
        match self.peek_kind().clone() {
            TokenKind::TypeIdent(name) => {
                self.advance();
                Ok(name)
            }
            TokenKind::Ident(name) => {
                // Allow lowercase for type variables
                self.advance();
                Ok(name)
            }
            _ => Err(ParseError::UnexpectedToken {
                found: self.peek_kind().clone(),
                expected: "type identifier".to_string(),
                line: self.peek().span.line,
            }),
        }
    }
}

/// Parse source code into an AST
pub fn parse(source: &str) -> Result<Program, ParseError> {
    use crate::lexer::Lexer;
    let tokens = Lexer::new(source).tokenize();
    let mut parser = Parser::new(tokens);
    parser.parse_program()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_function() {
        let source = ": double (x) -> x * 2 ;";
        let program = parse(source).unwrap();
        assert_eq!(program.items.len(), 1);
        match &program.items[0] {
            Item::FuncDef(f) => {
                assert_eq!(f.name, "double");
                assert_eq!(f.clauses.len(), 1);
            }
            _ => panic!("Expected function definition"),
        }
    }

    #[test]
    fn test_parse_multi_clause_function() {
        let source = r#"
            : factorial
              | (0) -> 1
              | (n) -> n * factorial(n - 1)
            ;
        "#;
        let program = parse(source).unwrap();
        match &program.items[0] {
            Item::FuncDef(f) => {
                assert_eq!(f.name, "factorial");
                assert_eq!(f.clauses.len(), 2);
            }
            _ => panic!("Expected function definition"),
        }
    }

    #[test]
    fn test_parse_type_def() {
        let source = r#"
            type Option[T] =
              | None
              | Some(value: T)
            ;
        "#;
        let program = parse(source).unwrap();
        match &program.items[0] {
            Item::TypeDef(t) => {
                assert_eq!(t.name, "Option");
                assert_eq!(t.type_params, vec!["T"]);
                assert_eq!(t.variants.len(), 2);
            }
            _ => panic!("Expected type definition"),
        }
    }

    #[test]
    fn test_parse_let_expr() {
        let source = r#"
            : example (x) ->
              let y = x * 2
              y + 1
            ;
        "#;
        let program = parse(source).unwrap();
        match &program.items[0] {
            Item::FuncDef(f) => {
                match &f.clauses[0].body {
                    Expr::Let { pattern, .. } => {
                        assert!(matches!(pattern, Pattern::Var { name, .. } if name == "y"));
                    }
                    _ => panic!("Expected let expression"),
                }
            }
            _ => panic!("Expected function definition"),
        }
    }

    #[test]
    fn test_parse_if_expr() {
        let source = ": abs (x) -> if x < 0 then 0 - x else x ;";
        let program = parse(source).unwrap();
        match &program.items[0] {
            Item::FuncDef(f) => {
                assert!(matches!(f.clauses[0].body, Expr::If { .. }));
            }
            _ => panic!("Expected function definition"),
        }
    }

    #[test]
    fn test_parse_tagged() {
        let source = ": mk_pair (x, y) -> {pair, x, y} ;";
        let program = parse(source).unwrap();
        match &program.items[0] {
            Item::FuncDef(f) => {
                match &f.clauses[0].body {
                    Expr::Tagged { tag, elements, .. } => {
                        assert_eq!(tag, "pair");
                        assert_eq!(elements.len(), 2);
                    }
                    _ => panic!("Expected tagged expression"),
                }
            }
            _ => panic!("Expected function definition"),
        }
    }

    #[test]
    fn test_parse_list() {
        let source = ": mk_list () -> [1, 2, 3] ;";
        let program = parse(source).unwrap();
        match &program.items[0] {
            Item::FuncDef(f) => {
                match &f.clauses[0].body {
                    Expr::List { elements, tail, .. } => {
                        assert_eq!(elements.len(), 3);
                        assert!(tail.is_none());
                    }
                    _ => panic!("Expected list expression"),
                }
            }
            _ => panic!("Expected function definition"),
        }
    }

    #[test]
    fn test_parse_module_call() {
        let source = r#": main () -> io:println("Hello") ;"#;
        let program = parse(source).unwrap();
        match &program.items[0] {
            Item::FuncDef(f) => {
                match &f.clauses[0].body {
                    Expr::ModuleCall { module, func, args, .. } => {
                        assert_eq!(module, "io");
                        assert_eq!(func, "println");
                        assert_eq!(args.len(), 1);
                    }
                    _ => panic!("Expected ModuleCall expression, got {:?}", f.clauses[0].body),
                }
            }
            _ => panic!("Expected function definition"),
        }
    }

    #[test]
    fn test_parse_supervised_block() {
        let source = r#"
            : main () ->
                supervised one_for_all
                    spawn worker()
                end
            ;
        "#;
        let program = parse(source).unwrap();
        match &program.items[0] {
            Item::FuncDef(f) => {
                match &f.clauses[0].body {
                    Expr::Supervised { strategy, body, .. } => {
                        assert_eq!(*strategy, SupervisionStrategy::OneForAll);
                        assert!(!body.is_empty());
                    }
                    _ => panic!("Expected Supervised expression, got {:?}", f.clauses[0].body),
                }
            }
            _ => panic!("Expected function definition"),
        }
    }
}
