use crate::syntax::ast::{keyword::Keyword, pos::Position, punc::Punctuator};
use std::fmt::{Debug, Display, Formatter, Result};

#[cfg(feature = "serde-ast")]
use serde::{Deserialize, Serialize};

/// Represents a token.
#[cfg_attr(feature = "serde-ast", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// The token Data
    pub kind: TokenKind,
    /// Token position from original source code
    pub pos: Position,
}

impl Token {
    /// Create a new detailed token from the token data, line number and column number
    pub fn new(kind: TokenKind, line_number: u64, column_number: u64) -> Self {
        Self {
            kind,
            pos: Position::new(line_number, column_number),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.kind)
    }
}

pub struct VecToken(Vec<Token>);

impl Debug for VecToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let mut buffer = String::new();
        for token in &self.0 {
            buffer.push_str(&token.to_string());
        }
        write!(f, "{}", buffer)
    }
}

/// Represents the type of Token and the data it has inside.
#[cfg_attr(feature = "serde-ast", derive(Serialize, Deserialize))]
#[derive(Clone, PartialEq, Debug)]
pub enum TokenKind {
    /// A boolean literal, which is either `true` or `false`
    BooleanLiteral(bool),
    /// The end of the file
    EOF,
    /// An identifier
    Identifier(String),
    /// A keyword
    Keyword(Keyword),
    /// A `null` literal
    NullLiteral,
    /// A numeric literal
    NumericLiteral(f64),
    /// A piece of punctuation
    Punctuator(Punctuator),
    /// A string literal
    StringLiteral(String),
    /// A regular expression, consisting of body and flags
    RegularExpressionLiteral(String, String),
    /// Indicates the end of a line \n
    LineTerminator,
}

impl TokenKind {
    /// Creates a `BooleanLiteral` token kind.
    pub fn boolean_literal(lit: bool) -> Self {
        Self::BooleanLiteral(lit)
    }

    /// Creates an `EOF` token kind.
    pub fn eof() -> Self {
        Self::EOF
    }

    /// Creates an `Identifier` token type.
    pub fn identifier<I>(ident: I) -> Self
    where
        I: Into<String>,
    {
        Self::Identifier(ident.into())
    }

    /// Creates a `Keyword` token kind.
    pub fn keyword(keyword: Keyword) -> Self {
        Self::Keyword(keyword)
    }

    /// Creates a `NumericLiteral` token kind.
    pub fn numeric_literal(lit: f64) -> Self {
        Self::NumericLiteral(lit)
    }

    /// Creates a `Punctuator` token type.
    pub fn punctuator(punc: Punctuator) -> Self {
        Self::Punctuator(punc)
    }

    /// Creates a `StringLiteral` token type.
    pub fn string_literal<S>(lit: S) -> Self
    where
        S: Into<String>,
    {
        Self::StringLiteral(lit.into())
    }

    /// Creates a `RegularExpressionLiteral` token kind.
    pub fn regular_expression_literal<B, F>(body: B, flags: F) -> Self
    where
        B: Into<String>,
        F: Into<String>,
    {
        Self::RegularExpressionLiteral(body.into(), flags.into())
    }

    /// Creates a `LineTerminator` token kind.
    pub fn line_terminator() -> Self {
        Self::LineTerminator
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match *self {
            Self::BooleanLiteral(ref val) => write!(f, "{}", val),
            Self::EOF => write!(f, "end of file"),
            Self::Identifier(ref ident) => write!(f, "{}", ident),
            Self::Keyword(ref word) => write!(f, "{}", word),
            Self::NullLiteral => write!(f, "null"),
            Self::NumericLiteral(ref num) => write!(f, "{}", num),
            Self::Punctuator(ref punc) => write!(f, "{}", punc),
            Self::StringLiteral(ref lit) => write!(f, "{}", lit),
            Self::RegularExpressionLiteral(ref body, ref flags) => write!(f, "/{}/{}", body, flags),
            Self::LineTerminator => write!(f, "line terminator"),
        }
    }
}
