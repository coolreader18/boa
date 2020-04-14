//! Boa parser implementation.

mod cursor;
pub mod error;
mod expression;
mod function;
mod statement;
#[cfg(test)]
mod tests;

use self::{
    error::{ParseError, ParseResult},
    expression::*,
    function::*,
    statement::*,
};
use crate::syntax::ast::{node::Node, token::Token};
use cursor::Cursor;

/// Trait implemented by parsers.
///
/// This makes it possible to abstract over the underlying implementation of a parser.
trait TokenParser {
    /// Parses the token stream using the current parser.
    ///
    /// This method needs to be provided by the implementor type.
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult;

    /// Tries to parse the following tokens with this parser.
    ///
    /// It will return the cursor to the initial position if an error occurs during parsing.
    fn try_parse(cursor: &mut Cursor<'_>) -> Option<Node> {
        let initial_pos = cursor.pos();
        if let Ok(node) = Self::parse(cursor) {
            Some(node)
        } else {
            cursor.seek(initial_pos);
            None
        }
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    /// Cursor in the parser, the internal structure used to read tokens.
    cursor: Cursor<'a>,
}

impl<'a> Parser<'a> {
    /// Create a new parser, using `tokens` as input
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            cursor: Cursor::new(tokens),
        }
    }

    /// Parse all expressions in the token array
    pub fn parse_all(&mut self) -> ParseResult {
        read_statements(&mut self.cursor, false).map(Node::StatementList)
    }
}
