//! Cursor implementation for the parser.

use super::ParseError;
use crate::syntax::ast::{
    punc::Punctuator,
    token::{Token, TokenKind},
};

/// Token cursor.
///
/// This internal structure gives basic testable operations to the parser.
#[derive(Debug, Clone, Default)]
pub(super) struct Cursor<'a> {
    /// The tokens being input.
    tokens: &'a [Token],
    /// The current position within the tokens.
    pos: usize,
}

impl<'a> Cursor<'a> {
    /// Creates a new cursor.
    pub(super) fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            ..Self::default()
        }
    }

    /// Retrieves the current position of the cursor in the token stream.
    pub(super) fn pos(&self) -> usize {
        self.pos
    }

    /// Moves the cursor to the given position.
    /// This is intended to be used *always* with `Cursor::pos()`.
    ///
    pub(super) fn seek(&mut self, pos: usize) {
        self.pos = pos
    }

    /// Moves the cursor to the next token and returns the token.
    pub(super) fn next(&mut self) -> Option<&'a Token> {
        let token = self.tokens.get(self.pos);

        if self.pos != self.tokens.len() {
            self.pos += 1;
        }

        token
    }

    /// Skips all the tokens until it finds a token that makes the `skip` function return `true`.
    ///
    /// Calling `next()` after this will return the token that shouldn't have been skipped.
    pub(super) fn skip<P>(&mut self, mut skip: P)
    where
        P: FnMut(&Token) -> bool,
    {
        while let Some(token) = self.tokens.get(self.pos) {
            if !skip(token) {
                return;
            }
            self.pos += 1;
        }
    }

    /// Moves the cursor to the next token after skipping tokens based on the predicate.
    pub(super) fn next_skip<P>(&mut self, skip: P) -> Option<&'a Token>
    where
        P: FnMut(&Token) -> bool,
    {
        self.skip(skip);
        self.next()
    }

    /// Peeks the next token without moving the cursor.
    pub(super) fn peek(&self, skip: usize) -> Option<&'a Token> {
        self.tokens.get(self.pos + skip)
    }

    /// Peeks the next token after skipping tokens based on the predicate.
    ///
    /// It will not change the cursor position.
    pub(super) fn peek_skip<P>(&self, mut skip: P) -> Option<&'a Token>
    where
        P: FnMut(&Token) -> bool,
    {
        let mut current = self.pos;
        while let Some(token) = self.tokens.get(current) {
            if !skip(token) {
                return Some(token);
            }
            current += 1;
        }

        None
    }

    /// Moves the cursor to the previous token and returns the token.
    pub(super) fn back(&mut self) {
        assert!(
            self.pos > 0,
            "cannot go back in a cursor that is at the beginning of the list of tokens"
        );

        self.pos -= 1;
    }

    /// Peeks the previous token without moving the cursor.
    pub(super) fn peek_prev(&self) -> Option<&'a Token> {
        if self.pos == 0 {
            None
        } else {
            self.tokens.get(self.pos - 1)
        }
    }

    /// Returns an error if the next token is not of kind `kind`.
    ///
    /// Note: it will consume the next token.
    pub(super) fn expect(
        &mut self,
        kind: TokenKind,
        routine: &'static str,
    ) -> Result<(), ParseError> {
        let next_token = self.next().ok_or(ParseError::AbruptEnd)?;
        if next_token.kind == kind {
            Ok(())
        } else {
            Err(ParseError::Expected(
                vec![kind],
                next_token.clone(),
                routine,
            ))
        }
    }

    /// Returns an error if the next token is not the punctuator `p`.
    ///
    /// Note: it will consume the next token.
    pub(super) fn expect_punc(
        &mut self,
        p: Punctuator,
        routine: &'static str,
    ) -> Result<(), ParseError> {
        self.expect(TokenKind::Punctuator(p), routine)
    }

    /// It will check if the next token is a semicolon.
    ///
    /// It will automatically insert a semicolon if needed, as specified in the [spec][spec].
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-automatic-semicolon-insertion
    pub(super) fn expect_semicolon(
        &mut self,
        do_while: bool,
        routine: &'static str,
    ) -> Result<(), ParseError> {
        match self.peek(0) {
            Some(tk) => match tk.kind {
                TokenKind::Punctuator(Punctuator::Semicolon) => {
                    let _ = self.next();
                    Ok(())
                }
                TokenKind::LineTerminator | TokenKind::Punctuator(Punctuator::CloseBlock) => Ok(()),
                _ => {
                    if do_while {
                        debug_assert!(
                            self.pos != 0,
                            "cannot be finishing a do-while if we are at the beginning"
                        );

                        let tok = self
                            .tokens
                            .get(self.pos - 1)
                            .expect("could not find previous token");
                        if tok.kind == TokenKind::Punctuator(Punctuator::CloseParen) {
                            return Ok(());
                        }
                    }

                    Err(ParseError::Expected(
                        vec![TokenKind::Punctuator(Punctuator::Semicolon)],
                        tk.clone(),
                        routine,
                    ))
                }
            },
            None => Ok(()),
        }
    }

    /// Consume the next token skipping line terminators.
    pub(super) fn next_skip_lineterminator(&mut self) -> Option<&'a Token> {
        self.next_skip(|tk| tk.kind == TokenKind::LineTerminator)
    }

    /// Peek the next token skipping line terminators.
    pub(super) fn peek_skip_lineterminator(&mut self) -> Option<&'a Token> {
        self.peek_skip(|tk| tk.kind == TokenKind::LineTerminator)
    }

    /// Advance the cursor to the next token and retrieve it, only if it's of `kind` type.
    ///
    /// When the next token is a `kind` token, get the token, otherwise return `None`. This
    /// function skips line terminators.
    pub(super) fn next_if_skip_lineterminator(&mut self, kind: TokenKind) -> Option<&'a Token> {
        let next_token = self.peek_skip_lineterminator()?;

        if next_token.kind == kind {
            self.next_skip_lineterminator()
        } else {
            None
        }
    }

    /// Advance the cursor to the next token and retrieve it, only if it's of `kind` type.
    ///
    /// When the next token is a `kind` token, get the token, otherwise return `None`.
    pub(super) fn next_if(&mut self, kind: TokenKind) -> Option<&'a Token> {
        let next_token = self.peek(0)?;

        if next_token.kind == kind {
            self.next()
        } else {
            None
        }
    }
}
