//! Function parsing.

use super::{AssignmentExpression, Block, Cursor, ParseError, ParseResult, TokenParser};
use crate::syntax::ast::{
    node::{FormalParameter, FormalParameters, Node},
    punc::Punctuator,
    token::TokenKind,
};

/// Arrow function parsing.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Arrow_functions
/// [spec]: https://tc39.es/ecma262/#sec-arrow-function-definitions
#[derive(Debug, Clone, Copy)]
pub(super) struct ArrowFunction;

impl TokenParser for ArrowFunction {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let next_token = cursor.next().ok_or(ParseError::AbruptEnd)?;
        let params = match &next_token.kind {
            TokenKind::Punctuator(Punctuator::OpenParen) => read_formal_parameters(cursor)?,
            TokenKind::Identifier(param_name) => vec![FormalParameter {
                init: None,
                name: param_name.clone(),
                is_rest_param: false,
            }],
            _ => {
                return Err(ParseError::Expected(
                    vec![
                        TokenKind::Punctuator(Punctuator::OpenParen),
                        TokenKind::Identifier("identifier".to_owned()),
                    ],
                    next_token.clone(),
                    Some("arrow function"),
                ))
            }
        };

        cursor.expect_punc(Punctuator::Arrow, Some("arrow function"))?;

        cursor.skip(|tk| tk.kind == TokenKind::LineTerminator);
        let body = match cursor.peek(0) {
            Some(tk) if tk.kind == TokenKind::Punctuator(Punctuator::OpenBlock) => {
                Block::parse(cursor)?
            }
            _ => Node::Return(Some(Box::new(AssignmentExpression::parse(cursor)?))),
        };

        Ok(Node::ArrowFunctionDecl(params, Box::new(body)))
    }
}

/// Formal parameters parsing.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Glossary/Parameter
/// [spec]: https://tc39.es/ecma262/#prod-FormalParameters
pub(super) fn read_formal_parameters(
    cursor: &mut Cursor<'_>,
) -> Result<FormalParameters, ParseError> {
    let mut params = Vec::new();

    if cursor
        .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::CloseParen))
        .is_some()
    {
        return Ok(params);
    }

    loop {
        let mut rest_param = false;

        params.push(
            if cursor
                .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::Spread))
                .is_some()
            {
                rest_param = true;
                read_function_rest_parameter(cursor)?
            } else {
                read_formal_parameter(cursor)?
            },
        );

        if cursor
            .next_if(TokenKind::Punctuator(Punctuator::CloseParen))
            .is_some()
        {
            break;
        }

        if rest_param {
            return Err(ParseError::Unexpected(
                cursor
                    .peek_prev()
                    .expect("current token disappeared")
                    .clone(),
                Some("rest parameter must be the last formal parameter"),
            ));
        }

        cursor.expect_punc(Punctuator::Comma, Some("parameter list"))?;
    }

    Ok(params)
}

/// Rest parameter parsing.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/rest_parameters
/// [spec]: https://tc39.es/ecma262/#prod-FunctionRestParameter
fn read_function_rest_parameter(cursor: &mut Cursor<'_>) -> Result<FormalParameter, ParseError> {
    let token = cursor.next().ok_or(ParseError::AbruptEnd)?;
    Ok(FormalParameter::new(
        if let TokenKind::Identifier(name) = &token.kind {
            name.clone()
        } else {
            return Err(ParseError::Expected(
                vec![TokenKind::Identifier("identifier".to_owned())],
                token.clone(),
                Some("rest parameter"),
            ));
        },
        None,
        true,
    ))
}

/// Formal parameter parsing.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Glossary/Parameter
/// [spec]: https://tc39.es/ecma262/#prod-FormalParameter
fn read_formal_parameter(cursor: &mut Cursor<'_>) -> Result<FormalParameter, ParseError> {
    let token = cursor
        .next_skip_lineterminator()
        .ok_or(ParseError::AbruptEnd)?;
    let name = if let TokenKind::Identifier(name) = &token.kind {
        name
    } else {
        return Err(ParseError::Expected(
            vec![TokenKind::Identifier("identifier".to_owned())],
            token.clone(),
            Some("formal parameter"),
        ));
    };

    // TODO: Implement initializer.
    Ok(FormalParameter::new(name.clone(), None, false))
}

/// <https://tc39.es/ecma262/#prod-FunctionExpression>
#[derive(Debug, Clone, Copy)]
pub(super) struct FunctionExpression;

impl TokenParser for FunctionExpression {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let name = if let TokenKind::Identifier(name) =
            &cursor.peek(0).ok_or(ParseError::AbruptEnd)?.kind
        {
            Some(name.clone())
        } else {
            None
        };
        if name.is_some() {
            // We move the cursor forward.
            let _ = cursor.next().expect("nex token disappeared");
        }

        cursor.expect_punc(Punctuator::OpenParen, Some("function expression"))?;

        let params = read_formal_parameters(cursor)?;

        let body = Block::parse(cursor)?;

        Ok(Node::FunctionDecl(name, params, Box::new(body)))
    }
}
