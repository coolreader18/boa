//! Statement and declaration parsing.
//!
//! More information:
//!  - [MDN documentation][mdn]
//!  - [ECMAScript specification][spec]
//!
//! [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements
//! [spec]: https://tc39.es/ecma262/#sec-ecmascript-language-statements-and-declarations

use super::{
    cursor::Cursor,
    error::{ParseError, ParseResult},
    read_formal_parameters, AssignmentExpression, Expression, TokenParser,
};
use crate::syntax::ast::{
    constant::Const, keyword::Keyword, node::Node, punc::Punctuator, token::TokenKind,
};

type ExpressionStatement = Expression;
type BlockStatement = Block;

/// Statement parsing.
///
/// This can be one of the following:
///
///  - `BlockStatement`
///  - `VariableStatement`
///  - `EmptyStatement`
///  - `ExpressionStatement`
///  - `IfStatement`
///  - `BreakableStatement`
///  - `ContinueStatement`
///  - `BreakStatement`
///  - `ReturnStatement`
///  - `WithStatement`
///  - `LabelledStatement`
///  - `ThrowStatement`
///  - `TryStatement`
///  - `DebuggerStatement`
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements
/// [spec]: https://tc39.es/ecma262/#prod-Statement
#[derive(Debug, Clone, Copy)]
pub(super) struct Statement;

impl TokenParser for Statement {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        // TODO: add BreakableStatement and divide Whiles, fors and so on to another place.
        cursor.skip(|tk| tk.kind == TokenKind::LineTerminator);
        let tok = cursor.peek(0).ok_or(ParseError::AbruptEnd)?;

        let mut is_expression_statement = false;
        let stmt = match tok.kind {
            TokenKind::Keyword(Keyword::If) => IfStatement::parse(cursor),
            TokenKind::Keyword(Keyword::Var) => VariableStatement::parse(cursor),
            TokenKind::Keyword(Keyword::While) => WhileStatement::parse(cursor),
            TokenKind::Keyword(Keyword::Do) => DoWhileStatement::parse(cursor),
            TokenKind::Keyword(Keyword::For) => ForStatement::parse(cursor),
            TokenKind::Keyword(Keyword::Return) => ReturnStatement::parse(cursor),
            TokenKind::Keyword(Keyword::Break) => BreakStatement::parse(cursor),
            TokenKind::Keyword(Keyword::Continue) => ContinueStatement::parse(cursor),
            TokenKind::Keyword(Keyword::Try) => TryStatement::parse(cursor),
            TokenKind::Keyword(Keyword::Throw) => ThrowStatement::parse(cursor),
            TokenKind::Keyword(Keyword::Switch) => SwitchStatement::parse(cursor),
            TokenKind::Punctuator(Punctuator::OpenBlock) => BlockStatement::parse(cursor),
            // TODO: https://tc39.es/ecma262/#prod-LabelledStatement
            // TokenKind::Punctuator(Punctuator::Semicolon) => {
            //     return Ok(Node::new(NodeBase::Nope, tok.pos))
            // }
            _ => {
                is_expression_statement = true;
                ExpressionStatement::parse(cursor)
            }
        };

        if cursor
            .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::Semicolon))
            .is_none()
            && is_expression_statement
        {
            if let Some(tok) = cursor.peek(0) {
                if tok.kind != TokenKind::LineTerminator
                    && tok.kind != TokenKind::Punctuator(Punctuator::CloseBlock)
                {
                    return Err(ParseError::Expected(
                        vec![
                            TokenKind::Punctuator(Punctuator::Semicolon),
                            TokenKind::Punctuator(Punctuator::CloseBlock),
                            TokenKind::LineTerminator,
                        ],
                        tok.clone(),
                        None,
                    ));
                }
            }
        }

        stmt
    }
}

/// If statement parsing.
///
/// An _If_ statement will have a condition, a block statemet, and an optional _else_ statement.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/if...else
/// [spec]: https://tc39.es/ecma262/#sec-if-statement
#[derive(Debug, Clone, Copy)]
struct IfStatement;

impl TokenParser for IfStatement {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        cursor.expect(TokenKind::Keyword(Keyword::If), Some("if statement"))?;
        cursor.expect_punc(Punctuator::OpenParen, Some("if statement"))?;

        let cond = Expression::parse(cursor)?;

        cursor.expect_punc(Punctuator::CloseParen, Some("if statement"))?;

        let then_stm = Statement::parse(cursor)?;

        let else_stm = match cursor.next() {
            Some(else_tok) if else_tok.kind == TokenKind::Keyword(Keyword::Else) => {
                Some(Box::new(Statement::parse(cursor)?))
            }
            _ => {
                cursor.back();
                None
            }
        };

        Ok(Node::If(Box::new(cond), Box::new(then_stm), else_stm))
    }
}

/// Variable statement parsing.
///
/// A varible statement contains the `var` keyword.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/var
/// [spec]: https://tc39.es/ecma262/#prod-VariableStatement
#[derive(Debug, Clone, Copy)]
struct VariableStatement;

impl TokenParser for VariableStatement {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        cursor.expect(TokenKind::Keyword(Keyword::Var), Some("variable statement"))?;

        let decl_list = VariableDeclarationList::parse(cursor)?;

        cursor.expect_semicolon(Some("variable statement"))?;

        Ok(decl_list)
    }
}

/// Variable declaration list parsing.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/var
/// [spec]: https://tc39.es/ecma262/#prod-VariableDeclarationList
#[derive(Debug, Clone, Copy)]
struct VariableDeclarationList;

impl VariableDeclarationList {
    fn read_declaration(cursor: &mut Cursor<'_>) -> Result<(String, Option<Node>), ParseError> {
        let tok = cursor
            .next_skip_lineterminator()
            .ok_or(ParseError::AbruptEnd)?;
        let name = if let TokenKind::Identifier(name) = &tok.kind {
            name.clone()
        } else {
            return Err(ParseError::Expected(
                vec![TokenKind::Identifier("identifier".to_string())],
                tok.clone(),
                Some("variable declaration"),
            ));
        };

        match cursor.peek(0) {
            Some(tk) if tk.kind == TokenKind::Punctuator(Punctuator::Assign) => {
                Ok((name, Some(Initializer::parse(cursor)?)))
            }
            _ => Ok((name, None)),
        }
    }
}

impl TokenParser for VariableDeclarationList {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let mut list = Vec::new();

        loop {
            list.push(Self::read_declaration(cursor)?);
            if !lexical_declaration_continuation(cursor)? {
                break;
            }
        }

        Ok(Node::VarDecl(list))
    }
}

/// Initializer parsing.
///
/// More information:
///  - [ECMAScript specification][spec]
///
/// [spec]: https://tc39.es/ecma262/#prod-Initializer
#[derive(Debug, Clone, Copy)]
struct Initializer;

impl TokenParser for Initializer {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        cursor.expect_punc(Punctuator::Assign, Some("initializer"))?;
        AssignmentExpression::parse(cursor)
    }
}

/// Variable declaration list parsing.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/block
/// [spec]: https://tc39.es/ecma262/#prod-algorithm-conventions-Block
#[derive(Debug, Clone, Copy)]
pub(super) struct Block;

impl TokenParser for Block {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        cursor.expect_punc(Punctuator::OpenBlock, Some("block"))?;
        let statement_list = read_statements(cursor, true).map(Node::StatementList)?;
        cursor.expect_punc(Punctuator::CloseBlock, Some("block"))?;

        Ok(statement_list)
    }
}

/// Reads a list of statements.
///
/// If `break_when_closingbrase` is `true`, it will stop as soon as it finds a `}` character.
pub(super) fn read_statements(
    cursor: &mut Cursor<'_>,
    break_when_closingbrase: bool,
) -> Result<Vec<Node>, ParseError> {
    let mut items = Vec::new();

    loop {
        cursor.skip(|tk| tk.kind == TokenKind::LineTerminator);
        match cursor.peek(0) {
            Some(token) if token.kind == TokenKind::Punctuator(Punctuator::CloseBlock) => {
                if break_when_closingbrase {
                    break;
                } else {
                    return Err(ParseError::Unexpected(token.clone(), None));
                }
            }
            None => {
                if break_when_closingbrase {
                    return Err(ParseError::AbruptEnd);
                } else {
                    break;
                }
            }
            _ => {}
        }

        let item = StatementListItem::parse(cursor)?;
        items.push(item);

        // move the cursor forward for any consecutive semicolon.
        while cursor
            .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::Semicolon))
            .is_some()
        {}
    }

    Ok(items)
}

/// Reads an individual statement list item.
///
/// A statement list item can either be an statement or a declaration.
///
/// More information:
///  - ECMAScript reference: <https://tc39.es/ecma262/#prod-StatementListItem>.
///  - MDN information page about statements and declarations:
/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements>.
#[derive(Debug, Clone, Copy)]
struct StatementListItem;

impl TokenParser for StatementListItem {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let tok = cursor
            .peek_skip_lineterminator()
            .ok_or(ParseError::AbruptEnd)?;

        match tok.kind {
            TokenKind::Keyword(Keyword::Function)
            | TokenKind::Keyword(Keyword::Const)
            | TokenKind::Keyword(Keyword::Let) => Declaration::parse(cursor),
            _ => Statement::parse(cursor),
        }
    }
}

/// Parses a declaration.
///
/// More information:: <https://tc39.es/ecma262/#prod-Declaration>
#[derive(Debug, Clone, Copy)]
struct Declaration;

impl TokenParser for Declaration {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let tok = cursor
            .next_skip_lineterminator()
            .ok_or(ParseError::AbruptEnd)?;

        match tok.kind {
            TokenKind::Keyword(Keyword::Function) => FunctionDeclaration::parse(cursor),
            TokenKind::Keyword(Keyword::Const) => Self::read_binding_list(cursor, true),
            TokenKind::Keyword(Keyword::Let) => Self::read_binding_list(cursor, false),
            _ => unreachable!("unknown token found"),
        }
    }
}

impl Declaration {
    /// Reads a binding list.
    ///
    /// It will return an error if a `const` declaration is being parsed and there is no
    /// initializer.
    ///
    /// More information: <https://tc39.es/ecma262/#prod-BindingList>.
    fn read_binding_list(cursor: &mut Cursor<'_>, is_const: bool) -> ParseResult {
        // Create vectors to store the variable declarations
        // Const and Let signatures are slightly different, Const needs definitions, Lets don't
        let mut let_decls = Vec::new();
        let mut const_decls = Vec::new();

        loop {
            let token = cursor
                .next_skip_lineterminator()
                .ok_or(ParseError::AbruptEnd)?;
            let name = if let TokenKind::Identifier(ref name) = token.kind {
                name.clone()
            } else {
                return Err(ParseError::Expected(
                    vec![TokenKind::Identifier("identifier".to_owned())],
                    token.clone(),
                    if is_const {
                        Some("const declaration")
                    } else {
                        Some("let declaration")
                    },
                ));
            };

            cursor.skip(|tk| tk.kind == TokenKind::LineTerminator);
            match cursor.peek(0) {
                Some(token) if token.kind == TokenKind::Punctuator(Punctuator::Assign) => {
                    let init = Some(Initializer::parse(cursor)?);
                    if is_const {
                        const_decls.push((name, init.unwrap()));
                    } else {
                        let_decls.push((name, init));
                    };
                }
                _ => {
                    if is_const {
                        return Err(ParseError::Expected(
                            vec![TokenKind::Punctuator(Punctuator::Assign)],
                            cursor
                                .next_skip_lineterminator()
                                .ok_or(ParseError::AbruptEnd)?
                                .clone(),
                            Some("const declaration"),
                        ));
                    } else {
                        let_decls.push((name, None));
                    }
                }
            }

            if !lexical_declaration_continuation(cursor)? {
                break;
            }
        }

        if is_const {
            Ok(Node::ConstDecl(const_decls))
        } else {
            Ok(Node::LetDecl(let_decls))
        }
    }
}

/// Parses a function declaration.
///
/// More information:
///  - ECMAScript specification: <https://tc39.es/ecma262/#prod-FunctionDeclaration>.
///  - MDN documentation:
/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/function>
#[derive(Debug, Clone, Copy)]
struct FunctionDeclaration;

impl TokenParser for FunctionDeclaration {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let token = cursor
            .next_skip_lineterminator()
            .ok_or(ParseError::AbruptEnd)?;
        let name = if let TokenKind::Identifier(name) = &token.kind {
            name.clone()
        } else {
            return Err(ParseError::Expected(
                vec![TokenKind::Identifier(String::from("function name"))],
                token.clone(),
                Some("function declaration"),
            ));
        };

        cursor.expect(
            TokenKind::Punctuator(Punctuator::OpenParen),
            Some("function declaration"),
        )?;

        let params = read_formal_parameters(cursor)?;

        let body = Block::parse(cursor)?;

        Ok(Node::FunctionDecl(Some(name), params, Box::new(body)))
    }
}

/// <https://tc39.es/ecma262/#prod-ReturnStatement>
#[derive(Debug, Clone, Copy)]
struct ReturnStatement;

impl TokenParser for ReturnStatement {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        cursor.expect(
            TokenKind::Keyword(Keyword::Return),
            Some("return statement"),
        )?;

        if let Some(tok) = cursor.peek(0) {
            match tok.kind {
                TokenKind::LineTerminator | TokenKind::Punctuator(Punctuator::Semicolon) => {
                    let _ = cursor.next();
                    return Ok(Node::Return(None));
                }
                TokenKind::Punctuator(Punctuator::CloseBlock) => {
                    return Ok(Node::Return(None));
                }
                _ => {}
            }
        }

        let expr = Expression::parse(cursor)?;

        cursor.expect_semicolon(Some("return statement"))?;

        Ok(Node::Return(Some(Box::new(expr))))
    }
}

/// <https://tc39.es/ecma262/#sec-while-statement>
#[derive(Debug, Clone, Copy)]
struct WhileStatement;

impl TokenParser for WhileStatement {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        cursor.expect(TokenKind::Keyword(Keyword::While), Some("while statement"))?;
        cursor.expect_punc(Punctuator::OpenParen, Some("while statement"))?;

        let cond = Expression::parse(cursor)?;

        cursor.expect_punc(Punctuator::CloseParen, Some("while statement"))?;

        let body = Statement::parse(cursor)?;

        Ok(Node::WhileLoop(Box::new(cond), Box::new(body)))
    }
}

/// Checks if the lexical declaration continues with more bindings.
///
/// If it does, it will advance the internal cursor to the next identifier token.
/// A Lexical Declaration continues its binding list if we find a `,` character. A New line
/// indicates the same as a `;`.
///
/// More information: <https://tc39.es/ecma262/#prod-LexicalDeclaration>.
fn lexical_declaration_continuation(cursor: &mut Cursor<'_>) -> Result<bool, ParseError> {
    if let Some(tok) = cursor.peek(0) {
        match tok.kind {
            TokenKind::LineTerminator | TokenKind::Punctuator(Punctuator::Semicolon) => Ok(false),
            TokenKind::Punctuator(Punctuator::Comma) => {
                let _ = cursor
                    .next_skip_lineterminator()
                    .ok_or(ParseError::AbruptEnd)?;
                Ok(true)
            }
            _ => Err(ParseError::Expected(
                vec![
                    TokenKind::Punctuator(Punctuator::Semicolon),
                    TokenKind::LineTerminator,
                ],
                cursor.next().ok_or(ParseError::AbruptEnd)?.clone(),
                Some("lexical declaration"),
            )),
        }
    } else {
        Ok(false)
    }
}

/// <https://tc39.es/ecma262/#sec-break-statement>
#[derive(Debug, Clone, Copy)]
struct BreakStatement;

impl TokenParser for BreakStatement {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        cursor.expect(TokenKind::Keyword(Keyword::Break), Some("break statement"))?;

        let tok = cursor.next().ok_or(ParseError::AbruptEnd)?;
        match &tok.kind {
            TokenKind::LineTerminator
            | TokenKind::Punctuator(Punctuator::Semicolon)
            | TokenKind::Punctuator(Punctuator::CloseBlock) => {
                cursor.back();
                Ok(Node::Break(None))
            }
            TokenKind::Identifier(name) => Ok(Node::Break(Some(name.clone()))),
            _ => Err(ParseError::Expected(
                vec![
                    TokenKind::Punctuator(Punctuator::Semicolon),
                    TokenKind::Punctuator(Punctuator::CloseBlock),
                    TokenKind::LineTerminator,
                    TokenKind::Identifier("identifier".to_owned()),
                ],
                tok.clone(),
                Some("break statement"),
            )),
        }
    }
}

/// <https://tc39.es/ecma262/#sec-for-statement>
#[derive(Debug, Clone, Copy)]
struct ForStatement;

impl TokenParser for ForStatement {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        cursor.expect(TokenKind::Keyword(Keyword::For), Some("for statement"))?;
        cursor.expect_punc(Punctuator::OpenParen, Some("for statement"))?;

        let init = match cursor.peek(0).ok_or(ParseError::AbruptEnd)?.kind {
            TokenKind::Keyword(Keyword::Var) => {
                Some(Box::new(VariableDeclarationList::parse(cursor)?))
            }
            TokenKind::Keyword(Keyword::Let) | TokenKind::Keyword(Keyword::Const) => {
                Some(Box::new(Declaration::parse(cursor)?))
            }
            TokenKind::Punctuator(Punctuator::Semicolon) => None,
            _ => Some(Box::new(Expression::parse(cursor)?)),
        };

        cursor.expect_punc(Punctuator::Semicolon, Some("for statement"))?;

        let cond = if cursor
            .next_if(TokenKind::Punctuator(Punctuator::Semicolon))
            .is_some()
        {
            Some(Box::new(Node::Const(Const::Bool(true))))
        } else {
            let step = Some(Box::new(Expression::parse(cursor)?));
            cursor.expect_punc(Punctuator::Semicolon, Some("for statement"))?;
            step
        };

        let step = if cursor
            .next_if(TokenKind::Punctuator(Punctuator::CloseParen))
            .is_some()
        {
            None
        } else {
            let step = Expression::parse(cursor)?;
            cursor.expect(
                TokenKind::Punctuator(Punctuator::CloseParen),
                Some("for statement"),
            )?;
            Some(Box::new(step))
        };

        let body = Box::new(Statement::parse(cursor)?);

        let for_node = Node::ForLoop(init, cond, step, body);

        Ok(Node::Block(vec![for_node]))
    }
}

/// <https://tc39.es/ecma262/#sec-continue-statement>
#[derive(Debug, Clone, Copy)]
struct ContinueStatement;

impl TokenParser for ContinueStatement {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        cursor.expect(
            TokenKind::Keyword(Keyword::Continue),
            Some("continue statement"),
        )?;

        let tok = cursor.next().ok_or(ParseError::AbruptEnd)?;
        match &tok.kind {
            TokenKind::LineTerminator
            | TokenKind::Punctuator(Punctuator::Semicolon)
            | TokenKind::Punctuator(Punctuator::CloseBlock) => {
                cursor.back();
                Ok(Node::Continue(None))
            }
            TokenKind::Identifier(name) => Ok(Node::Continue(Some(name.clone()))),
            _ => Err(ParseError::Expected(
                vec![
                    TokenKind::Punctuator(Punctuator::Semicolon),
                    TokenKind::LineTerminator,
                    TokenKind::Punctuator(Punctuator::CloseBlock),
                ],
                tok.clone(),
                Some("continue statement"),
            )),
        }
    }
}

/// <https://tc39.es/ecma262/#prod-ThrowStatement>
#[derive(Debug, Clone, Copy)]
struct ThrowStatement;

impl TokenParser for ThrowStatement {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        cursor.expect(TokenKind::Keyword(Keyword::Throw), Some("throw statement"))?;

        if let Some(tok) = cursor.peek(0) {
            match tok.kind {
                TokenKind::LineTerminator // no `LineTerminator` here
                | TokenKind::Punctuator(Punctuator::Semicolon)
                | TokenKind::Punctuator(Punctuator::CloseBlock) => {
                    return Err(ParseError::Unexpected(tok.clone(), Some("throw statement")));
                }
                _ => {}
            }
        }

        let expr = Expression::parse(cursor)?;
        if let Some(tok) = cursor.peek(0) {
            if tok.kind == TokenKind::Punctuator(Punctuator::Semicolon) {
                let _ = cursor.next();
            }
        }

        Ok(Node::Throw(Box::new(expr)))
    }
}

/// <https://tc39.es/ecma262/#sec-try-statement>
#[derive(Debug, Clone, Copy)]
struct TryStatement;

impl TokenParser for TryStatement {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        // TRY
        cursor.expect(TokenKind::Keyword(Keyword::Try), Some("try statement"))?;

        let try_clause = Block::parse(cursor)?;

        let next_token = cursor
            .peek_skip_lineterminator()
            .ok_or(ParseError::AbruptEnd)?;

        if next_token.kind != TokenKind::Keyword(Keyword::Catch)
            && next_token.kind != TokenKind::Keyword(Keyword::Finally)
        {
            return Err(ParseError::Expected(
                vec![
                    TokenKind::Keyword(Keyword::Catch),
                    TokenKind::Keyword(Keyword::Finally),
                ],
                next_token.clone(),
                Some("try statement"),
            ));
        }

        // CATCH
        let (catch, param) = if next_token.kind == TokenKind::Keyword(Keyword::Catch) {
            cursor.skip(|tk| tk.kind == TokenKind::LineTerminator); // Advance the cursor

            // Catch binding
            cursor.expect_punc(Punctuator::OpenParen, Some("catch in try statement"))?;
            // TODO: should accept BindingPattern
            let tok = cursor.next().ok_or(ParseError::AbruptEnd)?;
            let catch_param = if let TokenKind::Identifier(s) = &tok.kind {
                Node::Local(s.clone())
            } else {
                return Err(ParseError::Expected(
                    vec![TokenKind::Identifier("identifier".to_owned())],
                    tok.clone(),
                    Some("catch in try statement"),
                ));
            };
            cursor.expect_punc(Punctuator::CloseParen, Some("catch in try statement"))?;

            // Catch block
            (
                Some(Box::new(Block::parse(cursor)?)),
                Some(Box::new(catch_param)),
            )
        } else {
            (None, None)
        };

        // FINALLY
        let finally_block = if cursor
            .next_if_skip_lineterminator(TokenKind::Keyword(Keyword::Finally))
            .is_some()
        {
            Some(Box::new(Block::parse(cursor)?))
        } else {
            None
        };

        Ok(Node::Try(Box::new(try_clause), catch, param, finally_block))
    }
}

/// <https://tc39.es/ecma262/#prod-SwitchStatement>
#[derive(Debug, Clone, Copy)]
struct SwitchStatement;

impl TokenParser for SwitchStatement {
    fn parse(_cursor: &mut Cursor<'_>) -> ParseResult {
        // TODO: Reimplement the switch statement in the new parser.
        unimplemented!("Switch statement parsing is not implemented");
    }
}

/// <https://tc39.es/ecma262/#sec-do-while-statement>
#[derive(Debug, Clone, Copy)]
struct DoWhileStatement;

impl TokenParser for DoWhileStatement {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let body = Statement::parse(cursor)?;

        let next_token = cursor
            .peek_skip_lineterminator()
            .ok_or(ParseError::AbruptEnd)?;

        if next_token.kind != TokenKind::Keyword(Keyword::While) {
            return Err(ParseError::Expected(
                vec![TokenKind::Keyword(Keyword::While)],
                next_token.clone(),
                Some("do while statement"),
            ));
        }

        let _ = cursor.next_skip_lineterminator(); // skip while token

        cursor.expect_punc(Punctuator::OpenParen, Some("do while statement"))?;

        let cond = Expression::parse(cursor)?;

        cursor.expect_punc(Punctuator::CloseParen, Some("do while statement"))?;

        Ok(Node::DoWhileLoop(Box::new(body), Box::new(cond)))
    }
}
