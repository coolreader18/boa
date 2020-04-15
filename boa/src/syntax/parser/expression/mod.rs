//! Expression parsing.
//!
//! More information:
//!  - [MDN documentation][mdn]
//!  - [ECMAScript specification][spec]
//!
//! [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators
//! [spec]: https://tc39.es/ecma262/#sec-ecmascript-language-expressions

use super::{
    read_formal_parameters, read_statements, ArrowFunction, Cursor, FunctionExpression, ParseError,
    ParseResult, TokenParser,
};
use crate::syntax::ast::{
    constant::Const,
    keyword::Keyword,
    node::{MethodDefinitionKind, Node, PropertyDefinition},
    op::{BinOp, NumOp, UnaryOp},
    punc::Punctuator,
    token::TokenKind,
};

/// Generates an expression parser.
///
/// This macro has 2 mandatory identifiers:
///  - The `$name` identifier will contain the name of the parsing structure.
///  - The `$lower` identifier will contain the parser for lower level expressions.
///
/// Those exressions are divided by the punctuators passed as the third parameter.
macro_rules! expression { ($name:ident, $lower:ident, [$( $op:path ),*] ) => {
    impl TokenParser for $name {
        fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
            let mut lhs = $lower::parse(cursor)?;
            while let Some(tok) = cursor.peek_skip_lineterminator() {
                match tok.kind {
                    TokenKind::Punctuator(op) if $( op == $op )||* => {
                        let _ = cursor.next_skip_lineterminator().expect("token disappeared");
                        lhs = Node::bin_op(
                            op.as_binop().expect("could not get binary operation"),
                            lhs,
                            $lower::parse(cursor)?
                        )
                    }
                    _ => break
                }
            }
            Ok(lhs)
        }
    }
} }

/// <https://tc39.es/ecma262/#prod-Expression>
#[derive(Debug, Clone, Copy)]
pub(super) struct Expression;

expression!(Expression, AssignmentExpression, [Punctuator::Comma]);

/// Assignment expression parsing.
///
/// This can be one of the following:
///
///  - `ConditionalExpression`
///  - `YieldExpression`
///  - `ArrowFunction`
///  - `AsyncArrowFunction`
///  - `LeftHandSideExpression` `=` `AssignmentExpression`
///  - `LeftHandSideExpression` `AssignmentOperator` `AssignmentExpression`
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Assignment_Operators#Assignment
/// [spec]: https://tc39.es/ecma262/#prod-AssignmentExpression
#[derive(Debug, Clone, Copy)]
pub(super) struct AssignmentExpression;

impl TokenParser for AssignmentExpression {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        // Arrow function
        let next_token = cursor.peek(0).ok_or(ParseError::AbruptEnd)?;
        match next_token.kind {
            // a=>{}
            TokenKind::Identifier(_) => {
                if let Some(tok) = cursor.peek(1) {
                    if tok.kind == TokenKind::Punctuator(Punctuator::Arrow) {
                        return ArrowFunction::parse(cursor);
                    }
                }
            }
            // (a,b)=>{}
            TokenKind::Punctuator(Punctuator::OpenParen) => {
                if let Some(node) = ArrowFunction::try_parse(cursor) {
                    return Ok(node);
                }
            }
            _ => {}
        }

        let mut lhs = ConditionalExpression::parse(cursor)?;
        // let mut lhs = self.read_block()?;

        if let Some(tok) = cursor.next() {
            match tok.kind {
                TokenKind::Punctuator(Punctuator::Assign) => {
                    lhs = Node::assign(lhs, Self::parse(cursor)?)
                }
                TokenKind::Punctuator(p) if p.as_binop().is_some() => {
                    let expr = Self::parse(cursor)?;
                    let binop = p.as_binop().expect("binop disappeared");
                    lhs = Node::bin_op(binop, lhs, expr);
                }
                _ => {
                    cursor.back();
                }
            }
        }

        Ok(lhs)
    }
}

/// Conditional expression parsing.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Conditional_Operator
/// [spec]: https://tc39.es/ecma262/#sec-conditional-operator
#[derive(Debug, Clone, Copy)]
pub(super) struct ConditionalExpression;

impl TokenParser for ConditionalExpression {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        // TODO: coalesce expression
        let lhs = LogicalORExpression::parse(cursor)?;

        if let Some(tok) = cursor.next() {
            if tok.kind == TokenKind::Punctuator(Punctuator::Question) {
                let then_clause = AssignmentExpression::parse(cursor)?;
                cursor.expect_punc(Punctuator::Colon, "conditional expression")?;

                let else_clause = AssignmentExpression::parse(cursor)?;
                return Ok(Node::conditional_op(lhs, then_clause, else_clause));
            } else {
                cursor.back();
            }
        }

        Ok(lhs)
    }
}

/// Parses a logical `OR` expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Logical_Operators#Logical_OR_2
/// [spec]: https://tc39.es/ecma262/#prod-LogicalORExpression
#[derive(Debug, Clone, Copy)]
struct LogicalORExpression;

expression!(
    LogicalORExpression,
    LogicalANDExpression,
    [Punctuator::BoolOr]
);

/// Parses a logical `AND` expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Logical_Operators#Logical_AND_2
/// [spec]: https://tc39.es/ecma262/#prod-LogicalANDExpression
#[derive(Debug, Clone, Copy)]
struct LogicalANDExpression;

expression!(
    LogicalANDExpression,
    BitwiseORExpression,
    [Punctuator::BoolAnd]
);

/// Parses a bitwise `OR` expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Bitwise_Operators#Bitwise_OR
/// [spec]: https://tc39.es/ecma262/#prod-BitwiseORExpression
#[derive(Debug, Clone, Copy)]
struct BitwiseORExpression;

expression!(BitwiseORExpression, BitwiseXORExpression, [Punctuator::Or]);

/// Parses a bitwise `XOR` expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Bitwise_Operators#Bitwise_XOR
/// [spec]: https://tc39.es/ecma262/#prod-BitwiseXORExpression
#[derive(Debug, Clone, Copy)]
struct BitwiseXORExpression;

expression!(
    BitwiseXORExpression,
    BitwiseANDExpression,
    [Punctuator::Xor]
);

/// Parses a bitwise `AND` expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Bitwise_Operators#Bitwise_AND
/// [spec]: https://tc39.es/ecma262/#prod-BitwiseANDExpression
#[derive(Debug, Clone, Copy)]
struct BitwiseANDExpression;

expression!(BitwiseANDExpression, EqualityExpression, [Punctuator::And]);

/// Parses an equality expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Comparison_Operators#Equality_operators
/// [spec]: https://tc39.es/ecma262/#sec-equality-operators
#[derive(Debug, Clone, Copy)]
struct EqualityExpression;

expression!(
    EqualityExpression,
    RelationalExpression,
    [
        Punctuator::Eq,
        Punctuator::NotEq,
        Punctuator::StrictEq,
        Punctuator::StrictNotEq
    ]
);

/// Parses a relational expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Comparison_Operators#Relational_operators
/// [spec]: https://tc39.es/ecma262/#sec-relational-operators
#[derive(Debug, Clone, Copy)]
struct RelationalExpression;

expression!(
    RelationalExpression,
    ShiftExpression,
    [
        Punctuator::LessThan,
        Punctuator::GreaterThan,
        Punctuator::LessThanOrEq,
        Punctuator::GreaterThanOrEq
    ]
);

/// Parses a bitwise shift expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Bitwise_Operators#Bitwise_shift_operators
/// [spec]: https://tc39.es/ecma262/#sec-bitwise-shift-operators
#[derive(Debug, Clone, Copy)]
struct ShiftExpression;

expression!(
    ShiftExpression,
    AdditiveExpression,
    [
        Punctuator::LeftSh,
        Punctuator::RightSh,
        Punctuator::URightSh
    ]
);

/// Parses an additive expression.
///
/// This can be either an addition or a subtraction.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Arithmetic_Operators
/// [spec]: https://tc39.es/ecma262/#sec-additive-operators
#[derive(Debug, Clone, Copy)]
struct AdditiveExpression;

expression!(
    AdditiveExpression,
    MultiplicativeExpression,
    [Punctuator::Add, Punctuator::Sub]
);

/// Parses a multiplicative expression.
///
/// This can be either an addition or a subtraction.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Arithmetic_Operators
/// [spec]: https://tc39.es/ecma262/#sec-multiplicative-operators
#[derive(Debug, Clone, Copy)]
struct MultiplicativeExpression;

expression!(
    MultiplicativeExpression,
    ExponentiationExpression,
    [Punctuator::Mul, Punctuator::Div, Punctuator::Mod]
);

/// <https://tc39.es/ecma262/#prod-MultiplicativeExpression>
#[derive(Debug, Clone, Copy)]
struct ExponentiationExpression;

impl ExponentiationExpression {
    /// Checks by looking at the next token to see whether it's a unary operator or not.
    fn is_unary_expression(cursor: &mut Cursor<'_>) -> bool {
        if let Some(tok) = cursor.peek(0) {
            match tok.kind {
                TokenKind::Keyword(Keyword::Delete)
                | TokenKind::Keyword(Keyword::Void)
                | TokenKind::Keyword(Keyword::TypeOf)
                | TokenKind::Punctuator(Punctuator::Add)
                | TokenKind::Punctuator(Punctuator::Sub)
                | TokenKind::Punctuator(Punctuator::Not)
                | TokenKind::Punctuator(Punctuator::Neg) => true,
                _ => false,
            }
        } else {
            false
        }
    }
}

impl TokenParser for ExponentiationExpression {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        if Self::is_unary_expression(cursor) {
            return UnaryExpression::parse(cursor);
        }

        let lhs = UpdateExpression::parse(cursor)?;
        if let Some(tok) = cursor.next() {
            if let TokenKind::Punctuator(Punctuator::Exp) = tok.kind {
                return Ok(Node::bin_op(
                    BinOp::Num(NumOp::Exp),
                    lhs,
                    Self::parse(cursor)?,
                ));
            } else {
                cursor.back();
            }
        }
        Ok(lhs)
    }
}

/// <https://tc39.es/ecma262/#prod-UnaryExpression>
#[derive(Debug, Clone, Copy)]
struct UnaryExpression;

impl TokenParser for UnaryExpression {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let tok = cursor.next().ok_or(ParseError::AbruptEnd)?;
        match tok.kind {
            TokenKind::Keyword(Keyword::Delete) => {
                Ok(Node::unary_op(UnaryOp::Delete, Self::parse(cursor)?))
            }
            TokenKind::Keyword(Keyword::Void) => {
                Ok(Node::unary_op(UnaryOp::Void, Self::parse(cursor)?))
            }
            TokenKind::Keyword(Keyword::TypeOf) => {
                Ok(Node::unary_op(UnaryOp::TypeOf, Self::parse(cursor)?))
            }
            TokenKind::Punctuator(Punctuator::Add) => {
                Ok(Node::unary_op(UnaryOp::Plus, Self::parse(cursor)?))
            }
            TokenKind::Punctuator(Punctuator::Sub) => {
                Ok(Node::unary_op(UnaryOp::Minus, Self::parse(cursor)?))
            }
            TokenKind::Punctuator(Punctuator::Neg) => {
                Ok(Node::unary_op(UnaryOp::Tilde, Self::parse(cursor)?))
            }
            TokenKind::Punctuator(Punctuator::Not) => {
                Ok(Node::unary_op(UnaryOp::Not, Self::parse(cursor)?))
            }
            _ => {
                cursor.back();
                UpdateExpression::parse(cursor)
            }
        }
    }
}

/// <https://tc39.es/ecma262/#prod-UpdateExpression>
#[derive(Debug, Clone, Copy)]
struct UpdateExpression;

impl TokenParser for UpdateExpression {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let tok = cursor
            .peek_skip_lineterminator()
            .ok_or(ParseError::AbruptEnd)?;
        match tok.kind {
            TokenKind::Punctuator(Punctuator::Inc) => {
                cursor
                    .next_skip_lineterminator()
                    .expect("token disappeared");
                return Ok(Node::unary_op(
                    UnaryOp::IncrementPre,
                    LeftHandSideExpression::parse(cursor)?,
                ));
            }
            TokenKind::Punctuator(Punctuator::Dec) => {
                cursor
                    .next_skip_lineterminator()
                    .expect("token disappeared");
                return Ok(Node::unary_op(
                    UnaryOp::DecrementPre,
                    LeftHandSideExpression::parse(cursor)?,
                ));
            }
            _ => {}
        }

        let lhs = LeftHandSideExpression::parse(cursor)?;
        if let Some(tok) = cursor.peek(0) {
            match tok.kind {
                TokenKind::Punctuator(Punctuator::Inc) => {
                    cursor.next().expect("token disappeared");
                    return Ok(Node::unary_op(UnaryOp::IncrementPost, lhs));
                }
                TokenKind::Punctuator(Punctuator::Dec) => {
                    cursor.next().expect("token disappeared");
                    return Ok(Node::unary_op(UnaryOp::DecrementPost, lhs));
                }
                _ => {}
            }
        }

        Ok(lhs)
    }
}

/// <https://tc39.es/ecma262/#prod-LeftHandSideExpression>
#[derive(Debug, Clone, Copy)]
struct LeftHandSideExpression;

impl TokenParser for LeftHandSideExpression {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        // TODO: Implement NewExpression: new MemberExpression
        let lhs = MemberExpression::parse(cursor)?;
        match cursor.peek_skip_lineterminator() {
            Some(ref tok) if tok.kind == TokenKind::Punctuator(Punctuator::OpenParen) => {
                read_call_expression(cursor, lhs)
            }
            _ => Ok(lhs), // TODO: is this correct?
        }
    }
}

/// <https://tc39.es/ecma262/#prod-MemberExpression>
#[derive(Debug, Clone, Copy)]
struct MemberExpression;

impl TokenParser for MemberExpression {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let mut lhs = if cursor
            .peek_skip_lineterminator()
            .ok_or(ParseError::AbruptEnd)?
            .kind
            == TokenKind::Keyword(Keyword::New)
        {
            let _ = cursor
                .next_skip_lineterminator()
                .expect("keyword disappeared");
            let lhs = Self::parse(cursor)?;
            cursor.expect_punc(Punctuator::OpenParen, "member expression")?;
            let args = read_arguments(cursor)?;
            let call_node = Node::call(lhs, args);

            Node::new(call_node)
        } else {
            PrimaryExpression::parse(cursor)?
        };
        while let Some(tok) = cursor.peek_skip_lineterminator() {
            match &tok.kind {
                TokenKind::Punctuator(Punctuator::Dot) => {
                    let _ = cursor
                        .next_skip_lineterminator()
                        .ok_or(ParseError::AbruptEnd)?; // We move the cursor forward.
                    match &cursor
                        .next_skip_lineterminator()
                        .ok_or(ParseError::AbruptEnd)?
                        .kind
                    {
                        TokenKind::Identifier(name) => lhs = Node::get_const_field(lhs, name),
                        TokenKind::Keyword(kw) => lhs = Node::get_const_field(lhs, kw.to_string()),
                        _ => {
                            return Err(ParseError::Expected(
                                vec![TokenKind::identifier("identifier")],
                                tok.clone(),
                                "member expression",
                            ));
                        }
                    }
                }
                TokenKind::Punctuator(Punctuator::OpenBracket) => {
                    let _ = cursor
                        .next_skip_lineterminator()
                        .ok_or(ParseError::AbruptEnd)?; // We move the cursor forward.
                    let idx = Expression::parse(cursor)?;
                    cursor.expect_punc(Punctuator::CloseBracket, "member expression")?;
                    lhs = Node::get_field(lhs, idx);
                }
                _ => break,
            }
        }

        Ok(lhs)
    }
}

/// <https://tc39.es/ecma262/#prod-CallExpression>
fn read_call_expression(cursor: &mut Cursor<'_>, first_member_expr: Node) -> ParseResult {
    let mut lhs = first_member_expr;
    if cursor
        .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::OpenParen))
        .is_some()
    {
        let args = read_arguments(cursor)?;
        lhs = Node::call(lhs, args);
    } else {
        let next_token = cursor
            .next_skip_lineterminator()
            .ok_or(ParseError::AbruptEnd)?;
        return Err(ParseError::Expected(
            vec![TokenKind::Punctuator(Punctuator::OpenParen)],
            next_token.clone(),
            "call expression",
        ));
    }

    while let Some(tok) = cursor.peek_skip_lineterminator() {
        match tok.kind {
            TokenKind::Punctuator(Punctuator::OpenParen) => {
                let _ = cursor
                    .next_skip_lineterminator()
                    .ok_or(ParseError::AbruptEnd)?; // We move the cursor.
                let args = read_arguments(cursor)?;
                lhs = Node::call(lhs, args);
            }
            TokenKind::Punctuator(Punctuator::Dot) => {
                let _ = cursor
                    .next_skip_lineterminator()
                    .ok_or(ParseError::AbruptEnd)?; // We move the cursor.
                match &cursor
                    .next_skip_lineterminator()
                    .ok_or(ParseError::AbruptEnd)?
                    .kind
                {
                    TokenKind::Identifier(name) => {
                        lhs = Node::get_const_field(lhs, name);
                    }
                    TokenKind::Keyword(kw) => {
                        lhs = Node::get_const_field(lhs, kw.to_string());
                    }
                    _ => {
                        return Err(ParseError::Expected(
                            vec![TokenKind::identifier("identifier")],
                            tok.clone(),
                            "call expression",
                        ));
                    }
                }
            }
            TokenKind::Punctuator(Punctuator::OpenBracket) => {
                let _ = cursor
                    .next_skip_lineterminator()
                    .ok_or(ParseError::AbruptEnd)?; // We move the cursor.
                let idx = Expression::parse(cursor)?;
                cursor.expect_punc(Punctuator::CloseBracket, "call expression")?;
                lhs = Node::get_field(lhs, idx);
            }
            _ => break,
        }
    }
    Ok(lhs)
}

/// <https://tc39.es/ecma262/#prod-Arguments>
fn read_arguments(cursor: &mut Cursor<'_>) -> Result<Vec<Node>, ParseError> {
    let mut args = Vec::new();
    loop {
        let next_token = cursor
            .next_skip_lineterminator()
            .ok_or(ParseError::AbruptEnd)?;
        match next_token.kind {
            TokenKind::Punctuator(Punctuator::CloseParen) => break,
            TokenKind::Punctuator(Punctuator::Comma) => {
                if args.is_empty() {
                    return Err(ParseError::Unexpected(next_token.clone(), None));
                }

                if cursor
                    .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::CloseParen))
                    .is_some()
                {
                    break;
                }
            }
            _ => {
                if !args.is_empty() {
                    return Err(ParseError::Expected(
                        vec![
                            TokenKind::Punctuator(Punctuator::Comma),
                            TokenKind::Punctuator(Punctuator::CloseParen),
                        ],
                        next_token.clone(),
                        "argument list",
                    ));
                } else {
                    cursor.back();
                }
            }
        }

        if cursor
            .next_if(TokenKind::Punctuator(Punctuator::Spread))
            .is_some()
        {
            args.push(Node::spread(AssignmentExpression::parse(cursor)?));
        } else {
            args.push(AssignmentExpression::parse(cursor)?);
        }
    }
    Ok(args)
}

/// <https://tc39.es/ecma262/#prod-PrimaryExpression>
#[derive(Debug, Clone, Copy)]
struct PrimaryExpression;

impl TokenParser for PrimaryExpression {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let tok = cursor
            .next_skip_lineterminator()
            .ok_or(ParseError::AbruptEnd)?;

        match &tok.kind {
            TokenKind::Keyword(Keyword::This) => Ok(Node::This),
            // TokenKind::Keyword(Keyword::Arguments) => Ok(Node::new(NodeBase::Arguments, tok.pos)),
            TokenKind::Keyword(Keyword::Function) => FunctionExpression::parse(cursor),
            TokenKind::Punctuator(Punctuator::OpenParen) => {
                let expr = Expression::parse(cursor)?;
                cursor.expect_punc(Punctuator::CloseParen, "primary expression")?;
                Ok(expr)
            }
            TokenKind::Punctuator(Punctuator::OpenBracket) => ArrayLiteral::parse(cursor),
            TokenKind::Punctuator(Punctuator::OpenBlock) => ObjectLiteral::parse(cursor),
            TokenKind::BooleanLiteral(boolean) => Ok(Node::const_node(*boolean)),
            // TODO: ADD TokenKind::UndefinedLiteral
            TokenKind::Identifier(ref i) if i == "undefined" => Ok(Node::Const(Const::Undefined)),
            TokenKind::NullLiteral => Ok(Node::Const(Const::Null)),
            TokenKind::Identifier(ident) => Ok(Node::local(ident)),
            TokenKind::StringLiteral(s) => Ok(Node::const_node(s)),
            TokenKind::NumericLiteral(num) => Ok(Node::const_node(*num)),
            TokenKind::RegularExpressionLiteral(body, flags) => Ok(Node::new(Node::call(
                Node::local("RegExp"),
                vec![Node::const_node(body), Node::const_node(flags)],
            ))),
            _ => Err(ParseError::Unexpected(
                tok.clone(),
                Some("primary expression"),
            )),
        }
    }
}

/// <https://tc39.es/ecma262/#prod-ArrayLiteral>
#[derive(Debug, Clone, Copy)]
struct ArrayLiteral;

impl TokenParser for ArrayLiteral {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let mut elements = Vec::new();

        loop {
            // TODO: Support all features.
            while cursor
                .next_if(TokenKind::Punctuator(Punctuator::Comma))
                .is_some()
            {
                elements.push(Node::Const(Const::Undefined));
            }

            if cursor
                .next_if(TokenKind::Punctuator(Punctuator::CloseBracket))
                .is_some()
            {
                break;
            }

            let _ = cursor.peek(0).ok_or(ParseError::AbruptEnd)?; // Check that there are more tokens to read.

            if cursor
                .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::Spread))
                .is_some()
            {
                let node = AssignmentExpression::parse(cursor)?;
                elements.push(Node::spread(node));
            } else {
                elements.push(AssignmentExpression::parse(cursor)?);
            }
            cursor.next_if(TokenKind::Punctuator(Punctuator::Comma));
        }

        Ok(Node::ArrayDecl(elements))
    }
}

/// <https://tc39.es/ecma262/#prod-ObjectLiteral>
#[derive(Debug, Clone, Copy)]
struct ObjectLiteral;

impl TokenParser for ObjectLiteral {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let mut elements = Vec::new();

        loop {
            if cursor
                .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::CloseBlock))
                .is_some()
            {
                break;
            }

            elements.push(Self::read_property_definition(cursor)?);

            if cursor
                .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::CloseBlock))
                .is_some()
            {
                break;
            }

            if cursor
                .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::Comma))
                .is_none()
            {
                let next_token = cursor
                    .next_skip_lineterminator()
                    .ok_or(ParseError::AbruptEnd)?;
                return Err(ParseError::Expected(
                    vec![
                        TokenKind::Punctuator(Punctuator::Comma),
                        TokenKind::Punctuator(Punctuator::CloseBlock),
                    ],
                    next_token.clone(),
                    "object literal",
                ));
            }
        }

        Ok(Node::Object(elements))
    }
}

impl ObjectLiteral {
    /// <https://tc39.es/ecma262/#prod-PropertyDefinition>
    fn read_property_definition(cursor: &mut Cursor<'_>) -> Result<PropertyDefinition, ParseError> {
        fn to_string(kind: &TokenKind) -> String {
            match kind {
                TokenKind::Identifier(name) => name.clone(),
                TokenKind::NumericLiteral(n) => format!("{}", n),
                TokenKind::StringLiteral(s) => s.clone(),
                _ => unimplemented!("{:?}", kind),
            }
        }

        if cursor
            .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::Spread))
            .is_some()
        {
            let node = AssignmentExpression::parse(cursor)?;
            return Ok(PropertyDefinition::SpreadObject(node));
        }

        let prop_name = cursor
            .next_skip_lineterminator()
            .map(|tok| to_string(&tok.kind))
            .ok_or(ParseError::AbruptEnd)?;

        if cursor
            .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::Colon))
            .is_some()
        {
            let val = AssignmentExpression::parse(cursor)?;
            return Ok(PropertyDefinition::Property(prop_name, val));
        }

        // TODO: Split into separate function: read_property_method_definition
        if cursor
            .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::OpenParen))
            .is_some()
        {
            let params = read_formal_parameters(cursor)?;

            cursor.expect_punc(Punctuator::OpenBlock, "method definition")?;

            let body = read_statements(cursor, true).map(Node::StatementList)?;

            cursor.expect_punc(Punctuator::CloseBlock, "method definition")?;

            return Ok(PropertyDefinition::MethodDefinition(
                MethodDefinitionKind::Ordinary,
                prop_name,
                Node::FunctionDecl(None, params, Box::new(body)),
            ));
        }

        // TODO need to revisit this
        // if let TokenKind::Identifier(name) = tok.kind {
        //     if name == "get" || name == "set" {
        //         let may_identifier = self.peek_skip_lineterminator();
        //         if may_identifier.is_some()
        //             && matches!(may_identifier.unwrap().kind, TokenKind::Identifier(_))
        //         {
        //             let f = self.read_function_expression()?;
        //             let func_name = if let NodeBase::FunctionExpr(ref name, _, _) = f.base {
        //                 name.clone().unwrap()
        //             } else {
        //                 panic!()
        //             };
        //             return Ok(PropertyDefinition::MethodDefinition(
        //                 if name == "get" {
        //                     MethodDefinitionKind::Get
        //                 } else {
        //                     MethodDefinitionKind::Set
        //                 },
        //                 func_name,
        //                 f,
        //             ));
        //         }
        //     }

        //     return Ok(PropertyDefinition::IdentifierReference(name));
        // }

        let pos = cursor
            .peek(0)
            .map(|tok| tok.pos)
            .ok_or(ParseError::AbruptEnd)?;
        Err(ParseError::General(
            "expected property definition",
            Some(pos),
        ))
    }
}
