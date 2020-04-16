//! Tests for the parser.

use super::*;
use crate::syntax::{
    ast::node::{FormalParameter, MethodDefinitionKind, Node, PropertyDefinition},
    ast::op::{AssignOp, BinOp, BitOp, CompOp, NumOp, UnaryOp},
    lexer::Lexer,
};

#[allow(clippy::result_unwrap_used)]
fn check_parser(js: &str, expr: &[Node]) {
    let mut lexer = Lexer::new(js);
    lexer.lex().expect("failed to lex");

    assert_eq!(
        Parser::new(&lexer.tokens).parse_all().unwrap(),
        Node::statement_list(expr)
    );
}

fn check_invalid(js: &str) {
    let mut lexer = Lexer::new(js);
    lexer.lex().expect("failed to lex");

    assert!(Parser::new(&lexer.tokens).parse_all().is_err());
}

#[test]
fn check_string() {
    // Check empty string
    check_parser("\"\"", &[Node::const_node("")]);

    // Check non-empty string
    check_parser("\"hello\"", &[Node::const_node("hello")]);
}

#[test]
fn check_object_literal() {
    let object_properties = vec![
        PropertyDefinition::property("a", Node::const_node(true)),
        PropertyDefinition::property("b", Node::const_node(false)),
    ];

    check_parser(
        "const x = {
            a: true,
            b: false,
        };
        ",
        &[Node::const_decl(vec![(
            String::from("x"),
            Node::Object(object_properties),
        )])],
    );
}

#[test]
fn check_object_short_function() {
    // Testing short function syntax
    let object_properties = vec![
        PropertyDefinition::property("a", Node::const_node(true)),
        PropertyDefinition::method_definition(
            MethodDefinitionKind::Ordinary,
            "b",
            Node::function_decl::<_, String, _, _>(None, Vec::new(), Node::StatementList(vec![])),
        ),
    ];

    check_parser(
        "const x = {
            a: true,
            b() {},
        };
        ",
        &[Node::ConstDecl(vec![(
            String::from("x"),
            Node::Object(object_properties),
        )])],
    );
}

#[test]
fn check_object_short_function_arguments() {
    // Testing short function syntax
    let object_properties = vec![
        PropertyDefinition::property("a", Node::const_node(true)),
        PropertyDefinition::method_definition(
            MethodDefinitionKind::Ordinary,
            "b",
            Node::function_decl::<_, String, _, _>(
                None,
                vec![FormalParameter::new("test", None, false)],
                Node::StatementList(Vec::new()),
            ),
        ),
    ];

    check_parser(
        "const x = {
            a: true,
            b(test) {}
         };
        ",
        &[Node::ConstDecl(vec![(
            String::from("x"),
            Node::Object(object_properties),
        )])],
    );
}

#[test]
fn check_array() {
    use crate::syntax::ast::constant::Const;

    // Check empty array
    check_parser("[]", &[Node::ArrayDecl(Vec::new())]);

    // Check array with empty slot
    check_parser(
        "[,]",
        &[Node::ArrayDecl(vec![Node::Const(Const::Undefined)])],
    );

    // Check numeric array
    check_parser(
        "[1, 2, 3]",
        &[Node::ArrayDecl(vec![
            Node::const_node(1.0),
            Node::const_node(2.0),
            Node::const_node(3.0),
        ])],
    );

    // Check numeric array with trailing comma
    check_parser(
        "[1, 2, 3,]",
        &[Node::ArrayDecl(vec![
            Node::const_node(1.0),
            Node::const_node(2.0),
            Node::const_node(3.0),
        ])],
    );

    // Check numeric array with an elision
    check_parser(
        "[1, 2, , 3]",
        &[Node::ArrayDecl(vec![
            Node::const_node(1.0),
            Node::const_node(2.0),
            Node::Const(Const::Undefined),
            Node::const_node(3.0),
        ])],
    );

    // Check numeric array with repeated elision
    check_parser(
        "[1, 2, ,, 3]",
        &[Node::ArrayDecl(vec![
            Node::const_node(1.0),
            Node::const_node(2.0),
            Node::Const(Const::Undefined),
            Node::Const(Const::Undefined),
            Node::const_node(3.0),
        ])],
    );

    // Check combined array
    check_parser(
        "[1, \"a\", 2]",
        &[Node::ArrayDecl(vec![
            Node::const_node(1.0),
            Node::const_node("a"),
            Node::const_node(2.0),
        ])],
    );

    // Check combined array with empty string
    check_parser(
        "[1, \"\", 2]",
        &[Node::ArrayDecl(vec![
            Node::const_node(1.0),
            Node::const_node(""),
            Node::const_node(2.0),
        ])],
    );
}

#[test]
fn check_declarations() {
    // Check `var` declaration
    check_parser(
        "var a = 5;",
        &[Node::VarDecl(vec![(
            String::from("a"),
            Some(Node::const_node(5.0)),
        )])],
    );

    // Check `var` declaration with no spaces
    check_parser(
        "var a=5;",
        &[Node::VarDecl(vec![(
            String::from("a"),
            Some(Node::const_node(5.0)),
        )])],
    );

    // Check empty `var` declaration
    check_parser("var a;", &[Node::VarDecl(vec![(String::from("a"), None)])]);

    // Check multiple `var` declaration
    check_parser(
        "var a = 5, b, c = 6;",
        &[Node::VarDecl(vec![
            (String::from("a"), Some(Node::const_node(5.0))),
            (String::from("b"), None),
            (String::from("c"), Some(Node::const_node(6.0))),
        ])],
    );

    // Check `let` declaration
    check_parser(
        "let a = 5;",
        &[Node::LetDecl(vec![(
            String::from("a"),
            Some(Node::const_node(5.0)),
        )])],
    );

    // Check `let` declaration with no spaces
    check_parser(
        "let a=5;",
        &[Node::LetDecl(vec![(
            String::from("a"),
            Some(Node::const_node(5.0)),
        )])],
    );

    // Check empty `let` declaration
    check_parser("let a;", &[Node::LetDecl(vec![(String::from("a"), None)])]);

    // Check multiple `let` declaration
    check_parser(
        "let a = 5, b, c = 6;",
        &[Node::LetDecl(vec![
            (String::from("a"), Some(Node::const_node(5.0))),
            (String::from("b"), None),
            (String::from("c"), Some(Node::const_node(6.0))),
        ])],
    );

    // Check `const` declaration
    check_parser(
        "const a = 5;",
        &[Node::ConstDecl(vec![(
            String::from("a"),
            Node::const_node(5.0),
        )])],
    );

    // Check `const` declaration with no spaces
    check_parser(
        "const a=5;",
        &[Node::ConstDecl(vec![(
            String::from("a"),
            Node::const_node(5.0),
        )])],
    );

    // Check empty `const` declaration
    check_invalid("const a;");

    // Check multiple `const` declaration
    check_parser(
        "const a = 5, c = 6;",
        &[Node::ConstDecl(vec![
            (String::from("a"), Node::const_node(5.0)),
            (String::from("c"), Node::const_node(6.0)),
        ])],
    );
}

#[test]
fn check_operations() {
    // Check numeric operations
    check_parser(
        "a + b",
        &[Node::bin_op(NumOp::Add, Node::local("a"), Node::local("b"))],
    );
    check_parser(
        "a+1",
        &[Node::bin_op(
            NumOp::Add,
            Node::local("a"),
            Node::const_node(1.0),
        )],
    );
    check_parser(
        "a - b",
        &[Node::bin_op(NumOp::Sub, Node::local("a"), Node::local("b"))],
    );
    check_parser(
        "a-1",
        &[Node::bin_op(
            NumOp::Sub,
            Node::local("a"),
            Node::const_node(1.0),
        )],
    );
    check_parser(
        "a / b",
        &[Node::bin_op(NumOp::Div, Node::local("a"), Node::local("b"))],
    );
    check_parser(
        "a/2",
        &[Node::bin_op(
            NumOp::Div,
            Node::local("a"),
            Node::const_node(2.0),
        )],
    );
    check_parser(
        "a * b",
        &[Node::bin_op(NumOp::Mul, Node::local("a"), Node::local("b"))],
    );
    check_parser(
        "a*2",
        &[Node::bin_op(
            NumOp::Mul,
            Node::local("a"),
            Node::const_node(2.0),
        )],
    );
    check_parser(
        "a ** b",
        &[Node::bin_op(NumOp::Exp, Node::local("a"), Node::local("b"))],
    );
    check_parser(
        "a**2",
        &[Node::bin_op(
            NumOp::Exp,
            Node::local("a"),
            Node::const_node(2.0),
        )],
    );
    check_parser(
        "a % b",
        &[Node::bin_op(NumOp::Mod, Node::local("a"), Node::local("b"))],
    );
    check_parser(
        "a%2",
        &[Node::bin_op(
            NumOp::Mod,
            Node::local("a"),
            Node::const_node(2.0),
        )],
    );

    // Check complex numeric operations
    check_parser(
        "a + d*(b-3)+1",
        &[Node::bin_op(
            NumOp::Add,
            Node::bin_op(
                NumOp::Add,
                Node::local("a"),
                Node::bin_op(
                    NumOp::Mul,
                    Node::local("d"),
                    Node::bin_op(NumOp::Sub, Node::local("b"), Node::const_node(3.0)),
                ),
            ),
            Node::const_node(1.0),
        )],
    );

    // Check bitwise operations
    check_parser(
        "a & b",
        &[Node::bin_op(
            BinOp::Bit(BitOp::And),
            Node::local("a"),
            Node::local("b"),
        )],
    );
    check_parser(
        "a&b",
        &[Node::bin_op(
            BinOp::Bit(BitOp::And),
            Node::local("a"),
            Node::local("b"),
        )],
    );

    check_parser(
        "a | b",
        &[Node::bin_op(
            BinOp::Bit(BitOp::Or),
            Node::local("a"),
            Node::local("b"),
        )],
    );
    check_parser(
        "a|b",
        &[Node::bin_op(
            BinOp::Bit(BitOp::Or),
            Node::local("a"),
            Node::local("b"),
        )],
    );

    check_parser(
        "a ^ b",
        &[Node::bin_op(
            BinOp::Bit(BitOp::Xor),
            Node::local("a"),
            Node::local("b"),
        )],
    );
    check_parser(
        "a^b",
        &[Node::bin_op(
            BinOp::Bit(BitOp::Xor),
            Node::local("a"),
            Node::local("b"),
        )],
    );

    check_parser(
        "a << b",
        &[Node::bin_op(
            BinOp::Bit(BitOp::Shl),
            Node::local("a"),
            Node::local("b"),
        )],
    );
    check_parser(
        "a<<b",
        &[Node::bin_op(
            BinOp::Bit(BitOp::Shl),
            Node::local("a"),
            Node::local("b"),
        )],
    );

    check_parser(
        "a >> b",
        &[Node::bin_op(
            BinOp::Bit(BitOp::Shr),
            Node::local("a"),
            Node::local("b"),
        )],
    );
    check_parser(
        "a>>b",
        &[Node::bin_op(
            BinOp::Bit(BitOp::Shr),
            Node::local("a"),
            Node::local("b"),
        )],
    );

    // Check assign ops
    check_parser(
        "a += b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Add),
            Node::local("a"),
            Node::local("b"),
        )],
    );
    check_parser(
        "a -= b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Sub),
            Node::local("a"),
            Node::local("b"),
        )],
    );
    check_parser(
        "a *= b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Mul),
            Node::local("a"),
            Node::local("b"),
        )],
    );
    check_parser(
        "a **= b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Exp),
            Node::local("a"),
            Node::local("b"),
        )],
    );
    check_parser(
        "a /= b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Div),
            Node::local("a"),
            Node::local("b"),
        )],
    );
    check_parser(
        "a %= b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Mod),
            Node::local("a"),
            Node::local("b"),
        )],
    );
    check_parser(
        "a &= b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::And),
            Node::local("a"),
            Node::local("b"),
        )],
    );
    check_parser(
        "a |= b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Or),
            Node::local("a"),
            Node::local("b"),
        )],
    );
    check_parser(
        "a ^= b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Xor),
            Node::local("a"),
            Node::local("b"),
        )],
    );
    check_parser(
        "a <<= b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Shl),
            Node::local("a"),
            Node::local("b"),
        )],
    );
    check_parser(
        "a >>= b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Shr),
            Node::local("a"),
            Node::local("b"),
        )],
    );
    check_parser(
        "a %= 10 / 2",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Mod),
            Node::local("a"),
            Node::bin_op(NumOp::Div, Node::const_node(10.0), Node::const_node(2.0)),
        )],
    );
}

#[test]
fn check_function_declarations() {
    check_parser(
        "function foo(a) { return a; }",
        &[Node::function_decl(
            "foo",
            vec![FormalParameter::new("a", None, false)],
            Node::StatementList(vec![Node::return_node(Node::local("a"))]),
        )],
    );

    check_parser(
        "function foo(a) { return; }",
        &[Node::function_decl(
            "foo",
            vec![FormalParameter::new("a", None, false)],
            Node::StatementList(vec![Node::Return(None)]),
        )],
    );

    check_parser(
        "function foo(a) { return }",
        &[Node::function_decl(
            "foo",
            vec![FormalParameter::new("a", None, false)],
            Node::StatementList(vec![Node::Return(None)]),
        )],
    );

    check_parser(
        "function foo(a, ...b) {}",
        &[Node::function_decl(
            "foo",
            vec![
                FormalParameter::new("a", None, false),
                FormalParameter::new("b", None, true),
            ],
            Node::StatementList(Vec::new()),
        )],
    );

    check_parser(
        "(...a) => {}",
        &[Node::arrow_function_decl(
            vec![FormalParameter::new("a", None, true)],
            Node::StatementList(Vec::new()),
        )],
    );

    check_parser(
        "(a, b, ...c) => {}",
        &[Node::arrow_function_decl(
            vec![
                FormalParameter::new("a", None, false),
                FormalParameter::new("b", None, false),
                FormalParameter::new("c", None, true),
            ],
            Node::StatementList(Vec::new()),
        )],
    );

    check_parser(
        "(a, b) => { return a + b; }",
        &[Node::arrow_function_decl(
            vec![
                FormalParameter::new("a", None, false),
                FormalParameter::new("b", None, false),
            ],
            Node::StatementList(vec![Node::return_node(Node::bin_op(
                NumOp::Add,
                Node::local("a"),
                Node::local("b"),
            ))]),
        )],
    );

    check_parser(
        "(a, b) => { return; }",
        &[Node::arrow_function_decl(
            vec![
                FormalParameter::new("a", None, false),
                FormalParameter::new("b", None, false),
            ],
            Node::StatementList(vec![Node::Return(None)]),
        )],
    );

    check_parser(
        "(a, b) => { return }",
        &[Node::arrow_function_decl(
            vec![
                FormalParameter::new("a", None, false),
                FormalParameter::new("b", None, false),
            ],
            Node::StatementList(vec![Node::Return(None)]),
        )],
    );
}

#[test]
fn check_do_while() {
    check_parser(
        r#"do {
            a += 1;
        } while (true)"#,
        &[Node::do_while_loop(
            Node::Block(vec![Node::bin_op(
                BinOp::Assign(AssignOp::Add),
                Node::local("a"),
                Node::const_node(1.0),
            )]),
            Node::const_node(true),
        )],
    );

    // Check semicolon insertion after do-while
    check_parser(
        r#"var i = 0;
        do {console.log("hello");} while(i++ < 10) console.log("end");"#,
        &[
            Node::VarDecl(vec![(String::from("i"), Some(Node::const_node(0.0)))]),
            Node::do_while_loop(
                Node::Block(vec![Node::call(
                    Node::get_const_field(Node::local("console"), "log"),
                    vec![Node::const_node("hello")],
                )]),
                Node::bin_op(
                    BinOp::Comp(CompOp::LessThan),
                    Node::unary_op(UnaryOp::IncrementPost, Node::local("i")),
                    Node::const_node(10.0),
                ),
            ),
            Node::call(
                Node::get_const_field(Node::local("console"), "log"),
                vec![Node::const_node("end")],
            ),
        ],
    );
}

#[test]
fn check_break_parsing() {
    check_parser(
        "while (true) break;",
        &[Node::while_loop(Node::const_node(true), Node::Break(None))],
    );

    check_parser(
        "while (true)
            break;",
        &[Node::while_loop(Node::const_node(true), Node::Break(None))],
    );

    check_parser(
        "while (true) {break}",
        &[Node::while_loop(
            Node::const_node(true),
            Node::Block(vec![Node::Break(None)]),
        )],
    );

    check_parser(
        "while (true) {
            break test
        }",
        &[Node::while_loop(
            Node::const_node(true),
            Node::Block(vec![Node::break_node("test")]),
        )],
    );

    check_parser(
        "while (true) {break;}",
        &[Node::while_loop(
            Node::const_node(true),
            Node::Block(vec![Node::Break(None)]),
        )],
    );

    check_parser(
        "while (true) {
            break test;
        }",
        &[Node::while_loop(
            Node::const_node(true),
            Node::Block(vec![Node::break_node("test")]),
        )],
    );
}

/// Should be parsed as `new Class().method()` instead of `new (Class().method())`
#[test]
fn check_construct_call_precedence() {
    check_parser(
        "new Date().getTime()",
        &[Node::call(
            Node::get_const_field(
                Node::new(Node::call(Node::local("Date"), Vec::new())),
                "getTime",
            ),
            Vec::new(),
        )],
    );
}

#[test]
fn check_continue_parsing() {
    check_parser(
        "while (true) continue;",
        &[Node::while_loop(
            Node::const_node(true),
            Node::Continue(None),
        )],
    );

    check_parser(
        "while (true)
            continue;",
        &[Node::while_loop(
            Node::const_node(true),
            Node::Continue(None),
        )],
    );

    check_parser(
        "while (true) {continue}",
        &[Node::while_loop(
            Node::const_node(true),
            Node::Block(vec![Node::Continue(None)]),
        )],
    );

    check_parser(
        "while (true) {
            continue test
        }",
        &[Node::while_loop(
            Node::const_node(true),
            Node::Block(vec![Node::continue_node("test")]),
        )],
    );

    check_parser(
        "while (true) {continue;}",
        &[Node::while_loop(
            Node::const_node(true),
            Node::Block(vec![Node::Continue(None)]),
        )],
    );

    check_parser(
        "while (true) {
            continue test;
        }",
        &[Node::while_loop(
            Node::const_node(true),
            Node::Block(vec![Node::continue_node("test")]),
        )],
    );
}

#[test]
fn assing_operator_precedence() {
    check_parser(
        "a = a + 1",
        &[Node::assign(
            Node::local("a"),
            Node::bin_op(NumOp::Add, Node::local("a"), Node::const_node(1.0)),
        )],
    );
}

#[test]
fn check_throw_parsing() {
    check_parser("throw 'error';", &[Node::throw(Node::const_node("error"))]);
}
