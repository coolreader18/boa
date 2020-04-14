use std::{
    error,
    fmt::{Display, Error, Formatter},
    str::FromStr,
};

#[cfg(feature = "serde-ast")]
use serde::{Deserialize, Serialize};

/// A Javascript Keyword
///
/// As specificed by <https://www.ecma-international.org/ecma-262/#sec-keywords>
#[cfg_attr(feature = "serde-ast", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Keyword {
    /// The `await` keyword
    Await,
    /// The `break` keyword
    Break,
    /// The `case` keyword
    Case,
    /// The `catch` keyword
    Catch,
    /// The `class` keyword, which is reserved for future use
    Class,
    /// The `continue` keyword
    Continue,
    /// The `const` keyword
    Const,
    /// The `debugger` keyword
    Debugger,
    /// The `default` keyword
    Default,
    /// The `delete` keyword
    Delete,
    /// The `do` keyword
    Do,
    /// The `else` keyword
    Else,
    /// The `enum` keyword
    Enum,
    /// The `export` keyword
    Export,
    /// The `extends` keyword
    Extends,
    /// The `finally` keyword
    Finally,
    /// The `for` keyword
    For,
    /// The `function` keyword
    Function,
    /// The `if` keyword
    If,
    /// The `in` keyword
    In,
    /// The `instanceof` keyword
    InstanceOf,
    /// The `import` keyword
    Import,
    /// The `let` keyword
    Let,
    /// The `new` keyword
    New,
    /// The `return` keyword
    Return,
    /// The `super` keyword
    Super,
    /// The `switch` keyword
    Switch,
    /// The `this` keyword
    This,
    /// The `throw` keyword
    Throw,
    /// The `try` keyword
    Try,
    /// The `typeof` keyword
    TypeOf,
    /// The `var` keyword
    Var,
    /// The `void` keyword
    Void,
    /// The `while` keyword
    While,
    /// The `with` keyword
    With,
    /// The 'yield' keyword
    Yield,
}

#[derive(Debug, Clone, Copy)]
pub struct KeywordError;
impl Display for KeywordError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "invalid token")
    }
}

// This is important for other errors to wrap this one.
impl error::Error for KeywordError {
    fn description(&self) -> &str {
        "invalid token"
    }

    fn cause(&self) -> Option<&dyn error::Error> {
        // Generic error, underlying cause isn't tracked.
        None
    }
}
impl FromStr for Keyword {
    type Err = KeywordError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "await" => Ok(Self::Await),
            "break" => Ok(Self::Break),
            "case" => Ok(Self::Case),
            "catch" => Ok(Self::Catch),
            "class" => Ok(Self::Class),
            "continue" => Ok(Self::Continue),
            "const" => Ok(Self::Const),
            "debugger" => Ok(Self::Debugger),
            "default" => Ok(Self::Default),
            "delete" => Ok(Self::Delete),
            "do" => Ok(Self::Do),
            "else" => Ok(Self::Else),
            "enum" => Ok(Self::Enum),
            "extends" => Ok(Self::Extends),
            "export" => Ok(Self::Export),
            "finally" => Ok(Self::Finally),
            "for" => Ok(Self::For),
            "function" => Ok(Self::Function),
            "if" => Ok(Self::If),
            "in" => Ok(Self::In),
            "instanceof" => Ok(Self::InstanceOf),
            "import" => Ok(Self::Import),
            "let" => Ok(Self::Let),
            "new" => Ok(Self::New),
            "return" => Ok(Self::Return),
            "super" => Ok(Self::Super),
            "switch" => Ok(Self::Switch),
            "this" => Ok(Self::This),
            "throw" => Ok(Self::Throw),
            "try" => Ok(Self::Try),
            "typeof" => Ok(Self::TypeOf),
            "var" => Ok(Self::Var),
            "void" => Ok(Self::Void),
            "while" => Ok(Self::While),
            "with" => Ok(Self::With),
            "yield" => Ok(Self::Yield),
            _ => Err(KeywordError),
        }
    }
}
impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(
            f,
            "{}",
            match *self {
                Self::Await => "await",
                Self::Break => "break",
                Self::Case => "case",
                Self::Catch => "catch",
                Self::Class => "class",
                Self::Continue => "continue",
                Self::Const => "const",
                Self::Debugger => "debugger",
                Self::Default => "default",
                Self::Delete => "delete",
                Self::Do => "do",
                Self::Else => "else",
                Self::Enum => "enum",
                Self::Extends => "extends",
                Self::Export => "export",
                Self::Finally => "finally",
                Self::For => "for",
                Self::Function => "function",
                Self::If => "if",
                Self::In => "in",
                Self::InstanceOf => "instanceof",
                Self::Import => "import",
                Self::Let => "let",
                Self::New => "new",
                Self::Return => "return",
                Self::Super => "super",
                Self::Switch => "switch",
                Self::This => "this",
                Self::Throw => "throw",
                Self::Try => "try",
                Self::TypeOf => "typeof",
                Self::Var => "var",
                Self::Void => "void",
                Self::While => "while",
                Self::With => "with",
                Self::Yield => "yield",
            }
        )
    }
}
