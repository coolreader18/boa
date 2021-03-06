#[cfg(test)]
mod tests;

use crate::{
    builtins::{
        function::NativeFunctionData,
        object::{Object, ObjectKind, PROTOTYPE},
        property::Property,
        regexp::{make_regexp, match_all as regexp_match_all, r#match as regexp_match},
        value::{from_value, to_value, ResultValue, Value, ValueData},
    },
    exec::Interpreter,
};
use gc::Gc;
use regex::Regex;
use std::{
    cmp::{max, min},
    f64::NAN,
    ops::Deref,
};

/// Create new string [[Construct]]
/// <https://searchfox.org/mozilla-central/source/js/src/vm/StringObject.h#19>
// This gets called when a new String() is created, it's called by exec:346
pub fn make_string(this: &Value, args: &[Value], _: &mut Interpreter) -> ResultValue {
    // If we're constructing a string, we should set the initial length
    // To do this we need to convert the string back to a Rust String, then get the .len()
    // let a: String = from_value(args.get(0).expect("failed to get argument for String method").clone()).unwrap();
    // this.set_field_slice("length", to_value(a.len() as i32));

    // This value is used by console.log and other routines to match Obexpecty"failed to parse argument for String method"pe
    // to its Javascript Identifier (global constructor method name)
    this.set_kind(ObjectKind::String);
    this.set_internal_slot(
        "StringData",
        args.get(0)
            .expect("failed to get StringData for make_string()")
            .clone(),
    );
    Ok(this.clone())
}

/// Call new string [[Call]]
/// https://tc39.es/ecma262/#sec-string-constructor-string-value
pub fn call_string(_: &Value, args: &[Value], _: &mut Interpreter) -> ResultValue {
    let arg = match args.get(0) {
        Some(v) => v.clone(),
        None => Gc::new(ValueData::Undefined),
    };

    if arg.is_undefined() {
        return Ok(to_value(""));
    }

    Ok(to_value(arg.to_string()))
}

/// Get the string value to a primitive string
pub fn to_string(this: &Value, _: &[Value], _: &mut Interpreter) -> ResultValue {
    // Get String from String Object and send it back as a new value
    let primitive_val = this.get_internal_slot("StringData");
    Ok(to_value(format!("{}", primitive_val)))
}

/// Returns a single element String containing the code unit at index pos within the String value
/// resulting from converting this object to a String. If there is no element at that index, the
/// result is the empty String. The result is a String value, not a String object.
/// <https://tc39.es/ecma262/#sec-string.prototype.charat>
pub fn char_at(this: &Value, args: &[Value], ctx: &mut Interpreter) -> ResultValue {
    // First we get it the actual string a private field stored on the object only the engine has access to.
    // Then we convert it into a Rust String by wrapping it in from_value
    let primitive_val = ctx.value_to_rust_string(this);
    let pos: i32 = from_value(
        args.get(0)
            .expect("failed to get argument for String method")
            .clone(),
    )
    .expect("failed to parse argument for String method");

    // Calling .len() on a string would give the wrong result, as they are bytes not the number of
    // unicode code points
    // Note that this is an O(N) operation (because UTF-8 is complex) while getting the number of
    // bytes is an O(1) operation.
    let length = primitive_val.chars().count();

    // We should return an empty string is pos is out of range
    if pos >= length as i32 || pos < 0 {
        return Ok(to_value::<String>(String::new()));
    }

    Ok(to_value::<char>(
        primitive_val
            .chars()
            .nth(pos as usize)
            .expect("failed to get value"),
    ))
}

/// Returns a Number (a nonnegative integer less than 216) that is the numeric value of the code
/// unit at index pos within the String resulting from converting this object to a String. If there
/// is no element at that index, the result is NaN.
/// <https://tc39.es/ecma262/#sec-string.prototype.charcodeat>
pub fn char_code_at(this: &Value, args: &[Value], ctx: &mut Interpreter) -> ResultValue {
    // First we get it the actual string a private field stored on the object only the engine has access to.
    // Then we convert it into a Rust String by wrapping it in from_value
    let primitive_val: String = ctx.value_to_rust_string(this);

    // Calling .len() on a string would give the wrong result, as they are bytes not the number of unicode code points
    // Note that this is an O(N) operation (because UTF-8 is complex) while getting the number of bytes is an O(1) operation.
    let length = primitive_val.chars().count();
    let pos: i32 = from_value(
        args.get(0)
            .expect("failed to get argument for String method")
            .clone(),
    )
    .expect("failed to parse argument for String method");

    if pos >= length as i32 || pos < 0 {
        return Ok(to_value(NAN));
    }

    let utf16_val = primitive_val
        .encode_utf16()
        .nth(pos as usize)
        .expect("failed to get utf16 value");
    // If there is no element at that index, the result is NaN
    // TODO: We currently don't have NaN
    Ok(to_value(f64::from(utf16_val)))
}

/// Returns a String that is the result of concatenating this String and all strings provided as
/// arguments
/// <https://tc39.es/ecma262/#sec-string.prototype.concat>
pub fn concat(this: &Value, args: &[Value], ctx: &mut Interpreter) -> ResultValue {
    // First we get it the actual string a private field stored on the object only the engine has access to.
    // Then we convert it into a Rust String by wrapping it in from_value
    let mut new_str = ctx.value_to_rust_string(this);

    for arg in args {
        let concat_str: String = from_value(arg.clone()).expect("failed to get argument value");
        new_str.push_str(&concat_str);
    }

    Ok(to_value(new_str))
}

/// Returns a String that is the result of repeating this String the number of times given by the
/// first argument
/// <https://tc39.es/ecma262/#sec-string.prototype.repeat>
pub fn repeat(this: &Value, args: &[Value], ctx: &mut Interpreter) -> ResultValue {
    // First we get it the actual string a private field stored on the object only the engine has access to.
    // Then we convert it into a Rust String by wrapping it in from_value
    let primitive_val: String = ctx.value_to_rust_string(this);

    let repeat_times: usize = from_value(
        args.get(0)
            .expect("failed to get argument for String method")
            .clone(),
    )
    .expect("failed to parse argument for String method");
    Ok(to_value(primitive_val.repeat(repeat_times)))
}

/// Returns a String which contains the slice of the JS String from character at "start" index up
/// to but not including character at "end" index
/// <https://tc39.es/ecma262/#sec-string.prototype.slice>
pub fn slice(this: &Value, args: &[Value], ctx: &mut Interpreter) -> ResultValue {
    // First we get it the actual string a private field stored on the object only the engine has access to.
    // Then we convert it into a Rust String by wrapping it in from_value
    let primitive_val: String = ctx.value_to_rust_string(this);

    let start: i32 = from_value(
        args.get(0)
            .expect("failed to get argument for String method")
            .clone(),
    )
    .expect("failed to parse argument for String method");
    let end: i32 = from_value(
        args.get(1)
            .expect("failed to get argument in slice")
            .clone(),
    )
    .expect("failed to parse argument");

    // Calling .len() on a string would give the wrong result, as they are bytes not the number of unicode code points
    // Note that this is an O(N) operation (because UTF-8 is complex) while getting the number of bytes is an O(1) operation.
    let length: i32 = primitive_val.chars().count() as i32;

    let from: i32 = if start < 0 {
        max(length.wrapping_add(start), 0)
    } else {
        min(start, length)
    };
    let to: i32 = if end < 0 {
        max(length.wrapping_add(end), 0)
    } else {
        min(end, length)
    };

    let span = max(to.wrapping_sub(from), 0);

    let mut new_str = String::new();
    for i in from..from.wrapping_add(span) {
        new_str.push(
            primitive_val
                .chars()
                .nth(i as usize)
                .expect("Could not get nth char"),
        );
    }
    Ok(to_value(new_str))
}

/// Returns a Boolean indicating whether the sequence of code units of the
/// "search string" is the same as the corresponding code units of this string
/// starting at index "position"
/// <https://tc39.es/ecma262/#sec-string.prototype.startswith>
pub fn starts_with(this: &Value, args: &[Value], ctx: &mut Interpreter) -> ResultValue {
    // First we get it the actual string a private field stored on the object only the engine has access to.
    // Then we convert it into a Rust String by wrapping it in from_value
    let primitive_val: String = ctx.value_to_rust_string(this);

    // TODO: Should throw TypeError if pattern is regular expression
    let search_string: String = from_value(
        args.get(0)
            .expect("failed to get argument for String method")
            .clone(),
    )
    .expect("failed to parse argument for String method");

    let length: i32 = primitive_val.chars().count() as i32;
    let search_length: i32 = search_string.chars().count() as i32;

    // If less than 2 args specified, position is 'undefined', defaults to 0
    let position: i32 = if args.len() < 2 {
        0
    } else {
        from_value(args.get(1).expect("failed to get arg").clone()).expect("failed to get argument")
    };

    let start = min(max(position, 0), length);
    let end = start.wrapping_add(search_length);

    if end > length {
        Ok(to_value(false))
    } else {
        // Only use the part of the string from "start"
        let this_string: String = primitive_val.chars().skip(start as usize).collect();
        Ok(to_value(this_string.starts_with(&search_string)))
    }
}

/// Returns a Boolean indicating whether the sequence of code units of the
/// "search string"  is the same as the corresponding code units of this string
/// starting at position "end position" - length
/// <https://tc39.es/ecma262/#sec-string.prototype.endswith>
pub fn ends_with(this: &Value, args: &[Value], ctx: &mut Interpreter) -> ResultValue {
    // First we get it the actual string a private field stored on the object only the engine has access to.
    // Then we convert it into a Rust String by wrapping it in from_value
    let primitive_val: String = ctx.value_to_rust_string(this);

    // TODO: Should throw TypeError if search_string is regular expression
    let search_string: String = from_value(
        args.get(0)
            .expect("failed to get argument for String method")
            .clone(),
    )
    .expect("failed to parse argument for String method");

    let length: i32 = primitive_val.chars().count() as i32;
    let search_length: i32 = search_string.chars().count() as i32;

    // If less than 2 args specified, end_position is 'undefined', defaults to
    // length of this
    let end_position: i32 = if args.len() < 2 {
        length
    } else {
        from_value(args.get(1).expect("Could not get argumetn").clone())
            .expect("Could not convert value to i32")
    };

    let end = min(max(end_position, 0), length);
    let start = end.wrapping_sub(search_length);

    if start < 0 {
        Ok(to_value(false))
    } else {
        // Only use the part of the string up to "end"
        let this_string: String = primitive_val.chars().take(end as usize).collect();
        Ok(to_value(this_string.ends_with(&search_string)))
    }
}

/// Returns a Boolean indicating whether searchString appears as a substring of
/// the result of converting this object to a String, at one or more indices
/// that are greater than or equal to position. If position is undefined, 0 is
/// assumed, so as to search all of the String.
/// <https://tc39.es/ecma262/#sec-string.prototype.includes>
pub fn includes(this: &Value, args: &[Value], ctx: &mut Interpreter) -> ResultValue {
    // First we get it the actual string a private field stored on the object only the engine has access to.
    // Then we convert it into a Rust String by wrapping it in from_value
    let primitive_val: String = ctx.value_to_rust_string(this);

    // TODO: Should throw TypeError if search_string is regular expression
    let search_string: String = from_value(
        args.get(0)
            .expect("failed to get argument for String method")
            .clone(),
    )
    .expect("failed to parse argument for String method");

    let length: i32 = primitive_val.chars().count() as i32;

    // If less than 2 args specified, position is 'undefined', defaults to 0
    let position: i32 = if args.len() < 2 {
        0
    } else {
        from_value(args.get(1).expect("Could not get argument").clone())
            .expect("Could not convert value to i32")
    };

    let start = min(max(position, 0), length);

    // Take the string from "this" and use only the part of it after "start"
    let this_string: String = primitive_val.chars().skip(start as usize).collect();

    Ok(to_value(this_string.contains(&search_string)))
}

/// Return either the string itself or the string of the regex equivalent
fn get_regex_string(value: &Value) -> String {
    match value.deref() {
        ValueData::String(ref body) => body.into(),
        ValueData::Object(ref obj) => {
            let slots = &*obj.borrow().internal_slots;
            if slots.get("RegExpMatcher").is_some() {
                // first argument is another `RegExp` object, so copy its pattern and flags
                if let Some(body) = slots.get("OriginalSource") {
                    return from_value(r#body.clone())
                        .expect("unable to get body from regex value");
                }
            }
            "undefined".to_string()
        }
        _ => "undefined".to_string(),
    }
}

/// <https://tc39.es/ecma262/#sec-string.prototype.replace>
pub fn replace(this: &Value, args: &[Value], ctx: &mut Interpreter) -> ResultValue {
    // TODO: Support Symbol replacer
    let primitive_val: String = ctx.value_to_rust_string(this);
    if args.is_empty() {
        return Ok(to_value(primitive_val));
    }

    let regex_body = get_regex_string(args.get(0).expect("Value needed"));
    let re = Regex::new(&regex_body).expect("unable to convert regex to regex object");
    let mat = re.find(&primitive_val).expect("unable to find value");
    let caps = re
        .captures(&primitive_val)
        .expect("unable to get capture groups from text");

    let replace_value = if args.len() > 1 {
        // replace_object could be a string or function or not exist at all
        let replace_object: &Value = args.get(1).expect("second argument expected");
        match replace_object.deref() {
            ValueData::String(val) => {
                // https://tc39.es/ecma262/#table-45
                let mut result: String = val.to_string();
                let re = Regex::new(r"\$(\d)").unwrap();

                if val.find("$$").is_some() {
                    result = val.replace("$$", "$")
                }

                if val.find("$`").is_some() {
                    let start_of_match = mat.start();
                    let slice = &primitive_val[..start_of_match];
                    result = val.replace("$`", slice);
                }

                if val.find("$'").is_some() {
                    let end_of_match = mat.end();
                    let slice = &primitive_val[end_of_match..];
                    result = val.replace("$'", slice);
                }

                if val.find("$&").is_some() {
                    // get matched value
                    let matched = caps.get(0).expect("cannot get matched value");
                    result = val.replace("$&", matched.as_str());
                }

                // Capture $1, $2, $3 etc
                if re.is_match(&result) {
                    let mat_caps = re.captures(&result).unwrap();
                    let group_str = mat_caps.get(1).unwrap().as_str();
                    let group_int = group_str.parse::<usize>().unwrap();
                    result = re
                        .replace(result.as_str(), caps.get(group_int).unwrap().as_str())
                        .to_string()
                }

                result
            }
            ValueData::Function(_) => {
                // This will return the matched substring first, then captured parenthesized groups later
                let mut results: Vec<Value> = caps
                    .iter()
                    .map(|capture| to_value(capture.unwrap().as_str()))
                    .collect();

                // Returns the starting byte offset of the match
                let start = caps
                    .get(0)
                    .expect("Unable to get Byte offset from string for match")
                    .start();
                results.push(to_value(start));
                // Push the whole string being examined
                results.push(to_value(primitive_val.to_string()));

                let result = ctx.call(&replace_object, &this, results).unwrap();

                ctx.value_to_rust_string(&result)
            }
            _ => "undefined".to_string(),
        }
    } else {
        "undefined".to_string()
    };

    Ok(to_value(primitive_val.replacen(
        &mat.as_str(),
        &replace_value,
        1,
    )))
}

/// If searchString appears as a substring of the result of converting this
/// object to a String, at one or more indices that are greater than or equal to
/// position, then the smallest such index is returned; otherwise, -1 is
/// returned. If position is undefined, 0 is assumed, so as to search all of the
/// String.
/// <https://tc39.es/ecma262/#sec-string.prototype.includes>
pub fn index_of(this: &Value, args: &[Value], ctx: &mut Interpreter) -> ResultValue {
    // First we get it the actual string a private field stored on the object only the engine has access to.
    // Then we convert it into a Rust String by wrapping it in from_value
    let primitive_val: String = ctx.value_to_rust_string(this);

    // TODO: Should throw TypeError if search_string is regular expression
    let search_string: String = from_value(
        args.get(0)
            .expect("failed to get argument for String method")
            .clone(),
    )
    .expect("failed to parse argument for String method");

    let length: i32 = primitive_val.chars().count() as i32;

    // If less than 2 args specified, position is 'undefined', defaults to 0
    let position: i32 = if args.len() < 2 {
        0
    } else {
        from_value(args.get(1).expect("Could not get argument").clone())
            .expect("Could not convert value to i32")
    };

    let start = min(max(position, 0), length);

    // Here cannot use the &str method "find", because this returns the byte
    // index: we need to return the char index in the JS String
    // Instead, iterate over the part we're checking until the slice we're
    // checking "starts with" the search string
    for index in start..length {
        let this_string: String = primitive_val.chars().skip(index as usize).collect();
        if this_string.starts_with(&search_string) {
            // Explicitly return early with the index value
            return Ok(to_value(index));
        }
    }
    // Didn't find a match, so return -1
    Ok(to_value(-1))
}

//// If searchString appears as a substring of the result of converting this
/// object to a String at one or more indices that are smaller than or equal to
/// position, then the greatest such index is returned; otherwise, -1 is
/// returned. If position is undefined, the length of the String value is
/// assumed, so as to search all of the String.
/// <https://tc39.es/ecma262/#sec-string.prototype.lastindexof>
pub fn last_index_of(this: &Value, args: &[Value], ctx: &mut Interpreter) -> ResultValue {
    // First we get it the actual string a private field stored on the object only the engine has access to.
    // Then we convert it into a Rust String by wrapping it in from_value
    let primitive_val: String = ctx.value_to_rust_string(this);

    // TODO: Should throw TypeError if search_string is regular expression
    let search_string: String = from_value(
        args.get(0)
            .expect("failed to get argument for String method")
            .clone(),
    )
    .expect("failed to parse argument for String method");

    let length: i32 = primitive_val.chars().count() as i32;

    // If less than 2 args specified, position is 'undefined', defaults to 0
    let position: i32 = if args.len() < 2 {
        0
    } else {
        from_value(args.get(1).expect("Could not get argument").clone())
            .expect("Could not convert value to i32")
    };

    let start = min(max(position, 0), length);

    // Here cannot use the &str method "rfind", because this returns the last
    // byte index: we need to return the last char index in the JS String
    // Instead, iterate over the part we're checking keeping track of the higher
    // index we found that "starts with" the search string
    let mut highest_index: i32 = -1;
    for index in start..length {
        let this_string: String = primitive_val.chars().skip(index as usize).collect();
        if this_string.starts_with(&search_string) {
            highest_index = index;
        }
    }

    // This will still be -1 if no matches were found, else with be >= 0
    Ok(to_value(highest_index))
}

/// Returns an array whose contents is all the results matching the regular expression, if the global (g) flag is present,
/// in its absence, only the first complete match and its related capturing groups is returned,
/// otherwise null is returned if no match is found.
/// <https://tc39.es/ecma262/#sec-string.prototype.match>
pub fn r#match(this: &Value, args: &[Value], ctx: &mut Interpreter) -> ResultValue {
    let re = make_regexp(&to_value(Object::default()), &[args[0].clone()], ctx)?;
    regexp_match(&re, ctx.value_to_rust_string(this), ctx)
}

/// Abstract method `StringPad`
/// Performs the actual string padding for padStart/End.
/// <https://tc39.es/ecma262/#sec-stringpad/>
fn string_pad(
    primitive: String,
    max_length: i32,
    fill_string: Option<String>,
    at_start: bool,
) -> ResultValue {
    let primitive_length = primitive.len() as i32;

    if max_length <= primitive_length {
        return Ok(to_value(primitive));
    }

    let filler = match fill_string {
        Some(filler) => filler,
        None => String::from(" "),
    };

    if filler == "" {
        return Ok(to_value(primitive));
    }

    let fill_len = max_length.wrapping_sub(primitive_length);
    let mut fill_str = String::new();

    while fill_str.len() < fill_len as usize {
        fill_str.push_str(&filler);
    }
    // Cut to size max_length
    let concat_fill_str: String = fill_str.chars().take(fill_len as usize).collect();

    if at_start {
        Ok(to_value(format!("{}{}", concat_fill_str, &primitive)))
    } else {
        Ok(to_value(format!("{}{}", primitive, &concat_fill_str)))
    }
}

/// String.prototype.padEnd ( maxLength [ , fillString ] )
///
/// Pads the string with the given filler at the end of the string.
/// Filler defaults to single space.
/// <https://tc39.es/ecma262/#sec-string.prototype.padend/>
pub fn pad_end(this: &Value, args: &[Value], ctx: &mut Interpreter) -> ResultValue {
    let primitive_val: String = ctx.value_to_rust_string(this);
    if args.is_empty() {
        return Err(to_value("padEnd requires maxLength argument"));
    }
    let max_length = from_value(
        args.get(0)
            .expect("failed to get argument for String method")
            .clone(),
    )
    .expect("failed to parse argument for String method");
    let fill_string: Option<String> = match args.len() {
        1 => None,
        _ => Some(
            from_value(args.get(1).expect("Could not get argument").clone())
                .expect("Could not convert value to Option<String>"),
        ),
    };

    string_pad(primitive_val, max_length, fill_string, false)
}

/// String.prototype.padStart ( maxLength [ , fillString ] )
///
/// Pads the string with the given filler at the start of the string.
/// Filler defaults to single space.
/// <https://tc39.es/ecma262/#sec-string.prototype.padstart/>
pub fn pad_start(this: &Value, args: &[Value], ctx: &mut Interpreter) -> ResultValue {
    let primitive_val: String = ctx.value_to_rust_string(this);
    if args.is_empty() {
        return Err(to_value("padStart requires maxLength argument"));
    }
    let max_length = from_value(
        args.get(0)
            .expect("failed to get argument for String method")
            .clone(),
    )
    .expect("failed to parse argument for String method");
    let fill_string: Option<String> = match args.len() {
        1 => None,
        _ => Some(
            from_value(args.get(1).expect("Could not get argument").clone())
                .expect("Could not convert value to Option<String>"),
        ),
    };

    string_pad(primitive_val, max_length, fill_string, true)
}

fn is_trimmable_whitespace(c: char) -> bool {
    // The rust implementation of `trim` does not regard the same characters whitespace as ecma standard does
    //
    // Rust uses \p{White_Space} by default, which also includes:
    // `\u{0085}' (next line)
    // And does not include:
    // '\u{FEFF}' (zero width non-breaking space)
    match c {
        // Explicit whitespace: https://tc39.es/ecma262/#sec-white-space
        '\u{0009}' | '\u{000B}' | '\u{000C}' | '\u{0020}' | '\u{00A0}' | '\u{FEFF}' |
        // Unicode Space_Seperator category
        '\u{1680}' | '\u{2000}'..='\u{200A}' | '\u{202F}' | '\u{205F}' | '\u{3000}' |
        // Line terminators: https://tc39.es/ecma262/#sec-line-terminators
        '\u{000A}' | '\u{000D}' | '\u{2028}' | '\u{2029}' => true,
        _ => false,
    }
}

pub fn trim(this: &Value, _: &[Value], ctx: &mut Interpreter) -> ResultValue {
    let this_str: String = ctx.value_to_rust_string(this);
    Ok(to_value(this_str.trim_matches(is_trimmable_whitespace)))
}

pub fn trim_start(this: &Value, _: &[Value], ctx: &mut Interpreter) -> ResultValue {
    let this_str: String = ctx.value_to_rust_string(this);
    Ok(to_value(
        this_str.trim_start_matches(is_trimmable_whitespace),
    ))
}

pub fn trim_end(this: &Value, _: &[Value], ctx: &mut Interpreter) -> ResultValue {
    let this_str: String = ctx.value_to_rust_string(this);
    Ok(to_value(this_str.trim_end_matches(is_trimmable_whitespace)))
}

/// Return a String with every code point mapped to its corresponding lowercase equivalent.
/// With the current implementation the string is always copied even if the resulting String is identical
/// <https://tc39.es/ecma262/#sec-string.prototype.tolowercase>
pub fn to_lowercase(this: &Value, _: &[Value], ctx: &mut Interpreter) -> ResultValue {
    // First we get it the actual string a private field stored on the object only the engine has access to.
    // Then we convert it into a Rust String by wrapping it in from_value
    let this_str: String = ctx.value_to_rust_string(this);
    // The Rust String is mapped to uppercase using the builtin .to_lowercase().
    // There might be corner cases where it does not behave exactly like Javascript expects
    Ok(to_value(this_str.to_lowercase()))
}

/// Return a String with every code point mapped to its corresponding uppercase equivalent.
/// With the current implementation the string is always copied even if the resulting String is identical
/// <https://tc39.es/ecma262/#sec-string.prototype.touppercase>
pub fn to_uppercase(this: &Value, _: &[Value], ctx: &mut Interpreter) -> ResultValue {
    // First we get it the actual string a private field stored on the object only the engine has access to.
    // Then we convert it into a Rust String by wrapping it in from_value
    let this_str: String = ctx.value_to_rust_string(this);
    // The Rust String is mapped to uppercase using the builtin .to_uppercase().
    // There might be corner cases where it does not behave exactly like Javascript expects
    Ok(to_value(this_str.to_uppercase()))
}

/// Return a String which is a subset of the String value resulting from converting this object to a String.
/// The subset of the string is contained between the start index and the end index.
/// When both the start and end arguments are specified, the smaller one represent the index of the code unit
/// from which the returned String will start and the larger one the index of the code unit just after the end.
/// When only the start index is specified, the end index defaults to being the length of the string.
/// When no argument is specified, the returned String is the same as the original
/// <https://tc39.es/ecma262/#sec-string.prototype.substring>
pub fn substring(this: &Value, args: &[Value], ctx: &mut Interpreter) -> ResultValue {
    // First we get it the actual string a private field stored on the object only the engine has access to.
    // Then we convert it into a Rust String by wrapping it in from_value
    let primitive_val: String = ctx.value_to_rust_string(this);
    // If no args are specified, start is 'undefined', defaults to 0
    let start = if args.is_empty() {
        0
    } else {
        from_value(
            args.get(0)
                .expect("failed to get argument for String method")
                .clone(),
        )
        .expect("failed to parse argument for String method")
    };
    let length: i32 = primitive_val.chars().count() as i32;
    // If less than 2 args specified, end is the length of the this object converted to a String
    let end = if args.len() < 2 {
        length
    } else {
        from_value(args.get(1).expect("Could not get argument").clone())
            .expect("failed to parse argument for String method")
    };
    // Both start and end args replaced by 0 if they were negative
    // or by the length of the String if they were greater
    let final_start = min(max(start, 0), length);
    let final_end = min(max(end, 0), length);
    // Start and end are swapped if start is greater than end
    let from = min(final_start, final_end) as usize;
    let to = max(final_start, final_end) as usize;
    // Extract the part of the string contained between the start index and the end index
    // where start is guaranteed to be smaller or equals to end
    let extracted_string: String = primitive_val
        .chars()
        .skip(from)
        .take(to.wrapping_sub(from))
        .collect();
    Ok(to_value(extracted_string))
}

/// Return a String which is a subset of the String value resulting from converting this object to a String.
/// The subset of the string starts at the start index and is at most length code units long, depending if the string is shorter.
/// When only the start index is specified, the length become the length of the string.
/// When the start index is negative, the start index become the number of code units from the end of the string.
/// When no argument is specified, the returned String is the same as the original
/// <https://tc39.es/ecma262/#sec-string.prototype.substr>
pub fn substr(this: &Value, args: &[Value], ctx: &mut Interpreter) -> ResultValue {
    // First we get it the actual string a private field stored on the object only the engine has access to.
    // Then we convert it into a Rust String by wrapping it in from_value
    let primitive_val: String = ctx.value_to_rust_string(this);
    // If no args are specified, start is 'undefined', defaults to 0
    let mut start = if args.is_empty() {
        0
    } else {
        from_value(
            args.get(0)
                .expect("failed to get argument for String method")
                .clone(),
        )
        .expect("failed to parse argument for String method")
    };
    let length: i32 = primitive_val.chars().count() as i32;
    // If less than 2 args specified, end is +infinity, the maximum number value.
    // Using i32::max_value() should be safe because the final length used is at most
    // the number of code units from start to the end of the string,
    // which should always be smaller or equals to both +infinity and i32::max_value
    let end = if args.len() < 2 {
        i32::max_value()
    } else {
        from_value(args.get(1).expect("Could not get argument").clone())
            .expect("failed to parse argument for String method")
    };
    // If start is negative it become the number of code units from the end of the string
    if start < 0 {
        start = max(length.wrapping_add(start), 0);
    }
    // length replaced by 0 if it was negative
    // or by the number of code units from start to the end of the string if it was greater
    let result_length = min(max(end, 0), length.wrapping_sub(start));
    // If length is negative we return an empty string
    // otherwise we extract the part of the string from start and is length code units long
    if result_length <= 0 {
        Ok(to_value("".to_string()))
    } else {
        let extracted_string: String = primitive_val
            .chars()
            .skip(start as usize)
            .take(result_length as usize)
            .collect();
        Ok(to_value(extracted_string))
    }
}

/// Get the string value to a primitive string
/// <https://tc39.es/ecma262/#sec-string.prototype.valueof>
pub fn value_of(this: &Value, args: &[Value], ctx: &mut Interpreter) -> ResultValue {
    // Use the to_string method because it is specified to do the same thing in this case
    to_string(this, args, ctx)
}

/// TODO: update this method to return iterator
/// Returns an array* of all results matching a string against a regular expression, including capturing groups
/// <https://tc39.es/ecma262/#sec-string.prototype.matchall>
pub fn match_all(this: &Value, args: &[Value], ctx: &mut Interpreter) -> ResultValue {
    let re: Value = match args.get(0) {
        Some(arg) => {
            if arg == &Gc::new(ValueData::Null) {
                make_regexp(
                    &to_value(Object::default()),
                    &[
                        to_value(ctx.value_to_rust_string(arg)),
                        to_value(String::from("g")),
                    ],
                    ctx,
                )
            } else if arg == &Gc::new(ValueData::Undefined) {
                make_regexp(
                    &to_value(Object::default()),
                    &[Gc::new(ValueData::Undefined), to_value(String::from("g"))],
                    ctx,
                )
            } else {
                from_value(arg.clone()).map_err(to_value)
            }
        }
        None => make_regexp(
            &to_value(Object::default()),
            &[to_value(String::new()), to_value(String::from("g"))],
            ctx,
        ),
    }?;

    regexp_match_all(&re, ctx.value_to_rust_string(this))
}

/// Create a new `String` object
pub fn create_constructor(global: &Value) -> Value {
    // Create constructor function object
    let mut string_constructor = Object::default();
    string_constructor.kind = ObjectKind::Function;

    string_constructor.set_internal_method("construct", make_string);
    // Todo: add call internal method (should be easy)
    // Currently call points to the constructor function, this is wrong
    string_constructor.set_internal_method("call", call_string);

    // Create prototype
    let proto = ValueData::new_obj(Some(global));
    let prop = Property::default().value(to_value(0_i32));

    proto.set_prop_slice("length", prop);
    make_builtin_fn!(char_at, named "charAt", with length 1, of proto);
    make_builtin_fn!(char_code_at, named "charCodeAt", with length 1, of proto);
    make_builtin_fn!(to_string, named "toString", of proto);
    make_builtin_fn!(concat, named "concat", with length 1, of proto);
    make_builtin_fn!(repeat, named "repeat", with length 1, of proto);
    make_builtin_fn!(slice, named "slice", with length 2, of proto);
    make_builtin_fn!(starts_with, named "startsWith", with length 1, of proto);
    make_builtin_fn!(ends_with, named "endsWith", with length 1, of proto);
    make_builtin_fn!(includes, named "includes", with length 1, of proto);
    make_builtin_fn!(index_of, named "indexOf", with length 1, of proto);
    make_builtin_fn!(last_index_of, named "lastIndexOf", with length 1, of proto);
    make_builtin_fn!(r#match, named "match", with length 1, of proto);
    make_builtin_fn!(pad_end, named "padEnd", with length 1, of proto);
    make_builtin_fn!(pad_start, named "padStart", with length 1, of proto);
    make_builtin_fn!(trim, named "trim", of proto);
    make_builtin_fn!(trim_start, named "trimStart", of proto);
    make_builtin_fn!(to_lowercase, named "toLowerCase", of proto);
    make_builtin_fn!(to_uppercase, named "toUpperCase", of proto);
    make_builtin_fn!(substring, named "substring", with length 2, of proto);
    make_builtin_fn!(substr, named "substr", with length 2, of proto);
    make_builtin_fn!(value_of, named "valueOf", of proto);
    make_builtin_fn!(match_all, named "matchAll", with length 1, of proto);
    make_builtin_fn!(replace, named "replace", with length 2, of proto);

    let string = to_value(string_constructor);
    proto.set_field_slice("constructor", string.clone());
    string.set_field_slice(PROTOTYPE, proto);
    string
}

/// Initialise the `String` object on the global object
pub fn init(global: &Value) {
    global.set_field_slice("String", create_constructor(global));
}
