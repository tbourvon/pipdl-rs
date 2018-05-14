/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

//! Fairly minimal recursive-descent parser helper functions and types.

use std::path::Path;

#[derive(Copy, Clone, Debug)]
pub struct Location<'filepath> {
    pub line: usize,
    pub col: usize,
    pub file: &'filepath Path,
}

impl<'filepath> Location<'filepath> {
    fn new(file: &'filepath Path) -> Self {
        Location {
            line: 0,
            col: 0,
            file,
        }
    }

    pub fn is_null(&self) -> bool {
        self.line == 0
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Span<'filepath> {
    pub start: Location<'filepath>,
    pub end: Location<'filepath>,
}

impl<'filepath> Span<'filepath> {
    pub fn new(file: &'filepath Path) -> Self {
        Span {
            start: Location::new(file),
            end: Location::new(file),
        }
    }

    pub fn is_null(&self) -> bool {
        self.start.is_null() && self.end.is_null()
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Spanned<'filepath, T> {
    pub data: T,
    pub span: Span<'filepath>,
}

impl<'filepath, T> Spanned<'filepath, T> {
    pub fn new(span: Span<'filepath>, data: T) -> Self {
        Spanned {data, span}
    }
}

/// Every bit set but the high bit in usize.
const OFF_MASK: usize = <usize>::max_value() / 2;

/// Only the high bit in usize.
const FATAL_MASK: usize = ! OFF_MASK;

/// An error produced by pipdl
#[derive(Debug)]
pub struct ParserError<'filepath> {
    message: String,
    fatal: bool,
    span: Span<'filepath>,
}

impl<'filepath> ParserError<'filepath> {
    pub(crate) fn is_fatal(&self) -> bool {
        self.fatal
    }

    pub(crate) fn make_fatal(mut self) -> Self {
        self.fatal = true;
        self
    }

    pub(crate) fn span(&self) -> Span {
        self.span
    }

    pub(crate) fn message(&self) -> &str {
        &self.message
    }
}

/// Attempts to run each expression in order, recovering from any non-fatal
/// errors and attempting the next option.
macro_rules! any {
    ($i:ident, $expected:expr, $($e:expr => |$x:ident| $f:expr),+ $(,)*) => {
        // NOTE: We have to do this sorcery for early returns. Using a loop with breaks makes clippy
        // mad because the loop never loops and we can't desable the lint because we're not in a function.
        // Also, we don't directly call the closure otherwise clippy would also complain about that. Yeah.
        {
            let mut my_closure = || {
                $(match $e {
                    Ok((i, $x)) => return Ok((i, $f)),
                    Err(e) => {
                        // This assignment is used to help out with type inference.
                        let e: $crate::util::ParserError = e;
                        if e.is_fatal() {
                            return Err(e);
                        }
                    }
                })+
                return $i.expected($expected);
            };

            my_closure()
        }
    };

    ($i:ident, $expected:expr, $($e:expr => $f:expr),+ $(,)*) => {
        any!($i, $expected, $($e => |_x| $f),+);
    }
}

/// Attempts to repeatedly run the expression, stopping on a non-fatal error,
/// and directly returning any fatal error.
macro_rules! drive {
    ($i:ident, $e:expr) => {
        let mut $i = $i;
        loop {
            match $e {
                Ok((j, _)) => $i = j,
                Err(e) => if e.is_fatal() {
                    return Err(e);
                } else {
                    break;
                }
            }
        }
    }
}

/// The type of error used by internal parsers
pub(crate) type PResult<'src, 'filepath, T> = Result<(In<'src, 'filepath>, T), ParserError<'filepath>>;

/// Specify that after this point, errors produced while parsing which are not
/// handled should instead be treated as fatal parsing errors.
macro_rules! commit {
    ($($e:tt)*) => {
        // Evaluate the inner expression, transforming errors into fatal errors.
        ({ $($e)* }).map_err($crate::util::ParserError::make_fatal)
    }
}

/// This datastructure is used as the cursor type into the input source data. It
/// holds the full source string, and the current offset.
#[derive(Copy, Clone, Debug)]
pub(crate) struct In<'src, 'filepath> {
    src: &'src str,
    byte_offset: usize,
    loc: Location<'filepath>,
}
impl<'src, 'filepath> In<'src, 'filepath> {
    pub(crate) fn new(s: &'src str, file: &'filepath Path) -> Self  {
        In { src: s, byte_offset: 0, loc: Location { line: 1, col: 0, file } }
    }

    /// The remaining string in the source file.
    pub(crate) fn rest(&self) -> &'src str {
        &self.src[self.byte_offset..]
    }

    /// Move the cursor forward by `n` characters.
    pub(crate) fn advance(&self, n_chars: usize) -> Self {
        let mut loc = self.loc;

        let (n_bytes, last_c) = self.rest().char_indices().take(n_chars).inspect(|&(_, character)| {
            if character == '\n' {
                loc.line += 1;
                loc.col = 0;
            } else {
                loc.col += 1;
            }
        }).last().expect("No characters remaining in advance");

        let byte_offset = self.byte_offset.checked_add(n_bytes + last_c.len_utf8()).expect("Failed checked add");

        assert!(byte_offset <= self.src.len());

        In { src: self.src, byte_offset, loc }
    }

    /// Produce a new non-fatal error result with the given expected value.
    pub(crate) fn expected<T>(&self, expected: &'static str) -> Result<T, ParserError<'filepath>> {
        assert!((self.byte_offset & FATAL_MASK) == 0, "Offset is too large!");
        Err(ParserError {
            message: format!("Expected {}", expected),
            fatal: false,
            span: Span { start: self.loc, end: self.loc },
        })
    }

    pub(crate) fn loc(&self) -> Location<'filepath> {
        self.loc
    }
}

/// Repeatedly run f, collecting results into a vec. Returns an error if a fatal
/// error is produced while parsing.
pub(crate) fn many<'src, 'filepath, F, R>(i: In<'src, 'filepath>, mut f: F) -> PResult<'src, 'filepath, Vec<R>>
where
    F: FnMut(In<'src, 'filepath>) -> PResult<'src, 'filepath, R>,
{
    let mut v = Vec::new();
    drive!(i, match f(i) {
        Ok((i, x)) => {
            v.push(x);
            Ok((i, ()))
        }
        Err(e) => Err(e),
    });
    Ok((i, v))
}

/// Repeatedly run f, followed by parsing the seperator sep. Returns an error if
/// a fatal error is produced while parsing.
pub(crate) fn sep<'src, 'filepath, Parser, Ret>(
    i: In<'src, 'filepath>,
    mut parser: Parser,
    sep: &'static str,
) -> PResult<'src, 'filepath, Vec<Ret>>
where
    Parser: FnMut(In<'src, 'filepath>) -> PResult<'src, 'filepath, Ret>,
{
    let mut return_vector = Vec::new();
    drive!(i, match parser(i) {
        Ok((i, result)) => {
            return_vector.push(result);
            match punct(i, sep) {
                Ok(o) => Ok(o),
                Err(_) => return Ok((i, return_vector)),
            }
        }
        Err(e) => Err(e),
    });
    Ok((i, return_vector))
}

/// Skip any leading whitespace, including comments
pub(crate) fn skip_ws<'src, 'filepath>(mut i: In<'src, 'filepath>) -> Result<In<'src, 'filepath>, ParserError<'filepath>> {
    loop {
        if i.rest().is_empty() {
            break;
        }

        let c = i.rest().chars().next().expect("No characters remaining when skipping ws");
        if c.is_whitespace() {
            i = i.advance(1);
            continue;
        }

        // Line comments
        if i.rest().starts_with("//") {
            while !i.rest().starts_with('\n') {
                i = i.advance(1);
                if i.rest().is_empty() {
                    break;
                }
            }
            continue;
        }

        // Block comments
        if i.rest().starts_with("/*") {
            while !i.rest().starts_with("*/") {
                i = i.advance(1);
                if i.rest().is_empty() {
                    return i.expected("end of block comment (`*/`)");
                }
            }

            i = i.advance(2);
            continue;
        }
        break;
    }

    Ok(i)
}

/// Read an identifier as a string.
pub(crate) fn ident<'src, 'filepath>(i: In<'src, 'filepath>) -> PResult<'src, 'filepath, Spanned<'filepath, String>> {
    let i = skip_ws(i)?;
    let start = i.loc();
    let (end_char, end_byte) = i.rest()
        .char_indices()
        .enumerate()
        .skip_while(|&(_, (b_idx, c))| match c {
            '_' | 'a'...'z' | 'A'...'Z' => true,
            '0'...'9' if b_idx != 0 => true,
            _ => false,
        })
        .next()
        .map(|(c_idx, (b_idx, _))| (c_idx, b_idx))
        .unwrap_or((i.rest().chars().count(), i.rest().len()));

    if end_byte == 0 {
        return i.expected("identifier");
    }

    let j = i.advance(end_char);
    let end = j.loc();

    Ok((j, Spanned::new(Span {start, end}, i.rest()[..end_byte].to_owned())))
}

/// Parse a specific keyword.
pub(crate) fn kw<'src, 'filepath>(i: In<'src, 'filepath>, kw: &'static str) -> PResult<'src, 'filepath, Spanned<'filepath, ()>> {
    let (j, id) = ident(i)?;
    if id.data == kw {
        Ok((j, Spanned::new(id.span, ())))
    } else {
        i.expected(kw)
    }
}

/// Parse punctuation.
pub(crate) fn punct<'src, 'filepath>(i: In<'src, 'filepath>, p: &'static str) -> PResult<'src, 'filepath, Spanned<'filepath, ()>> {
    let i = skip_ws(i)?;
    let start = i.loc();
    if i.rest().starts_with(p) {
        let i = i.advance(p.chars().count());
        let end = i.loc();
        Ok((i, Spanned::new(Span { start, end }, ())))
    } else {
        i.expected(p)
    }
}

/// Try to parse the inner value, and return Some() if it succeeded, None if it
/// failed non-fatally, and an error if it failed fatally.
pub(crate) fn maybe<'src, 'filepath, T>(
    i: In<'src, 'filepath>,
    r: PResult<'src, 'filepath, T>
) -> PResult<'src, 'filepath, Option<T>> {
    match r {
        Ok((i, x)) => Ok((i, Some(x))),
        Err(e) => if e.is_fatal() {
            Err(e)
        } else {
            Ok((i, None))
        }
    }
}

/// Parse a string literal.
pub(crate) fn string<'src, 'filepath>(i: In<'src, 'filepath>) -> PResult<'src, 'filepath, Spanned<'filepath, String>> {
    let mut s = String::new();
    let start = i.loc();
    let (i, _) = punct(i, "\"")?;
    let mut chars = i.rest().chars().enumerate().peekable();
    while let Some((char_offset, ch)) = chars.next() {
        match ch {
            '"' => {
                let i = i.advance(char_offset + 1);
                let end = i.loc();
                return Ok((i, Spanned::new(Span {start, end}, s)))
            },
            '\\' => match chars.next() {
                Some((_, 'n')) => s.push('\n'),
                Some((_, 'r')) => s.push('\r'),
                Some((_, 't')) => s.push('\t'),
                Some((_, '\\')) => s.push('\\'),
                Some((_, '\'')) => s.push('\''),
                Some((_, '"')) => s.push('"'),
                Some((_, '0')) => s.push('\0'),
                _ => return i.advance(char_offset)
                    .expected("valid escape (\\n, \\r, \\t, \\\\, \\', \\\", or \\0)"),
            }
            x => s.push(x),
        }
    }
    i.expected("end of string literal (\")")
}
