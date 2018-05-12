/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

//! This is a basic recursive-descent parser for ipdl. It intentionally doesn't
//! have some featres which most languages would have, such as unicode support
//! in identifiers, as it is unnecessary for our usecase.


// Our code is Clippy aware, so we disable warnings for unknown lints
// which are triggered when we silence a clippy lint
#![allow(unknown_lints)]

use std::fmt;
use std::error;
use std::path::Path;

#[macro_use]
pub mod util;
use util::*;

struct ErrorInner {
    line: usize,
    column: usize,
    message: String,
}

/// Public error type for messages with resolved type information.
pub struct Error(Box<ErrorInner>);

impl Error {
    /// Get the line and column of the parsing error.
    pub fn line_column(&self) -> (usize, usize) {
        (self.0.line, self.0.column)
    }
}

impl error::Error for Error {
    fn description(&self) -> &str { &self.0.message }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.message.fmt(f)
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Error")
            .field("line", &self.0.line)
            .field("column", &self.0.column)
            .finish()
    }
}

#[derive(Debug)]
pub struct CxxInclude<'filepath> {
    pub file: Spanned<'filepath, String>,
}

fn cxx_include(i: In) -> PResult<Spanned<CxxInclude>> {
    let start = i.loc();
    let (i, _) = kw(i, "include")?;
    let (i, file) = string(i)?;
    commit! {
        let (i, _) = punct(i, ";")?;
        Ok((i, Spanned::new(Span { start, end: i.loc() }, CxxInclude { file })))
    }
}

#[derive(Debug)]
pub struct Include<'filepath> {
    pub protocol: Option<Spanned<'filepath, ()>>,
    pub id: Spanned<'filepath, String>,
}

fn include(i: In) -> PResult<Spanned<Include>> {
    let start = i.loc();
    let (i, _) = kw(i, "include")?;
    let (i, pcol) = maybe(i, kw(i, "protocol"))?;

    let common_end_code = |i, id| {
        let (i, _) = punct(i, ";")?;
        let end = i.loc();
        Ok((i, Spanned::new(Span { start, end }, Include {
            protocol: pcol,
            id,
        })))
    };

    if pcol.is_some() { // If we have the protocol keyword, then it can't be a C++ include anymore, and we start committing
        commit! {
            let (i, id) = ident(i)?;
            common_end_code(i, id)
        }
    } else { // Otherwise we first parse the ident
        let (i, id) = ident(i)?;
        commit! {
            common_end_code(i, id)
        }
    }
}

#[derive(Debug)]
pub enum CxxTypeKind {
    Class,
    Struct,
    None,
}

#[derive(Debug)]
pub struct CxxPathSeg<'filepath> {
    pub id: Spanned<'filepath, String>,
    pub args: Option<Spanned<'filepath, Vec<Spanned<'filepath, String>>>>,
}

fn template_args(i: In) -> PResult<Spanned<Vec<Spanned<String>>>> {
    let start = i.loc();
    let (i, _) = punct(i, "<")?;
    commit! {
        let (i, args) = sep(i, ident, ",")?;
        let (i, _) = punct(i, ">")?;

        let end = i.loc();
        Ok((i, Spanned::new(Span { start, end }, args)))
    }
}

fn cxx_path_seg(i: In) -> PResult<Spanned<CxxPathSeg>> {
    let start = i.loc();
    let (i, id) = ident(i)?;
    let (i, args) = maybe(i, template_args(i))?;
    let end = i.loc();
    Ok((i, Spanned::new(Span { start, end }, CxxPathSeg {
        id,
        args,
    })))
}

#[derive(Debug)]
pub struct CxxPath<'filepath> {
    pub segs: Vec<Spanned<'filepath, CxxPathSeg<'filepath>>>,
}

fn cxx_path(i: In) -> PResult<Spanned<CxxPath>> {
    let start = i.loc();
    let (i, segs) = sep(i, cxx_path_seg, "::")?;
    let end = i.loc();
    Ok((i, Spanned::new(Span { start, end }, CxxPath { segs })))
}

#[derive(Debug)]
pub struct Using<'filepath> {
    pub refcounted: Option<Spanned<'filepath, ()>>,
    pub kind: Spanned<'filepath, CxxTypeKind>,
    pub ty: Spanned<'filepath, CxxPath<'filepath>>,
    pub file: Spanned<'filepath, String>,
}

fn using(i: In) -> PResult<Spanned<Using>> {
    let start = i.loc();
    let (i, _) = kw(i, "using")?;
    commit! {
        let (i, refcounted) = maybe(i, kw(i, "refcounted"))?;
        let kw_start = i.loc();
        let (i, kind) = any!(
            i, "struct or class keyword",
            kw(i, "struct") => CxxTypeKind::Struct,
            kw(i, "class") => CxxTypeKind::Class,
            Ok((i, ())) => CxxTypeKind::None,
        )?;
        let kw_end = i.loc();

        let (i, ty) = cxx_path(i)?;
        let (i, _) = kw(i, "from")?;
        let (i, file) = string(i)?;
        let (i, _) = punct(i, ";")?;

        let end = i.loc();

        Ok((i, Spanned::new(Span { start, end }, Using { refcounted, kind: Spanned::new(Span { start: kw_start, end: kw_end }, kind), ty, file })))
    }
}

#[derive(Debug)]
pub struct Type<'filepath> {
    pub is_nullable: Option<Spanned<'filepath, ()>>,
    pub name: Spanned<'filepath, CxxPathSeg<'filepath>>,
    pub is_array: Option<Spanned<'filepath, ()>>,
}

fn ty(i: In) -> PResult<Spanned<Type>> {
    let start = i.loc();
    let (i, nullable) = maybe(i, kw(i, "nullable"))?;
    let (i, name) = cxx_path_seg(i)?;
    let (i, array) = maybe(i, {
        let (i, _) = punct(i, "[")?;
        commit! { punct(i, "]") }
    })?;

    let end = i.loc();

    Ok((i, Spanned::new(
        Span { start, end },
        Type {
            is_nullable: nullable,
            name,
            is_array: array,
        }
    )))
}

fn component(i: In) -> PResult<Spanned<Type>> {
    let (i, ty) = ty(i)?;
    commit! {
        let (i, _) = punct(i, ";")?;
        Ok((i, ty))
    }
}

#[derive(Debug)]
pub struct UnionItem<'filepath> {
    pub path: Vec<Spanned<'filepath, String>>,
    pub components: Vec<Spanned<'filepath, Type<'filepath>>>,
}

fn union_item(i: In) -> PResult<Spanned<UnionItem>> {
    let start = i.loc();
    let (i, _) = kw(i, "union")?;
    commit! {
        let (i, name) = ident(i)?;
        let (i, _) = punct(i, "{")?;
        let (i, components) = many(i, component)?;
        let (i, _) = punct(i, "}")?;
        let (i, _) = punct(i, ";")?;

        let end = i.loc();

        Ok((i, Spanned::new(Span { start, end }, UnionItem {
            path: vec![name],
            components,
        })))
    }
}

#[derive(Debug)]
pub struct Field<'filepath> {
    pub ty: Spanned<'filepath, Type<'filepath>>,
    pub name: Spanned<'filepath, String>,
}

fn field(i: In) -> PResult<Spanned<Field>> {
    let start = i.loc();
    let (i, ty) = ty(i)?;
    commit! {
        let (i, name) = ident(i)?;
        let (i, _) = punct(i, ";")?;
        let end = i.loc();
        Ok((i, Spanned::new(Span {start, end}, Field { ty, name })))
    }
}

#[derive(Debug)]
pub struct StructItem<'filepath> {
    pub path: Vec<Spanned<'filepath, String>>,
    pub fields: Vec<Spanned<'filepath, Field<'filepath>>>,
}

fn struct_item(i: In) -> PResult<Spanned<StructItem>> {
    let start = i.loc();
    let (i, _) = kw(i, "struct")?;
    commit! {
        let (i, name) = ident(i)?;
        let (i, _) = punct(i, "{")?;
        let (i, fields) = many(i, field)?;
        let (i, _) = punct(i, "}")?;
        let (i, _) = punct(i, ";")?;

        let end = i.loc();

        Ok((i, Spanned::new(Span { start, end }, StructItem {
            path: vec![name],
            fields,
        })))
    }
}

#[derive(Debug)]
pub enum Nesting {
    None,
    InsideSync,
    InsideCpow,
}

fn nesting(i: In) -> PResult<Spanned<Nesting>> {
    let start = i.loc();
    let (i, nesting) = any!(
        i, "nesting specifier (not, inside_sync, or inside_cpow)",
        kw(i, "not") => Nesting::None,
        kw(i, "inside_sync") => Nesting::InsideSync,
        kw(i, "inside_cpow") => Nesting::InsideCpow,
    )?;
    let end = i.loc();
    Ok((i, Spanned::new(Span {start, end}, nesting)))
}

#[derive(Debug)]
pub enum Priority {
    Normal,
    High,
    Input,
}

fn priority(i: In) -> PResult<Spanned<Priority>> {
    let start = i.loc();
    let (i, priority) = any!(
        i, "priority specifier (normal, high, or input)",
        kw(i, "normal") => Priority::Normal,
        kw(i, "high") => Priority::High,
        kw(i, "input") => Priority::Input,
    )?;
    let end = i.loc();
    Ok((i, Spanned::new(Span {start, end}, priority)))
}

#[derive(Debug)]
pub enum SendSemantics {
    Async,
    Sync,
    Intr,
}

#[derive(Debug)]
pub enum MessageModifier {
    Verify,
    Compress,
    CompressAll,
}

fn message_modifier(i: In) -> PResult<Spanned<MessageModifier>> {
    let start = i.loc();
    let (i, message_modifier) = any!(
        i, "message modifier (verify, compress, or compressall)",
        kw(i, "verify") => MessageModifier::Verify,
        kw(i, "compress") => MessageModifier::Compress,
        kw(i, "compressall") => MessageModifier::CompressAll,
    )?;
    let end = i.loc();
    Ok((i, Spanned::new(Span {start, end}, message_modifier)))
}

#[derive(Debug)]
pub struct Param<'filepath> {
    pub ty: Spanned<'filepath, Type<'filepath>>,
    pub name: Spanned<'filepath, String>,
}

fn param(i: In) -> PResult<Spanned<Param>> {
    let start = i.loc();
    let (i, ty) = ty(i)?;
    commit! {
        let (i, name) = ident(i)?;
        let end = i.loc();
        Ok((i, Spanned::new(Span { start, end }, Param { ty, name })))
    }
}

#[derive(Debug)]
pub struct MessageDecl<'filepath> {
    pub nested: Option<Spanned<'filepath, Spanned<'filepath, Nesting>>>,
    pub priority: Option<Spanned<'filepath, Spanned<'filepath, Priority>>>,
    pub send_semantics: Spanned<'filepath, SendSemantics>,
    pub name: Spanned<'filepath, String>,
    pub params: Vec<Spanned<'filepath, Param<'filepath>>>,
    pub returns: Option<Spanned<'filepath, Vec<Spanned<'filepath, Param<'filepath>>>>>,
    pub modifiers: Vec<Spanned<'filepath, MessageModifier>>,
}

fn returns(i: In) -> PResult<Spanned<Vec<Spanned<Param>>>> {
    let start = i.loc();
    let (i, _) = kw(i, "returns")?;
    commit! {
        let (i, _) = punct(i, "(")?;
        let (i, p) = sep(i, param, ",")?;
        let (i, _) = punct(i, ")")?;
        let end = i.loc();
        Ok((i, Spanned::new(Span { start, end }, p)))
    }
}

fn message_nested(i: In) -> PResult<Spanned<Spanned<Nesting>>> {
    let start = i.loc();
    let (i, _) = kw(i, "nested")?;
    commit! {
        let (i, _) = punct(i, "(")?;
        let (i, nested) = nesting(i)?;
        let (i, _) = punct(i, ")")?;

        let end = i.loc();

        Ok((i, Spanned::new(Span { start, end }, nested)))
    }
}

fn message_prio(i: In) -> PResult<Spanned<Spanned<Priority>>> {
    let start = i.loc();
    let (i, _) = kw(i, "prio")?;
    commit! {
        let (i, _) = punct(i, "(")?;
        let (i, prio) = priority(i)?;
        let (i, _) = punct(i, ")")?;

        let end = i.loc();

        Ok((i, Spanned::new(Span { start, end }, prio)))
    }
}

fn message_decl(i: In) -> PResult<Spanned<MessageDecl>> {
    let start = i.loc();

    // XXX(nika): This is really gross, maybe clean it up?
    let mut nested = None;
    let mut priority = None;
    drive!(i, any!(
        i, "message prefix",
        message_prio(i) => |p| priority = Some(p),
        message_nested(i) => |n| nested = Some(n),
    ));

    let send_semantics_start = i.loc();
    let (i, send_semantics) = any!(
        i, "send semantics (async, sync, or intr)",
        kw(i, "async") => SendSemantics::Async,
        kw(i, "sync") => SendSemantics::Sync,
        kw(i, "intr") => SendSemantics::Intr,
    )?;
    let send_semantics_end = i.loc();

    commit! {
        let (i, name) = ident(i)?;
        let (i, _) = punct(i, "(")?;
        let (i, params) = sep(i, param, ",")?;
        let (i, _) = punct(i, ")")?;
        let (i, returns) = maybe(i, returns(i))?;
        let (i, modifiers) = many(i, message_modifier)?;
        let (i, _) = punct(i, ";")?;

        let end = i.loc();

        Ok((i, Spanned::new(Span { start, end }, MessageDecl {
            nested,
            priority,
            send_semantics: Spanned::new(Span { start: send_semantics_start, end: send_semantics_end }, send_semantics),
            name,
            params,
            returns,
            modifiers,
        })))
    }
}

#[derive(Debug)]
pub enum Direction {
    ToChild,
    ToParent,
    Both,
}

fn direction(i: In) -> PResult<Spanned<Direction>> {
    let start = i.loc();
    let (i, direction) = any!(
        i, "direction (child, parent, or both)",
        kw(i, "child") => Direction::ToChild,
        kw(i, "parent") => Direction::ToParent,
        kw(i, "both") => Direction::Both,
    )?;
    let end = i.loc();
    Ok((i, Spanned::new(Span { start, end }, direction)))
}

#[derive(Debug)]
pub struct MessageGroup<'filepath> {
    pub direction: Spanned<'filepath, Direction>,
    pub decls: Vec<Spanned<'filepath, MessageDecl<'filepath>>>,
}

fn message_group(i: In) -> PResult<Spanned<MessageGroup>> {
    let start = i.loc();
    let (i, direction) = direction(i)?;
    commit! {
        let (i, _) = punct(i, ":")?;
        let (i, decls) = many(i, message_decl)?;

        let end = i.loc();

        Ok((i, Spanned::new(Span { start, end }, MessageGroup {
            direction,
            decls,
        })))
    }
}

#[derive(Debug)]
pub struct ProtocolItem<'filepath> {
    pub path: Vec<Spanned<'filepath, String>>,
    pub nested: Option<Spanned<'filepath, Nesting>>,
    pub send_semantics: Spanned<'filepath, SendSemantics>,
    pub managers: Option<Spanned<'filepath, Vec<Spanned<'filepath, String>>>>,
    pub manages: Vec<Spanned<'filepath, Spanned<'filepath, String>>>,
    pub groups: Vec<Spanned<'filepath, MessageGroup<'filepath>>>,
}

fn protocol_nested(i: In) -> PResult<(Spanned<Nesting>, Spanned<SendSemantics>)> {
    let (i, _) = kw(i, "nested")?;
    commit! {
        let (i, _) = punct(i, "(")?;
        let (i, _) = kw(i, "upto")?;
        let (i, n) = nesting(i)?;
        let (i, _) = punct(i, ")")?;
        let ss_start = i.loc();
        let (i, ss) = any!(
            i, "send semantics (async or sync)",
            kw(i, "async") => SendSemantics::Async,
            kw(i, "sync") => SendSemantics::Sync,
        )?;
        let ss_end = i.loc();
        Ok((i, (n, Spanned::new(Span { start: ss_start, end: ss_end}, ss))))
    }
}

fn managers(i: In) -> PResult<Spanned<Vec<Spanned<String>>>> {
    let start = i.loc();
    let (i, _) = kw(i, "manager")?;
    commit! {
        let (i, managers) = sep(i, ident, "or")?;
        let (i, _) = punct(i, ";")?;
        let end = i.loc();
        Ok((i, Spanned::new(Span {start, end}, managers)))
    }
}

fn manages(i: In) -> PResult<Spanned<Spanned<String>>> {
    let start = i.loc();
    let (i, _) = kw(i, "manages")?;
    commit! {
        let (i, name) = ident(i)?;
        let (i, _) = punct(i, ";")?;
        let end = i.loc();
        Ok((i, Spanned::new(Span { start, end }, name)))
    }
}

fn protocol_item(i: In) -> PResult<Spanned<ProtocolItem>> {
    let start = i.loc();
    let mut ss_span = Span::new(i.loc().file);

    let ss_start = i.loc();
    let (i, (nested, send_semantics)) = any!(
        i, "protocol item prefixes",
        kw(i, "async") => |_x| (None, SendSemantics::Async),
        kw(i, "sync") => |_x| (None, SendSemantics::Sync),
        kw(i, "intr") => |_x| (None, SendSemantics::Intr),
        protocol_nested(i) => |x| {ss_span = x.1.span; (Some(x.0), x.1.data)},
        Ok((i, ())) => |_x| (None, SendSemantics::Async),
    )?;
    let ss_end = i.loc();

    if ss_span.is_null() {
        ss_span = Span {start: ss_start, end: ss_end};
    }

    let (i, _) = kw(i, "protocol")?;
    commit! {
        let (i, name) = ident(i)?;
        let (i, _) = punct(i, "{")?;
        let (i, managers) = maybe(i, managers(i))?;
        let (i, manages) = many(i, manages)?;
        let (i, groups) = many(i, message_group)?;
        let (i, _) = punct(i, "}")?;
        let (i, _) = punct(i, ";")?;

        let end = i.loc();
        Ok((i, Spanned::new(Span { start, end }, ProtocolItem {
            send_semantics: Spanned::new(ss_span, send_semantics),
            nested,
            path: vec![name],
            managers,
            manages,
            groups,
        })))
    }
}

#[derive(Debug)]
#[allow(large_enum_variant)]
pub enum Item<'filepath> {
    Struct(Spanned<'filepath, StructItem<'filepath>>),
    Union(Spanned<'filepath, UnionItem<'filepath>>),
    Protocol(Spanned<'filepath, ProtocolItem<'filepath>>),
}

fn namespace(i: In) -> PResult<Vec<Item>> {
    let (i, _) = kw(i, "namespace")?;

    commit! {
        let (i, name) = ident(i)?;
        let (i, _) = punct(i, "{")?;
        let (i, mut items) = items(i)?;
        let (i, _) = punct(i, "}")?;
        for it in &mut items {
            match *it  {
                Item::Struct(ref mut i) =>
                    i.data.path.insert(0, name.clone()),
                Item::Union(ref mut i) =>
                    i.data.path.insert(0, name.clone()),
                Item::Protocol(ref mut i) =>
                    i.data.path.insert(0, name.clone()),
            }
        }
        Ok((i, items))
    }
}

fn items(i: In) -> PResult<Vec<Item>> {
    let mut v = Vec::new();
    drive!(i, any!(
        i, "item (struct, union, protocol, or namespace)",
        struct_item(i) => |x| v.push(Item::Struct(x)),
        union_item(i) => |x| v.push(Item::Union(x)),
        protocol_item(i) => |x| v.push(Item::Protocol(x)),
        namespace(i) => |x| v.extend(x),
    ));
    Ok((i, v))
}

#[derive(Debug)]
pub struct TranslationUnit<'filepath> {
    pub cxx_includes: Vec<Spanned<'filepath, CxxInclude<'filepath>>>,
    pub includes: Vec<Spanned<'filepath, Include<'filepath>>>,
    pub usings: Vec<Spanned<'filepath, Using<'filepath>>>,
    pub items: Vec<Item<'filepath>>,
}

fn translation_unit(i: In) -> PResult<Spanned<TranslationUnit>> {
    // Prelude.
    let mut usings = Vec::new();
    let mut includes = Vec::new();
    let mut cxx_includes = Vec::new();

    let start = i.loc();

    drive!(i, any!(
        i, "include or using declaration",
        using(i) => |u| usings.push(u),
        include(i) => |u| includes.push(u),
        cxx_include(i) => |u| cxx_includes.push(u),
    ));

    // Body.
    let (i, items) = items(i)?;

    // Make sure we're at EOF
    let i = skip_ws(i)?;
    if !i.rest().is_empty() {
        return i.expected("item (struct, union, protocol, or namespace)");
    }

    let end = i.loc();

    Ok((i, Spanned::new(Span {start, end}, TranslationUnit {
        cxx_includes,
        includes,
        usings,
        items,
    })))
}

/// Entry point - parses a whole translation unit.
pub fn parse<'a>(src: &'a str, file: &'a Path) -> Result<Spanned<'a, TranslationUnit<'a>>, Error> {
    match translation_unit(In::new(src, file)) {
        Ok((_, v)) => Ok(v),
        Err(err) => {
            // Get the line where the error occurred.
            println!("{}", err.span().start.line - 1);
            let text = src.lines().nth(err.span().start.line - 1)
                          .unwrap_or(""); // Usually happens when the error occurs on the last, empty line

            // Format the final error message.
            let message = format!("Parse Error @ {}:{}: {}\n\
                                   | {}\n{:~>off$}^\n",
                                  err.span().start.line, err.span().start.col, err.message(),
                                  text, "", off=err.span().start.col + 2);

            Err(Error(Box::new(ErrorInner {
                line: err.span().start.line, column: err.span().start.col, message
            })))
        }
    }
}
