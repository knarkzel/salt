use nom::{
    branch::alt,
    bytes::complete::take_until,
    character::complete::{alpha1, multispace0},
    combinator::map,
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair},
};
use nom_supreme::{error::ErrorTree, final_parser::final_parser, tag::complete::tag, ParserExt};
use std::fmt;

type Span<'a> = &'a str;
type IResult<'a, O> = nom::IResult<Span<'a>, O, ErrorTree<Span<'a>>>;

// Whitespace helper from nom docs
fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> IResult<O>
where
    F: Fn(&'a str) -> IResult<O>,
{
    delimited(multispace0, inner, multispace0)
}

// Atom parsers
#[derive(Debug, Clone)]
pub enum Atom {
    Name(String),
    String(String),
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Atom::Name(name) => write!(f, "{name}"),
            Atom::String(string) => write!(f, "\"{string}\""),
        }
    }
}

fn parse_name(input: &str) -> IResult<Atom> {
    map(alpha1.context("Expected name"), |name: &str| {
        Atom::Name(name.to_string())
    })(input)
}

fn parse_string(input: &str) -> IResult<Atom> {
    let parser = delimited(tag("\""), take_until("\""), tag("\"")).context("String is incomplete");
    map(parser.cut(), |string: &str| {
        Atom::String(string.to_string())
    })(input)
}

fn parse_atom(input: &str) -> IResult<Atom> {
    alt((parse_string, parse_name))(input)
}

// Expression parsers
#[derive(Debug, Clone)]
pub enum Expr {
    Void,
    Constant(Atom),
    Let(Atom, Box<Expr>),
    Call(Atom, Vec<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Constant(atom) => write!(f, "{atom}"),
            _ => Ok(()),
        }
    }
}

fn parse_constant(input: &str) -> IResult<Expr> {
    map(parse_atom, Expr::Constant)(input)
}

fn parse_let(input: &str) -> IResult<Expr> {
    let parse_statement = separated_pair(parse_name, ws(tag("=")), parse_atom);
    let parser = preceded(ws(tag("let")), parse_statement).context("Invalid let statement");
    map(parser.cut(), |(name, atom)| {
        Expr::Let(name, Box::new(Expr::Constant(atom)))
    })(input)
}

fn parse_call(input: &str) -> IResult<Expr> {
    let parse_args = delimited(
        tag("("),
        separated_list0(tag(","), ws(parse_expr)),
        tag(")"),
    );
    let parser = pair(parse_name, parse_args).context("Invalid function call");
    map(parser.cut(), |(name, args)| Expr::Call(name, args))(input)
}

fn parse_expr(input: &str) -> IResult<Expr> {
    alt((parse_let, parse_call, parse_constant))(input)
}

// Final parser which ties everything together
pub fn parse(input: &str) -> Result<Vec<Expr>, ErrorTree<&str>> {
    final_parser(many0(ws(parse_expr)))(input)
}
