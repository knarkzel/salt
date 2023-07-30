use nom::{
    branch::alt,
    bytes::complete::{take_until, is_not},
    character::complete::{alpha1, multispace0},
    combinator::map,
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, tuple},
};
use nom_supreme::{error::ErrorTree, final_parser::final_parser, tag::complete::tag, ParserExt};
use std::fmt;

type Span<'a> = &'a str;
type IResult<'a, O> = nom::IResult<Span<'a>, O, ErrorTree<Span<'a>>>;

// Whitespace helper from nom docs
fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> IResult<O>
where
    F: FnMut(&'a str) -> IResult<O>,
{
    delimited(multispace0, inner, multispace0)
}

// Atom parsers
#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Name(String),
    String(String),
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Atom::Name(name) => write!(f, "{name}"),
            Atom::String(string) => write!(f, "{string}"),
        }
    }
}

fn parse_variable(input: &str) -> IResult<String> {
    let parser = alpha1.context("Expected name");
    map(parser, str::to_string)(input)
}

fn parse_name(input: &str) -> IResult<Atom> {
    map(parse_variable, Atom::Name)(input)
}

fn parse_string(input: &str) -> IResult<Atom> {
    let parser = delimited(tag("\""), take_until("\""), tag("\"")).context("String is incomplete");
    map(parser, |string: &str| Atom::String(string.to_string()))(input)
}

fn parse_atom(input: &str) -> IResult<Atom> {
    alt((parse_string, parse_name))(input)
}

// Expression parsers
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Void,
    Constant(Atom),
    Let(String, Box<Expr>),
    Call(String, Vec<Expr>),
    Closure(Vec<String>, Vec<Expr>),
    Function(String, Vec<String>, Vec<Expr>),
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
    let parse_statement = separated_pair(parse_variable, ws(tag("=")), parse_atom);
    let parser = preceded(ws(tag("let")), parse_statement).context("Invalid let statement");
    map(parser, |(name, atom)| {
        Expr::Let(name, Box::new(Expr::Constant(atom)))
    })(input)
}

fn parse_call(input: &str) -> IResult<Expr> {
    let parse_args = delimited(
        tag("("),
        separated_list0(tag(","), ws(parse_expr)),
        tag(")"),
    );
    let parser = pair(parse_variable, parse_args).context("Invalid function call");
    map(parser, |(name, args)| Expr::Call(name, args))(input)
}

fn parse_function(input: &str) -> IResult<Expr> {
    let parse_args = delimited(
        tag("("),
        separated_list0(tag(","), ws(parse_variable)),
        tag(")"),
    );
    let parse_body = delimited(tag("{"), ws(many0(parse_expr)), tag("}"));
    let parser = preceded(
        tag("fn"),
        tuple((ws(parse_variable), parse_args, ws(parse_body))),
    );
    map(parser, |(name, args, body)| {
        Expr::Function(name, args, body)
    })(input)
}

fn parse_expr(input: &str) -> IResult<Expr> {
    alt((parse_function, parse_let, parse_call, parse_constant))(input)
}

// Other parsers to be used in the evaluator, such as interpolation
pub fn parse_interpolation(input: &str) -> IResult<Vec<Expr>> {
    let parse_braces = delimited(tag("{"), ws(parse_expr), tag("}"));
    let parse_string = map(is_not("{"), |string: &str| {
        Expr::Constant(Atom::String(string.to_string()))
    });
    many0(alt((parse_braces, parse_string)))(input)
}

// Final parser which ties everything together
pub fn parse(input: &str) -> Result<Vec<Expr>, ErrorTree<&str>> {
    final_parser(many0(ws(parse_expr)))(input)
}
