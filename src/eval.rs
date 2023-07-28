use crate::parse::{Atom, Expr};
use std::collections::HashMap;

pub fn eval(exprs: Vec<Expr>) {
    // Create new context for variables, then evaluate each expression
    let mut context = HashMap::new();
    for expr in exprs {
        eval_expr(expr, &mut context);
    }
}

fn eval_expr(expr: Expr, context: &mut HashMap<String, Expr>) -> Expr {
    // Evaluate expression
    match expr {
        Expr::Void => expr,
        Expr::Constant(ref atom) => match atom {
            Atom::Name(name) => context
                .get(name)
                .expect(&format!("{name} doesn't exist!"))
                .clone(),
            _ => expr,
        },
        Expr::Let(Atom::Name(name), expr) => {
            context.insert(name, *expr);
            Expr::Void
        }
        Expr::Call(Atom::Name(name), args) if name == "println" => {
            for arg in args {
                print!("{}", eval_expr(arg, context));
            }
            print!("\n");
            Expr::Void
        }
        _ => panic!("Invalid expression: {expr:?}"),
    }
}
