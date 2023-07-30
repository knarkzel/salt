use crate::parse::{parse_interpolation, Atom, Expr};
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
        Expr::Constant(Atom::String(ref string)) => match parse_interpolation(string) {
            Ok((_, exprs)) => {
                match exprs.len() {
                    0 | 1 => return expr,
                    _ => {
                        let mut output = String::with_capacity(string.len());
                        for mut expr in exprs {
                            // Keep evaluating until it's just a string
                            loop {
                                let new_expr = eval_expr(expr.clone(), context);
                                if expr == new_expr {
                                    break;
                                } else {
                                    expr = new_expr;
                                }
                            }
                            output.push_str(&expr.to_string());
                        }
                        return Expr::Constant(Atom::String(output));
                    }
                }
            }
            _ => expr,
        },
        Expr::Constant(ref atom) => match atom {
            Atom::Name(name) => context
                .get(name)
                .expect(&format!("{name} doesn't exist!"))
                .clone(),
            _ => expr,
        },
        Expr::Let(name, expr) => {
            context.insert(name, *expr);
            Expr::Void
        }
        Expr::Call(name, args) => {
            if name == "println" {
                for arg in args {
                    print!("{}", eval_expr(arg, context));
                }
                print!("\n");
            } else {
                match context.get(&name) {
                    Some(Expr::Closure(parameters, body)) => {
                        // Create new scope for context and run each line
                        let mut scope = context.clone();

                        for (parameter, arg) in parameters.into_iter().zip(args.into_iter()) {
                            let expr = eval_expr(arg, &mut scope);
                            scope.insert(parameter.clone(), expr);
                        }

                        for expr in body {
                            eval_expr(expr.clone(), &mut scope);
                        }
                    }
                    _ => panic!("function `{name}` doesn't exist."),
                }
            }
            Expr::Void
        }
        Expr::Function(name, args, body) => {
            context.insert(name, Expr::Closure(args, body));
            Expr::Void
        }
        _ => panic!("Invalid expression: {expr:?}"),
    }
}
