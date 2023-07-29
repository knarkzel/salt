use argh::FromArgs;

// Modules
mod eval;
mod parse;

#[derive(FromArgs)]
/// Interpreter for the salt language
struct Args {
    /// file to run
    #[argh(positional)]
    file: std::path::PathBuf,
}

fn main() {
    // Get file, then parse input and evaluate it
    let args = argh::from_env::<Args>();
    let input = std::fs::read_to_string(&args.file)
        .expect(&format!("Failed to read file `{}`", args.file.display()));
    let exprs = dbg!(parse::parse(&input)).unwrap();
    eval::eval(exprs);
}
