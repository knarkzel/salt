use argh::FromArgs;
use anyhow::{anyhow, Context, Result};

// Modules
mod parse;
mod eval;

#[derive(FromArgs)]
/// Interpreter for the salt language
struct Args {
    /// file to run
    #[argh(positional)]
    file: std::path::PathBuf,
}

fn main() -> Result<()> {
    // Get arguments from command line
    let args = argh::from_env::<Args>();

    // Read file
    let input = std::fs::read_to_string(&args.file)
        .context(format!("Failed to read file `{}`", args.file.display()))?;

    // Parse input
    let (_, exprs) = parse::parse(&input)
        .map_err(|error| anyhow!("Failed to parse input because of {error:#?}"))?;

    // Evaluate expressions
    eval::eval(exprs);
    
    // Success
    Ok(())
}
