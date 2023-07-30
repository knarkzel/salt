use argh::FromArgs;
use color_eyre::{
    eyre::{eyre, WrapErr},
    Help, Result,
};

// Modules
mod eval;
mod parse;

#[derive(FromArgs)]
/// Interpreter for the salt language
struct Args {
    /// file to run
    #[argh(positional)]
    file: String,
}

fn main() -> Result<()> {
    // Nicer panics / error messages
    color_eyre::install()?;

    // Get file
    let Args { file } = argh::from_env();
    let input = std::fs::read_to_string(&file)
        .wrap_err(format!("Failed to read file: \"{file}\""))
        .suggestion("try using a file that exists")?;

    // Parse file and evaluate it
    let exprs =
        parse::parse(&input).map_err(|error| eyre!("Error occurred while parsing: {error:#?}"))?;
    eval::eval(exprs);

    // Success!
    Ok(())
}
