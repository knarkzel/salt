use anyhow::{Context, Result};
use argh::FromArgs;
use ariadne::{Label, Report, ReportKind, Source};
use nom_supreme::{
    error::{GenericErrorTree, StackContext},
    final_parser::{Location, RecreateContext},
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
    // Get file
    let Args { file } = argh::from_env();
    let input = std::fs::read_to_string(&file).context(format!("Failed to read file `{file}`"))?;

    // Parse file, print errors if there are any
    match parse::parse(&input) {
        Ok(exprs) => eval::eval(exprs),
        Err(GenericErrorTree::Stack { contexts, .. }) => {
            // Get offset for line number in <line>:<column>
            let mut report = Report::build(ReportKind::Error, file.as_str(), 4);
            for (tail, context) in contexts.iter().take(1) {
                if let StackContext::Context(message) = context {
                    // Get location
                    let Location { column, .. } = Location::recreate_context(input.as_str(), tail);
                    let until_newline = tail.find("\n").unwrap_or(tail.len());
                    let range = (column - 1)..until_newline;
                    let label = Label::new((file.as_str(), range)).with_message(message);
                    report = report.with_label(label);
                }
            }

            // Print report
            let cache = (file.as_str(), Source::from(input.clone()));
            report.finish().print(cache)?;
        }
        Err(error) => {
            eprintln!("Error occurred while parsing: {error}");
            std::process::exit(1);
        }
    };

    // Success!
    Ok(())
}
