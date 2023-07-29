use anyhow::{Context, Result};
use argh::FromArgs;
use ariadne::{Label, Report, ReportKind, Source, ColorGenerator};
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
            // Get offset of first error for <line>:<column>
            let offset = contexts.iter().next().map(|(tail, _)| {
                let location = Location::recreate_context(input.as_str(), tail);
                location.column
            });

            // Build report for printing nice errors
            let mut colors = ColorGenerator::new();
            let mut report = Report::build(ReportKind::Error, file.as_str(), offset.unwrap_or(1));
            for (tail, context) in contexts {
                if let StackContext::Context(message) = context {
                    // Get range for current line and highlight it with message
                    let Location { column, .. } = Location::recreate_context(input.as_str(), tail);
                    let next_whitespace = tail.find(char::is_whitespace).unwrap_or(column);
                    let range = (column - 1)..(column - 1) + next_whitespace;
                    let label = Label::new((file.as_str(), range))
                        .with_message(message)
                        .with_color(colors.next());
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
