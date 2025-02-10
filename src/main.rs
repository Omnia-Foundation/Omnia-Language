mod lexer;
mod core;
mod parser;
mod codegen;

use std::fmt::Debug;
use std::fs::{read_to_string, File};
use std::io::Read;
use std::path::PathBuf;
use miette;
use std::process::exit;
use clap::Parser as clp;
use miette::{GraphicalReportHandler, Report};
use crate::codegen::Generator;
use crate::core::errors::FailureWriter;
use crate::lexer::Lexer;
use crate::parser::Parser;

#[derive(clp)]
#[command(name = "omnic", version = "0.1", about = "The Omnia Language compiler")]
struct Cli {
    #[arg(long, short, help = "File with source code")]
    input: PathBuf,
    #[arg(long, short, help = "Output file")]
    output: PathBuf,
    #[arg(short, help = "Enables debug and additional info")]
    verbose: bool,
    #[arg(long, help = "With this command only object file should generate")]
    obj: bool
}


fn main() -> miette::Result<()> {
    println!("Welcome to Omnia Compiler!");
    let args = Cli::parse();
    let handler = GraphicalReportHandler::new();
    let mut input = read_to_string(args.input).expect("");
    let mut lexer = Lexer::new(input.clone());
    match lexer.tokenize() {
        Err(e) => {
            eprintln!("{}", FailureWriter { handler, failure: e })
        },
        Ok(tokens) => {
            if args.verbose {
                for token in tokens.0.clone() {
                    println!("{}", token)
                }
            }
            let mut parser = Parser::new(tokens.0, tokens.1);
            match parser.parse() {
                Ok(s) => {
                    if args.verbose {
                        println!("{}", s.clone())
                    }
                    Generator::generate(s, args.output, args.verbose, args.obj);
                }
                Err(f) => {
                    eprintln!("{}", FailureWriter { handler, failure: f })
                }
            }
        }
    }
    Ok(())
}
