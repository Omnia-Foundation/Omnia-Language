use std::fmt::{Display, Formatter};
use std::io::ErrorKind;
use miette::{Diagnostic, GraphicalReportHandler, Report, SourceSpan};
use thiserror::Error;
use crate::core::utils::string_utils::StringBuilder;
use crate::lexer::token::{Token, TokenType};

#[derive(Diagnostic, Debug, Error, Clone)]
pub enum Failure {
    #[error("Unexpected character")]
    #[diagnostic(code(lexer::unknown_char))]
    LexerUnexpectedChar
    {
        #[source_code]
        src: String,
        #[label("Unexpected '{ch}' here")]
        span: SourceSpan,
        ch: char
    },

    #[error("Using expression as statement")]
    #[diagnostic(code(parser::using_expr_as_stmt))]
    ParserUsingExpressionAsStatement {
        #[source_code]
        src: String,
        #[label("You are trying to use expression {expr} as statement")]
        span: SourceSpan,
        expr: String
    },

    #[error("Unexpected token")]
    #[diagnostic(code(parser::unexpected_token_with_type))]
    ParserUnexpectedTokenWithType {
        #[source_code]
        src: String,
        #[label("Unexpected token: required token with type {required}, got {got}")]
        span: SourceSpan,
        required: TokenType,
        got: TokenType
    },
    #[error("Unexpected token")]
    #[diagnostic(code(parser::unexpected_token))]
    ParserUnexpectedToken {
        #[source_code]
        src: String,
        #[label("Unexpected token: {token}")]
        span: SourceSpan,
        token: Token
    },
    #[error("Unexpected token for BinaryOperation")]
    #[diagnostic(code(utils::unexpected_token_for_binop))]
    UtilsUnexpectedTokenForBinaryOperation {
        #[source_code]
        src: String,
        #[label("Unexpected token for BinaryOperation: {token}")]
        span: SourceSpan,
        token: Token
    },
    #[error("Unexpected token for ConditionalOperation")]
    #[diagnostic(code(utils::unexpected_token_for_conop))]
    UtilsUnexpectedTokenForConditionalOperation {
        #[label("Unexpected token for ConditionalOperation: {token}")]
        span: SourceSpan,
        token: Token
    },
    #[error("Variable with name {name} already exists!")]
    #[diagnostic(code(codegen::variable_already_exists))]
    CodegenVariableAlreadyExists {
        name: String
    },
    #[error("Current basic block is missing!")]
    #[diagnostic(code(codegen::cur_basic_block_missing))]
    CodegenCurrentBasicBlockIsMissing,
    #[error("Basic block is not linked to a function!")]
    #[diagnostic(code(codegen::basic_block_not_linked))]
    CodegenBasicBlockNotLinkedToFunction,
}

pub struct FailureWriter {
    pub handler: GraphicalReportHandler,
    pub failure: Failure
}
impl Display for FailureWriter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.handler.render_report(f, &self.failure)
    }
}

