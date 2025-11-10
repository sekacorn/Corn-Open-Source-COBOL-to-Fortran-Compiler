// Copyright (c) 2025 sekacorn
// Contact: sekacorn@gmail.com
// All rights reserved.
//
// This file is part of the Corn COBOL-to-Fortran Compiler.
// Licensed under the Corn Dual License.
// See LICENSE-COMMERCIAL.txt and LICENSE-OPEN-SOURCE.txt for details.

//! This module handles the parsing of COBOL source code.
//! Now supports:
//! - File Handling (OPEN, READ, WRITE)
//! - Advanced Loops (PERFORM VARYING, PERFORM UNTIL)
//! - COBOL Embedded SQL (EXEC SQL ... END-EXEC)

use std::collections::HashMap;

/// Represents different types of COBOL statements.
#[derive(Debug)]
pub enum CobolStatement {
    Display(String),                // `DISPLAY` statement
    StopRun,                        // `STOP RUN` statement
    Move(String, String),            // `MOVE <VALUE> TO <VARIABLE>`
    Compute(String, String),         // `COMPUTE <VAR> = <EXPR>`
    Perform(String),                 // `PERFORM <PARAGRAPH>`
    PerformUntil(String, Vec<CobolStatement>),  // `PERFORM UNTIL <CONDITION>`
    PerformVarying(String, String, String, Vec<CobolStatement>), // `PERFORM VARYING <VAR> FROM <X> TO <Y>`
    OpenFile(String, String),        // `OPEN INPUT/OUTPUT <FILE>`
    ReadFile(String),                // `READ <FILE>`
    WriteFile(String, Vec<String>),  // `WRITE <FIELDS> TO <FILE>`
    ExecSQL(String),                 // `EXEC SQL ... END-EXEC`
}

/// Represents a COBOL paragraph (block of statements).
#[derive(Debug)]
pub struct CobolParagraph {
    pub name: String,
    pub statements: Vec<CobolStatement>,
}

/// Represents a parsed COBOL program.
#[derive(Debug)]
pub struct CobolProgram {
    pub name: String,
    pub paragraphs: HashMap<String, CobolParagraph>,
}

/// Parses a COBOL source code string and returns a `CobolProgram`.
pub fn parse_cobol_code(input: &str) -> CobolProgram {
    println!("Parsing COBOL Code...");

    let mut program_name = String::new();
    let mut paragraphs = HashMap::new();
    let mut current_paragraph: Option<CobolParagraph> = None;

    for line in input.lines() {
        let line = line.trim();

        // Ignore empty lines or comments
        if line.is_empty() || line.starts_with("*") {
            continue;
        }

        let tokens: Vec<&str> = line.split_whitespace().collect();

        // Detect program name
        if tokens.starts_with(&["PROGRAM-ID."]) {
            program_name = tokens[1].trim_end_matches('.').to_string();
        }
        // Detect paragraph names
        else if tokens.len() == 1 && line.ends_with(".") {
            if let Some(paragraph) = current_paragraph.take() {
                paragraphs.insert(paragraph.name.clone(), paragraph);
            }
            current_paragraph = Some(CobolParagraph {
                name: line.trim_end_matches('.').to_string(),
                statements: Vec::new(),
            });
        }
        // Detect COBOL statements
        else if let Some(paragraph) = &mut current_paragraph {
            if tokens.starts_with(&["DISPLAY"]) {
                let message = tokens[1..].join(" ").trim_end_matches('.').to_string();
                paragraph.statements.push(CobolStatement::Display(message));
            } else if tokens.starts_with(&["STOP", "RUN."]) {
                paragraph.statements.push(CobolStatement::StopRun);
            } else if tokens.starts_with(&["MOVE"]) {
                let value = tokens[1].to_string();
                let variable = tokens.last().unwrap().trim_end_matches('.').to_string();
                paragraph.statements.push(CobolStatement::Move(value, variable));
            } else if tokens.starts_with(&["COMPUTE"]) {
                let variable = tokens[1].to_string();
                let expr = tokens[3..].join(" ").trim_end_matches('.').to_string();
                paragraph.statements.push(CobolStatement::Compute(variable, expr));
            } else if tokens.starts_with(&["PERFORM", "UNTIL"]) {
                let condition = tokens[2..].join(" ").trim_end_matches('.').to_string();
                paragraph.statements.push(CobolStatement::PerformUntil(condition, Vec::new()));
            } else if tokens.starts_with(&["PERFORM", "VARYING"]) {
                let variable = tokens[2].to_string();
                let start_value = tokens[4].to_string();
                let end_value = tokens[6].to_string();
                paragraph.statements.push(CobolStatement::PerformVarying(variable, start_value, end_value, Vec::new()));
            } else if tokens.starts_with(&["OPEN"]) {
                let mode = tokens[1].to_string(); // INPUT or OUTPUT
                let file = tokens.last().unwrap().trim_end_matches('.').to_string();
                paragraph.statements.push(CobolStatement::OpenFile(mode, file));
            } else if tokens.starts_with(&["READ"]) {
                let file = tokens[1].trim_end_matches('.').to_string();
                paragraph.statements.push(CobolStatement::ReadFile(file));
            } else if tokens.starts_with(&["WRITE"]) {
                let file = tokens.last().unwrap().trim_end_matches('.').to_string();
                let fields = tokens[1..tokens.len()-2].iter().map(|s| s.to_string()).collect();
                paragraph.statements.push(CobolStatement::WriteFile(file, fields));
            } else if tokens.starts_with(&["EXEC", "SQL"]) {
                let sql_statement = line.trim_end_matches('.').to_string();
                paragraph.statements.push(CobolStatement::ExecSQL(sql_statement));
            }
        }
    }

    if let Some(paragraph) = current_paragraph {
        paragraphs.insert(paragraph.name.clone(), paragraph);
    }

    CobolProgram {
        name: program_name,
        paragraphs,
    }
}
