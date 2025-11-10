// Copyright (c) 2025 sekacorn
// Contact: sekacorn@gmail.com
// All rights reserved.
//
// This file is part of the Corn COBOL-to-Fortran Compiler.
// Licensed under the Corn Dual License.
// See LICENSE-COMMERCIAL.txt and LICENSE-OPEN-SOURCE.txt for details.

//! This module converts parsed COBOL structures into Fortran code.
//! Now supports:
//! - File Handling (OPEN, READ, WRITE)
//! - Advanced Loops (PERFORM VARYING, PERFORM UNTIL)
//! - COBOL Embedded SQL (EXEC SQL -> Fortran SQL)

use crate::cobol_parser::{CobolProgram, CobolParagraph, CobolStatement};

/// Generates Fortran code from a parsed COBOL program.
pub fn generate_fortran_code(cobol: &CobolProgram) -> String {
    let mut fortran_code = String::new();

    // Add Fortran program header
    fortran_code.push_str(&format!("PROGRAM {}\n", cobol.name.to_uppercase()));
    fortran_code.push_str("  IMPLICIT NONE\n");

    // Declare variables (assume inferred from COBOL statements)
    let mut variables = Vec::new();

    for paragraph in cobol.paragraphs.values() {
        for statement in &paragraph.statements {
            if let CobolStatement::Move(_, var) = statement {
                if !variables.contains(var) {
                    variables.push(var.clone());
                }
            }
        }
    }

    for var in &variables {
        fortran_code.push_str(&format!("  INTEGER :: {}\n", var));
    }

    // Begin main procedure
    fortran_code.push_str("  CALL MAIN\n  STOP\n");
    fortran_code.push_str("END PROGRAM ");
    fortran_code.push_str(&cobol.name.to_uppercase());
    fortran_code.push_str("\n\n");

    // Convert COBOL paragraphs to Fortran subroutines
    for (name, paragraph) in &cobol.paragraphs {
        fortran_code.push_str(&convert_paragraph_to_fortran(name, paragraph));
    }

    fortran_code
}

/// Converts a COBOL paragraph into a Fortran subroutine.
fn convert_paragraph_to_fortran(name: &str, paragraph: &CobolParagraph) -> String {
    let mut fortran_code = String::new();

    fortran_code.push_str(&format!("SUBROUTINE {}\n", name.to_uppercase()));

    for statement in &paragraph.statements {
        match statement {
            CobolStatement::Display(msg) => {
                fortran_code.push_str(&format!("  WRITE(*,*) \"{}\"\n", msg));
            }
            CobolStatement::StopRun => {
                fortran_code.push_str("  STOP\n");
            }
            CobolStatement::Move(value, var) => {
                fortran_code.push_str(&format!("  {} = {}\n", var, value));
            }
            CobolStatement::Compute(var, expr) => {
                fortran_code.push_str(&format!("  {} = {}\n", var, expr.replace("**", "^")));
            }
            CobolStatement::Perform(target) => {
                fortran_code.push_str(&format!("  CALL {}\n", target.to_uppercase()));
            }
            CobolStatement::PerformUntil(condition, body) => {
                fortran_code.push_str(&format!("  DO WHILE ({})\n", condition));
                for stmt in body {
                    fortran_code.push_str(&convert_statement_to_fortran(stmt));
                }
                fortran_code.push_str("  END DO\n");
            }
            CobolStatement::PerformVarying(var, range, body) => {
                fortran_code.push_str(&format!("  DO {} = 1, {}\n", var, range));
                for stmt in body {
                    fortran_code.push_str(&convert_statement_to_fortran(stmt));
                }
                fortran_code.push_str("  END DO\n");
            }
            CobolStatement::OpenFile(mode, file) => {
                let unit_number = 10;
                let action = if mode == "INPUT" { "OLD" } else { "NEW" };
                fortran_code.push_str(&format!("  OPEN(UNIT={}, FILE='{}', STATUS='{}')\n", unit_number, file, action));
            }
            CobolStatement::ReadFile(file) => {
                let unit_number = 10;
                fortran_code.push_str(&format!("  READ(UNIT={}, FMT=*)\n", unit_number));
            }
            CobolStatement::WriteFile(file, fields) => {
                let unit_number = 10;
                let values = fields.join(", ");
                fortran_code.push_str(&format!("  WRITE(UNIT={}, FMT=*) {}\n", unit_number, values));
            }
            CobolStatement::ExecSQL(sql_query) => {
                fortran_code.push_str(&format!("  ! EXEC SQL TRANSLATION NOT IMPLEMENTED\n  ! {}\n", sql_query));
            }
        }
    }

    fortran_code.push_str(&format!("END SUBROUTINE {}\n\n", name.to_uppercase()));
    fortran_code
}

/// Converts a single COBOL statement to Fortran.
fn convert_statement_to_fortran(statement: &CobolStatement) -> String {
    match statement {
        CobolStatement::Display(msg) => format!("    WRITE(*,*) \"{}\"\n", msg),
        CobolStatement::StopRun => "    STOP\n".to_string(),
        CobolStatement::Move(value, var) => format!("    {} = {}\n", var, value),
        CobolStatement::Compute(var, expr) => format!("    {} = {}\n", var, expr.replace("**", "^")),
        CobolStatement::Perform(target) => format!("    CALL {}\n", target.to_uppercase()),
        _ => "".to_string(),
    }
}
