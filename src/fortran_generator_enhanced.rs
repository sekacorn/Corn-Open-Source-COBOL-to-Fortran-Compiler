//! Enhanced Fortran Code Generator
//! Generates accurate Fortran code from parsed COBOL with full type support
//! Handles:
//! - Complete Data Division translation to Fortran TYPE definitions
//! - PIC clause to Fortran type mapping
//! - Enhanced statement translation
//! - Proper variable declarations
//! - File I/O translation

use crate::cobol_parser_enhanced::*;

pub struct FortranGenerator {
    indent_level: usize,
    unit_counter: i32,
}

impl FortranGenerator {
    pub fn new() -> Self {
        FortranGenerator {
            indent_level: 0,
            unit_counter: 10,
        }
    }

    /// Generate complete Fortran program from COBOL
    pub fn generate(&mut self, program: &CobolProgram) -> String {
        let mut output = String::new();

        // Program header
        output.push_str(&format!("PROGRAM {}\n", program.name.to_uppercase()));
        output.push_str("  IMPLICIT NONE\n\n");

        // Generate type definitions and variable declarations from Data Division
        output.push_str(&self.generate_data_division(&program.data_division, &program.symbol_table));

        // Main program logic
        output.push_str("\n  ! Main program execution\n");
        output.push_str("  CALL MAIN\n");
        output.push_str("  STOP\n");
        output.push_str(&format!("END PROGRAM {}\n\n", program.name.to_uppercase()));

        // Generate subroutines from paragraphs
        for (name, paragraph) in &program.paragraphs {
            output.push_str(&self.generate_paragraph(name, paragraph, &program.symbol_table));
        }

        output
    }

    /// Generate Fortran code for Data Division
    fn generate_data_division(&mut self, data_div: &DataDivision, symbol_table: &SymbolTable) -> String {
        let mut output = String::new();

        output.push_str("  ! Data Division - Variable Declarations\n");

        // Generate from Working Storage
        if !data_div.working_storage.is_empty() {
            output.push_str("  ! Working Storage Section\n");
            for var in &data_div.working_storage {
                output.push_str(&self.generate_variable_declaration(var, symbol_table));
            }
            output.push_str("\n");
        }

        // Generate from Local Storage
        if !data_div.local_storage.is_empty() {
            output.push_str("  ! Local Storage Section\n");
            for var in &data_div.local_storage {
                output.push_str(&self.generate_variable_declaration(var, symbol_table));
            }
            output.push_str("\n");
        }

        // Generate from Linkage Section
        if !data_div.linkage_section.is_empty() {
            output.push_str("  ! Linkage Section (Subroutine Parameters)\n");
            for var in &data_div.linkage_section {
                output.push_str(&self.generate_variable_declaration(var, symbol_table));
            }
            output.push_str("\n");
        }

        output
    }

    /// Generate Fortran variable declaration from COBOL variable
    fn generate_variable_declaration(&mut self, var: &VariableDeclaration, symbol_table: &SymbolTable) -> String {
        let mut output = String::new();

        // Only generate declarations for elementary items (have PIC clause) or arrays
        if let Some(ref data_type) = var.data_type {
            let fortran_type = self.pic_to_fortran_type(&data_type.picture);
            let var_name = var.name.replace("-", "_").to_uppercase();

            // Handle OCCURS (arrays)
            if let Some(ref occurs) = var.occurs {
                output.push_str(&format!("  {}, DIMENSION({}) :: {}\n",
                    fortran_type, occurs.times, var_name));
            } else {
                output.push_str(&format!("  {} :: {}\n", fortran_type, var_name));
            }

            // Handle VALUE initialization
            if let Some(ref value) = var.value {
                let clean_value = value.trim_matches('"').trim_matches('\'');
                output.push_str(&format!("  DATA {} / {} /\n", var_name, clean_value));
            }
        } else if var.level == 1 && !var.children.is_empty() {
            // This is a group item - generate TYPE definition
            output.push_str(&self.generate_type_definition(var));
        }

        // Process children recursively for nested structures
        for child in &var.children {
            output.push_str(&self.generate_variable_declaration(child, symbol_table));
        }

        output
    }

    /// Generate Fortran TYPE definition for group items
    fn generate_type_definition(&self, var: &VariableDeclaration) -> String {
        let mut output = String::new();
        let type_name = var.name.replace("-", "_").to_uppercase();

        output.push_str(&format!("  TYPE {}_TYPE\n", type_name));

        for child in &var.children {
            if let Some(ref data_type) = child.data_type {
                let fortran_type = self.pic_to_fortran_type(&data_type.picture);
                let field_name = child.name.replace("-", "_").to_uppercase();

                if let Some(ref occurs) = child.occurs {
                    output.push_str(&format!("    {}, DIMENSION({}) :: {}\n",
                        fortran_type, occurs.times, field_name));
                } else {
                    output.push_str(&format!("    {} :: {}\n", fortran_type, field_name));
                }
            }
        }

        output.push_str(&format!("  END TYPE {}_TYPE\n", type_name));
        output.push_str(&format!("  TYPE({}_TYPE) :: {}\n\n", type_name, type_name));

        output
    }

    /// Map COBOL PIC clause to Fortran type
    fn pic_to_fortran_type(&self, pic: &PictureType) -> String {
        match pic {
            PictureType::Numeric { digits, signed } => {
                if *digits <= 9 {
                    "INTEGER".to_string()
                } else {
                    "INTEGER(KIND=8)".to_string() // Use 64-bit for large numbers
                }
            }
            PictureType::Decimal { integer_digits, decimal_digits, signed } => {
                // Use REAL for decimal numbers
                if integer_digits + decimal_digits <= 7 {
                    "REAL".to_string()
                } else {
                    "REAL(KIND=8)".to_string() // Double precision for larger numbers
                }
            }
            PictureType::Alphanumeric { length } => {
                format!("CHARACTER(LEN={})", length)
            }
            PictureType::Alphabetic { length } => {
                format!("CHARACTER(LEN={})", length)
            }
            PictureType::Edited { pattern } => {
                // For edited pictures, use character string
                let length = pattern.len();
                format!("CHARACTER(LEN={})", length)
            }
        }
    }

    /// Generate Fortran subroutine from COBOL paragraph
    fn generate_paragraph(&mut self, name: &str, paragraph: &CobolParagraph, symbol_table: &SymbolTable) -> String {
        let mut output = String::new();

        output.push_str(&format!("SUBROUTINE {}\n", name.to_uppercase().replace("-", "_")));
        output.push_str("  IMPLICIT NONE\n\n");

        for statement in &paragraph.statements {
            output.push_str(&self.generate_statement(statement, 2));
        }

        output.push_str(&format!("END SUBROUTINE {}\n\n", name.to_uppercase().replace("-", "_")));

        output
    }

    /// Generate Fortran code for a COBOL statement
    fn generate_statement(&mut self, statement: &CobolStatement, indent: usize) -> String {
        let indent_str = "  ".repeat(indent);
        let mut output = String::new();

        match statement {
            CobolStatement::Display(items) => {
                let items_str = items.join(", ");
                output.push_str(&format!("{}WRITE(*,*) {}\n", indent_str, items_str));
            }

            CobolStatement::StopRun => {
                output.push_str(&format!("{}STOP\n", indent_str));
            }

            CobolStatement::Exit => {
                output.push_str(&format!("{}RETURN\n", indent_str));
            }

            CobolStatement::Continue => {
                output.push_str(&format!("{}CONTINUE\n", indent_str));
            }

            CobolStatement::Move(from, to) => {
                let from_clean = from.replace("-", "_").to_uppercase();
                let to_clean = to.replace("-", "_").to_uppercase();
                output.push_str(&format!("{}{} = {}\n", indent_str, to_clean, from_clean));
            }

            CobolStatement::Add { operands, to, giving, rounded } => {
                if let Some(target) = giving {
                    let sum = operands.join(" + ");
                    let target_clean = target.replace("-", "_").to_uppercase();
                    output.push_str(&format!("{}{} = {}\n", indent_str, target_clean, sum));
                } else if let Some(target) = to {
                    let sum = operands.join(" + ");
                    let target_clean = target.replace("-", "_").to_uppercase();
                    output.push_str(&format!("{}{} = {} + {}\n", indent_str, target_clean, target_clean, sum));
                }
            }

            CobolStatement::Subtract { operands, from, giving, rounded } => {
                if let Some(target) = giving {
                    let diff = operands.join(" - ");
                    let from_clean = from.replace("-", "_").to_uppercase();
                    let target_clean = target.replace("-", "_").to_uppercase();
                    output.push_str(&format!("{}{} = {} - {}\n", indent_str, target_clean, from_clean, diff));
                } else {
                    let diff = operands.join(" - ");
                    let from_clean = from.replace("-", "_").to_uppercase();
                    output.push_str(&format!("{}{} = {} - {}\n", indent_str, from_clean, from_clean, diff));
                }
            }

            CobolStatement::Multiply { operand1, by, giving, rounded } => {
                let op1_clean = operand1.replace("-", "_").to_uppercase();
                let by_clean = by.replace("-", "_").to_uppercase();

                if let Some(target) = giving {
                    let target_clean = target.replace("-", "_").to_uppercase();
                    output.push_str(&format!("{}{} = {} * {}\n", indent_str, target_clean, op1_clean, by_clean));
                } else {
                    output.push_str(&format!("{}{} = {} * {}\n", indent_str, op1_clean, op1_clean, by_clean));
                }
            }

            CobolStatement::Divide { dividend, divisor, giving, remainder, rounded } => {
                let dividend_clean = dividend.replace("-", "_").to_uppercase();
                let divisor_clean = divisor.replace("-", "_").to_uppercase();

                if let Some(target) = giving {
                    let target_clean = target.replace("-", "_").to_uppercase();
                    output.push_str(&format!("{}{} = {} / {}\n", indent_str, target_clean, dividend_clean, divisor_clean));

                    if let Some(rem) = remainder {
                        let rem_clean = rem.replace("-", "_").to_uppercase();
                        output.push_str(&format!("{}{} = MOD({}, {})\n", indent_str, rem_clean, dividend_clean, divisor_clean));
                    }
                }
            }

            CobolStatement::Compute { variable, expression, rounded } => {
                let var_clean = variable.replace("-", "_").to_uppercase();
                let expr_clean = expression.replace("**", "**");
                output.push_str(&format!("{}{} = {}\n", indent_str, var_clean, expr_clean));
            }

            CobolStatement::If { condition, then_block, else_block } => {
                let condition_str = self.generate_condition(condition);
                output.push_str(&format!("{}IF ({}) THEN\n", indent_str, condition_str));

                for stmt in then_block {
                    output.push_str(&self.generate_statement(stmt, indent + 1));
                }

                if let Some(else_stmts) = else_block {
                    output.push_str(&format!("{}ELSE\n", indent_str));
                    for stmt in else_stmts {
                        output.push_str(&self.generate_statement(stmt, indent + 1));
                    }
                }

                output.push_str(&format!("{}END IF\n", indent_str));
            }

            CobolStatement::Evaluate { subject, when_clauses, when_other } => {
                let subject_clean = subject.replace("-", "_").to_uppercase();
                output.push_str(&format!("{}SELECT CASE ({})\n", indent_str, subject_clean));

                for (value, stmts) in when_clauses {
                    output.push_str(&format!("{}CASE ({})\n", indent_str, value));
                    for stmt in stmts {
                        output.push_str(&self.generate_statement(stmt, indent + 1));
                    }
                }

                if let Some(default_stmts) = when_other {
                    output.push_str(&format!("{}CASE DEFAULT\n", indent_str));
                    for stmt in default_stmts {
                        output.push_str(&self.generate_statement(stmt, indent + 1));
                    }
                }

                output.push_str(&format!("{}END SELECT\n", indent_str));
            }

            CobolStatement::Perform(target) => {
                let target_clean = target.replace("-", "_").to_uppercase();
                output.push_str(&format!("{}CALL {}\n", indent_str, target_clean));
            }

            CobolStatement::PerformUntil { condition, body } => {
                let cond_str = self.generate_condition(condition);
                output.push_str(&format!("{}DO WHILE (.NOT. ({}))\n", indent_str, cond_str));

                for stmt in body {
                    output.push_str(&self.generate_statement(stmt, indent + 1));
                }

                output.push_str(&format!("{}END DO\n", indent_str));
            }

            CobolStatement::PerformVarying { variable, from, to, by, body } => {
                let var_clean = variable.replace("-", "_").to_uppercase();
                let step = by.as_ref().map(|s| s.as_str()).unwrap_or("1");

                output.push_str(&format!("{}DO {} = {}, {}, {}\n", indent_str, var_clean, from, to, step));

                for stmt in body {
                    output.push_str(&self.generate_statement(stmt, indent + 1));
                }

                output.push_str(&format!("{}END DO\n", indent_str));
            }

            CobolStatement::OpenFile { mode, files } => {
                for file in files {
                    let unit = self.unit_counter;
                    self.unit_counter += 1;

                    let action = match mode.as_str() {
                        "INPUT" => "READ",
                        "OUTPUT" => "WRITE",
                        "I-O" => "READWRITE",
                        _ => "READ",
                    };

                    let file_clean = file.replace("-", "_");
                    output.push_str(&format!("{}OPEN(UNIT={}, FILE='{}', ACTION='{}')\n",
                        indent_str, unit, file_clean, action));
                }
            }

            CobolStatement::ReadFile { file, into, at_end } => {
                output.push_str(&format!("{}READ(UNIT=10, FMT=*, IOSTAT=IO_STAT)\n", indent_str));
                if let Some(at_end_block) = at_end {
                    output.push_str(&format!("{}IF (IO_STAT /= 0) THEN\n", indent_str));
                    for stmt in at_end_block {
                        output.push_str(&self.generate_statement(stmt, indent + 1));
                    }
                    output.push_str(&format!("{}END IF\n", indent_str));
                }
            }

            CobolStatement::WriteFile { file, from, after } => {
                if let Some(record) = from {
                    let record_clean = record.replace("-", "_").to_uppercase();
                    output.push_str(&format!("{}WRITE(UNIT=10, FMT=*) {}\n", indent_str, record_clean));
                }
            }

            CobolStatement::Call { program, using } => {
                let prog_clean = program.replace("-", "_").to_uppercase();
                let params: Vec<String> = using.iter().map(|p| match p {
                    CallParameter::ByReference(name) |
                    CallParameter::ByContent(name) |
                    CallParameter::ByValue(name) => name.replace("-", "_").to_uppercase(),
                }).collect();

                if params.is_empty() {
                    output.push_str(&format!("{}CALL {}\n", indent_str, prog_clean));
                } else {
                    output.push_str(&format!("{}CALL {}({})\n", indent_str, prog_clean, params.join(", ")));
                }
            }

            CobolStatement::ExecSQL(sql) => {
                output.push_str(&format!("{}! EXEC SQL TRANSLATION NOT YET IMPLEMENTED\n", indent_str));
                output.push_str(&format!("{}! {}\n", indent_str, sql));
            }

            _ => {
                output.push_str(&format!("{}! Statement not yet implemented\n", indent_str));
            }
        }

        output
    }

    /// Generate Fortran condition from COBOL condition
    fn generate_condition(&self, condition: &Condition) -> String {
        match condition {
            Condition::Simple { left, operator, right } => {
                let left_clean = left.replace("-", "_").to_uppercase();
                let right_clean = right.replace("-", "_").to_uppercase();

                let fort_op = match operator.as_str() {
                    "=" | "EQUAL" => "==",
                    ">" | "GREATER" => ">",
                    "<" | "LESS" => "<",
                    ">=" => ">=",
                    "<=" => "<=",
                    "NOT=" | "<>" => "/=",
                    _ => operator.as_str(),
                };

                format!("{} {} {}", left_clean, fort_op, right_clean)
            }

            Condition::And(cond1, cond2) => {
                let c1 = self.generate_condition(cond1);
                let c2 = self.generate_condition(cond2);
                format!("({}) .AND. ({})", c1, c2)
            }

            Condition::Or(cond1, cond2) => {
                let c1 = self.generate_condition(cond1);
                let c2 = self.generate_condition(cond2);
                format!("({}) .OR. ({})", c1, c2)
            }

            Condition::Not(cond) => {
                let c = self.generate_condition(cond);
                format!(".NOT. ({})", c)
            }

            Condition::ConditionName(name) => {
                name.replace("-", "_").to_uppercase()
            }
        }
    }
}

/// Generate Fortran code from enhanced COBOL program
pub fn generate_fortran_code_enhanced(program: &CobolProgram) -> String {
    let mut generator = FortranGenerator::new();
    generator.generate(program)
}
