// Copyright (c) 2025 sekacorn
// Contact: sekacorn@gmail.com
// All rights reserved.
//
// This file is part of the Corn COBOL-to-Fortran Compiler.
// Licensed under the Corn Dual License.
// See LICENSE-COMMERCIAL.txt and LICENSE-OPEN-SOURCE.txt for details.

//! SORT and MERGE Statement Handler
//! Critical for banking batch processing operations
//! Handles COBOL SORT and MERGE statements with INPUT/OUTPUT PROCEDURE

use std::collections::HashMap;

/// Represents a SORT statement
#[derive(Debug, Clone)]
pub struct SortStatement {
    pub sort_file: String,
    pub sort_keys: Vec<SortKey>,
    pub input_source: SortInput,
    pub output_target: SortOutput,
    pub collating_sequence: Option<String>,
    pub duplicates: DuplicateHandling,
}

/// Sort key definition
#[derive(Debug, Clone)]
pub struct SortKey {
    pub field_name: String,
    pub direction: SortDirection,
    pub data_type: KeyDataType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SortDirection {
    Ascending,
    Descending,
}

#[derive(Debug, Clone)]
pub enum KeyDataType {
    Alphanumeric,
    Numeric,
    SignedNumeric,
}

#[derive(Debug, Clone)]
pub enum SortInput {
    Using(Vec<String>),              // USING file-name
    InputProcedure(String, String),  // INPUT PROCEDURE section-name THRU section-name
}

#[derive(Debug, Clone)]
pub enum SortOutput {
    Giving(Vec<String>),             // GIVING file-name
    OutputProcedure(String, String), // OUTPUT PROCEDURE section-name THRU section-name
}

#[derive(Debug, Clone, PartialEq)]
pub enum DuplicateHandling {
    WithDuplicates,
    WithoutDuplicates,
}

/// Represents a MERGE statement
#[derive(Debug, Clone)]
pub struct MergeStatement {
    pub merge_file: String,
    pub merge_keys: Vec<SortKey>,
    pub using_files: Vec<String>,
    pub output_target: SortOutput,
    pub collating_sequence: Option<String>,
}

/// SORT/MERGE statement translator
pub struct SortMergeTranslator {
    temp_file_counter: usize,
}

impl SortMergeTranslator {
    pub fn new() -> Self {
        SortMergeTranslator {
            temp_file_counter: 0,
        }
    }

    /// Parse COBOL SORT statement
    /// Format: SORT sort-file ON ASCENDING/DESCENDING KEY field-name
    ///         USING file-name / INPUT PROCEDURE procedure-name
    ///         GIVING file-name / OUTPUT PROCEDURE procedure-name
    pub fn parse_sort(&mut self, stmt: &str) -> Result<SortStatement, String> {
        let tokens: Vec<&str> = stmt.split_whitespace().collect();

        if tokens.len() < 2 || tokens[0].to_uppercase() != "SORT" {
            return Err("Invalid SORT statement".to_string());
        }

        let sort_file = tokens[1].to_string();
        let mut sort_keys = Vec::new();
        let mut input_source = None;
        let mut output_target = None;
        let mut duplicates = DuplicateHandling::WithDuplicates;

        let mut i = 2;
        while i < tokens.len() {
            match tokens[i].to_uppercase().as_str() {
                "ON" => {
                    i += 1;
                    // Parse sort keys
                    while i < tokens.len() {
                        if tokens[i].to_uppercase() == "ASCENDING" || tokens[i].to_uppercase() == "DESCENDING" {
                            let direction = if tokens[i].to_uppercase() == "ASCENDING" {
                                SortDirection::Ascending
                            } else {
                                SortDirection::Descending
                            };

                            i += 1;
                            if i < tokens.len() && tokens[i].to_uppercase() == "KEY" {
                                i += 1;
                                if i < tokens.len() {
                                    sort_keys.push(SortKey {
                                        field_name: tokens[i].to_string(),
                                        direction,
                                        data_type: KeyDataType::Alphanumeric,
                                    });
                                    i += 1;
                                }
                            }
                        } else {
                            break;
                        }
                    }
                }
                "USING" => {
                    i += 1;
                    let mut files = Vec::new();
                    while i < tokens.len() && !["GIVING", "OUTPUT", "INPUT"].contains(&tokens[i].to_uppercase().as_str()) {
                        files.push(tokens[i].to_string());
                        i += 1;
                    }
                    input_source = Some(SortInput::Using(files));
                }
                "INPUT" => {
                    i += 1;
                    if i < tokens.len() && tokens[i].to_uppercase() == "PROCEDURE" {
                        i += 1;
                        let start_proc = tokens[i].to_string();
                        i += 1;
                        let end_proc = if i < tokens.len() && tokens[i].to_uppercase() == "THRU" {
                            i += 2;
                            tokens[i - 1].to_string()
                        } else {
                            start_proc.clone()
                        };
                        input_source = Some(SortInput::InputProcedure(start_proc, end_proc));
                    }
                }
                "GIVING" => {
                    i += 1;
                    let mut files = Vec::new();
                    while i < tokens.len() && !["OUTPUT", "INPUT"].contains(&tokens[i].to_uppercase().as_str()) {
                        files.push(tokens[i].to_string());
                        i += 1;
                    }
                    output_target = Some(SortOutput::Giving(files));
                }
                "OUTPUT" => {
                    i += 1;
                    if i < tokens.len() && tokens[i].to_uppercase() == "PROCEDURE" {
                        i += 1;
                        let start_proc = tokens[i].to_string();
                        i += 1;
                        let end_proc = if i < tokens.len() && tokens[i].to_uppercase() == "THRU" {
                            i += 2;
                            tokens[i - 1].to_string()
                        } else {
                            start_proc.clone()
                        };
                        output_target = Some(SortOutput::OutputProcedure(start_proc, end_proc));
                    }
                }
                "WITH" => {
                    i += 1;
                    if i < tokens.len() && tokens[i].to_uppercase() == "DUPLICATES" {
                        duplicates = DuplicateHandling::WithDuplicates;
                        i += 1;
                    }
                }
                _ => {
                    i += 1;
                }
            }
        }

        Ok(SortStatement {
            sort_file,
            sort_keys,
            input_source: input_source.ok_or("Missing INPUT specification")?,
            output_target: output_target.ok_or("Missing OUTPUT specification")?,
            collating_sequence: None,
            duplicates,
        })
    }

    /// Generate Fortran code for SORT statement
    pub fn generate_sort_fortran(&mut self, sort_stmt: &SortStatement) -> String {
        let mut output = String::new();

        output.push_str("  ! COBOL SORT statement translation\n");
        output.push_str("  ! Sort implementation using Fortran intrinsic sorting\n");

        // Declare sort work file
        let sort_file = sort_stmt.sort_file.replace("-", "_").to_uppercase();
        output.push_str(&format!("  ! Sort file: {}\n", sort_file));

        // Generate code based on input source
        match &sort_stmt.input_source {
            SortInput::Using(files) => {
                for file in files {
                    let file_clean = file.replace("-", "_").to_uppercase();
                    output.push_str(&format!("  ! Reading from input file: {}\n", file_clean));
                    output.push_str(&format!("  OPEN(UNIT={}, FILE='{}', STATUS='OLD', ACTION='READ')\n",
                        self.get_unit_number(), file_clean));
                }
            }
            SortInput::InputProcedure(start, end) => {
                output.push_str(&format!("  ! INPUT PROCEDURE: {} THRU {}\n", start, end));
                output.push_str(&format!("  CALL {}\n", start.replace("-", "_").to_uppercase()));
            }
        }

        // Generate sort logic
        output.push_str("  ! Sorting logic\n");
        output.push_str("  ! Sort keys:\n");
        for (idx, key) in sort_stmt.sort_keys.iter().enumerate() {
            let direction = match key.direction {
                SortDirection::Ascending => "ASCENDING",
                SortDirection::Descending => "DESCENDING",
            };
            output.push_str(&format!("  !   Key {}: {} ({})\n",
                idx + 1,
                key.field_name.replace("-", "_").to_uppercase(),
                direction));
        }

        // Fortran sorting implementation
        output.push_str("  ! Note: Implement actual sort using Fortran sorting routines\n");
        output.push_str("  ! For production, use ORDERPACK library or custom quicksort\n");

        // Generate code based on output target
        match &sort_stmt.output_target {
            SortOutput::Giving(files) => {
                for file in files {
                    let file_clean = file.replace("-", "_").to_uppercase();
                    output.push_str(&format!("  ! Writing to output file: {}\n", file_clean));
                    output.push_str(&format!("  OPEN(UNIT={}, FILE='{}', STATUS='REPLACE', ACTION='WRITE')\n",
                        self.get_unit_number(), file_clean));
                }
            }
            SortOutput::OutputProcedure(start, end) => {
                output.push_str(&format!("  ! OUTPUT PROCEDURE: {} THRU {}\n", start, end));
                output.push_str(&format!("  CALL {}\n", start.replace("-", "_").to_uppercase()));
            }
        }

        output.push_str("  ! End of SORT statement\n\n");
        output
    }

    /// Parse COBOL MERGE statement
    pub fn parse_merge(&mut self, stmt: &str) -> Result<MergeStatement, String> {
        let tokens: Vec<&str> = stmt.split_whitespace().collect();

        if tokens.len() < 2 || tokens[0].to_uppercase() != "MERGE" {
            return Err("Invalid MERGE statement".to_string());
        }

        let merge_file = tokens[1].to_string();
        let mut merge_keys = Vec::new();
        let mut using_files = Vec::new();
        let mut output_target = None;

        let mut i = 2;
        while i < tokens.len() {
            match tokens[i].to_uppercase().as_str() {
                "ON" => {
                    i += 1;
                    while i < tokens.len() {
                        if tokens[i].to_uppercase() == "ASCENDING" || tokens[i].to_uppercase() == "DESCENDING" {
                            let direction = if tokens[i].to_uppercase() == "ASCENDING" {
                                SortDirection::Ascending
                            } else {
                                SortDirection::Descending
                            };

                            i += 1;
                            if i < tokens.len() && tokens[i].to_uppercase() == "KEY" {
                                i += 1;
                                if i < tokens.len() {
                                    merge_keys.push(SortKey {
                                        field_name: tokens[i].to_string(),
                                        direction,
                                        data_type: KeyDataType::Alphanumeric,
                                    });
                                    i += 1;
                                }
                            }
                        } else {
                            break;
                        }
                    }
                }
                "USING" => {
                    i += 1;
                    while i < tokens.len() && !["GIVING", "OUTPUT"].contains(&tokens[i].to_uppercase().as_str()) {
                        using_files.push(tokens[i].to_string());
                        i += 1;
                    }
                }
                "GIVING" => {
                    i += 1;
                    let mut files = Vec::new();
                    while i < tokens.len() && tokens[i].to_uppercase() != "OUTPUT" {
                        files.push(tokens[i].to_string());
                        i += 1;
                    }
                    output_target = Some(SortOutput::Giving(files));
                }
                "OUTPUT" => {
                    i += 1;
                    if i < tokens.len() && tokens[i].to_uppercase() == "PROCEDURE" {
                        i += 1;
                        let start_proc = tokens[i].to_string();
                        i += 1;
                        let end_proc = if i < tokens.len() && tokens[i].to_uppercase() == "THRU" {
                            i += 2;
                            tokens[i - 1].to_string()
                        } else {
                            start_proc.clone()
                        };
                        output_target = Some(SortOutput::OutputProcedure(start_proc, end_proc));
                    }
                }
                _ => {
                    i += 1;
                }
            }
        }

        if using_files.len() < 2 {
            return Err("MERGE requires at least 2 USING files".to_string());
        }

        Ok(MergeStatement {
            merge_file,
            merge_keys,
            using_files,
            output_target: output_target.ok_or("Missing OUTPUT specification")?,
            collating_sequence: None,
        })
    }

    /// Generate Fortran code for MERGE statement
    pub fn generate_merge_fortran(&mut self, merge_stmt: &MergeStatement) -> String {
        let mut output = String::new();

        output.push_str("  ! COBOL MERGE statement translation\n");
        output.push_str("  ! Merge implementation using Fortran merge algorithms\n");

        let merge_file = merge_stmt.merge_file.replace("-", "_").to_uppercase();
        output.push_str(&format!("  ! Merge file: {}\n", merge_file));

        // List input files
        output.push_str("  ! Input files for merge:\n");
        for file in &merge_stmt.using_files {
            output.push_str(&format!("  !   - {}\n", file.replace("-", "_").to_uppercase()));
        }

        // Generate merge logic
        output.push_str("  ! Merge keys:\n");
        for (idx, key) in merge_stmt.merge_keys.iter().enumerate() {
            let direction = match key.direction {
                SortDirection::Ascending => "ASCENDING",
                SortDirection::Descending => "DESCENDING",
            };
            output.push_str(&format!("  !   Key {}: {} ({})\n",
                idx + 1,
                key.field_name.replace("-", "_").to_uppercase(),
                direction));
        }

        output.push_str("  ! Implementing multi-way merge algorithm\n");
        output.push_str("  ! Note: Use priority queue or k-way merge for production\n");

        // Generate output
        match &merge_stmt.output_target {
            SortOutput::Giving(files) => {
                for file in files {
                    output.push_str(&format!("  ! Output to: {}\n",
                        file.replace("-", "_").to_uppercase()));
                }
            }
            SortOutput::OutputProcedure(start, end) => {
                output.push_str(&format!("  ! OUTPUT PROCEDURE: {} THRU {}\n", start, end));
                output.push_str(&format!("  CALL {}\n", start.replace("-", "_").to_uppercase()));
            }
        }

        output.push_str("  ! End of MERGE statement\n\n");
        output
    }

    fn get_unit_number(&mut self) -> usize {
        self.temp_file_counter += 1;
        20 + self.temp_file_counter
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_sort_using_giving() {
        let mut translator = SortMergeTranslator::new();
        let stmt = "SORT SORT-FILE ON ASCENDING KEY EMPLOYEE-ID USING INPUT-FILE GIVING OUTPUT-FILE";
        let result = translator.parse_sort(stmt);
        assert!(result.is_ok());

        let sort_stmt = result.unwrap();
        assert_eq!(sort_stmt.sort_file, "SORT-FILE");
        assert_eq!(sort_stmt.sort_keys.len(), 1);
        assert_eq!(sort_stmt.sort_keys[0].field_name, "EMPLOYEE-ID");
    }

    #[test]
    fn test_parse_merge() {
        let mut translator = SortMergeTranslator::new();
        let stmt = "MERGE MERGE-FILE ON ASCENDING KEY ACCOUNT-NUMBER USING FILE1 FILE2 GIVING OUTPUT-FILE";
        let result = translator.parse_merge(stmt);
        assert!(result.is_ok());

        let merge_stmt = result.unwrap();
        assert_eq!(merge_stmt.merge_file, "MERGE-FILE");
        assert_eq!(merge_stmt.using_files.len(), 2);
    }

    #[test]
    fn test_generate_sort_fortran() {
        let mut translator = SortMergeTranslator::new();
        let sort_stmt = SortStatement {
            sort_file: "SORT-FILE".to_string(),
            sort_keys: vec![
                SortKey {
                    field_name: "EMPLOYEE-ID".to_string(),
                    direction: SortDirection::Ascending,
                    data_type: KeyDataType::Numeric,
                }
            ],
            input_source: SortInput::Using(vec!["INPUT-FILE".to_string()]),
            output_target: SortOutput::Giving(vec!["OUTPUT-FILE".to_string()]),
            collating_sequence: None,
            duplicates: DuplicateHandling::WithDuplicates,
        };

        let fortran = translator.generate_sort_fortran(&sort_stmt);
        assert!(fortran.contains("SORT"));
        assert!(fortran.contains("EMPLOYEE_ID"));
    }
}
