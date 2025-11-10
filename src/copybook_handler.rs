// Copyright (c) 2025 sekacorn
// Contact: sekacorn@gmail.com
// All rights reserved.
//
// This file is part of the Corn COBOL-to-Fortran Compiler.
// Licensed under the Corn Dual License.
// See LICENSE-COMMERCIAL.txt and LICENSE-OPEN-SOURCE.txt for details.

//! Copybook Handler Module
//! Handles COBOL COPY statements and copybook inclusion
//! Supports REPLACING clause and nested copybooks

use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

/// Copybook handler for processing COPY statements
pub struct CopybookHandler {
    copybook_paths: Vec<PathBuf>,
    loaded_copybooks: HashMap<String, String>,
    replacement_cache: HashMap<String, String>,
}

impl CopybookHandler {
    pub fn new() -> Self {
        CopybookHandler {
            copybook_paths: vec![PathBuf::from("./copybooks"), PathBuf::from(".")],
            loaded_copybooks: HashMap::new(),
            replacement_cache: HashMap::new(),
        }
    }

    /// Add a copybook search path
    pub fn add_copybook_path(&mut self, path: PathBuf) {
        if !self.copybook_paths.contains(&path) {
            self.copybook_paths.push(path);
        }
    }

    /// Process COPY statement
    /// Format: COPY copybook-name [REPLACING ==text1== BY ==text2==].
    pub fn process_copy_statement(&mut self, copy_stmt: &str) -> Result<String, String> {
        let stmt = copy_stmt.trim()
            .trim_start_matches("COPY")
            .trim_end_matches('.')
            .trim();

        // Parse copybook name and REPLACING clause
        let (copybook_name, replacing_clauses) = self.parse_copy_statement(stmt)?;

        // Load copybook content
        let content = self.load_copybook(&copybook_name)?;

        // Apply REPLACING clause if present
        let processed_content = if !replacing_clauses.is_empty() {
            self.apply_replacing(&content, &replacing_clauses)
        } else {
            content
        };

        Ok(processed_content)
    }

    /// Parse COPY statement to extract copybook name and REPLACING clauses
    fn parse_copy_statement(&self, stmt: &str) -> Result<(String, Vec<(String, String)>), String> {
        let parts: Vec<&str> = stmt.split("REPLACING").collect();

        let copybook_name = parts[0].trim().to_string();
        let mut replacing_clauses = Vec::new();

        if parts.len() > 1 {
            let replacing_part = parts[1];
            replacing_clauses = self.parse_replacing_clause(replacing_part)?;
        }

        Ok((copybook_name, replacing_clauses))
    }

    /// Parse REPLACING clause
    /// Format: ==text1== BY ==text2== [==text3== BY ==text4==]
    fn parse_replacing_clause(&self, replacing: &str) -> Result<Vec<(String, String)>, String> {
        let mut clauses = Vec::new();
        let parts: Vec<&str> = replacing.split("==").collect();

        let mut i = 1; // Skip first empty part
        while i < parts.len() {
            let from_text = parts[i].trim();
            i += 1;

            // Skip "BY"
            if i < parts.len() {
                let by_part = parts[i].trim();
                if !by_part.to_uppercase().contains("BY") {
                    return Err("Invalid REPLACING clause: expected BY".to_string());
                }
                i += 1;
            }

            // Get replacement text
            if i < parts.len() {
                let to_text = parts[i].trim();
                clauses.push((from_text.to_string(), to_text.to_string()));
                i += 1;
            }
        }

        Ok(clauses)
    }

    /// Load copybook from file
    fn load_copybook(&mut self, name: &str) -> Result<String, String> {
        // Check cache first
        if let Some(content) = self.loaded_copybooks.get(name) {
            return Ok(content.clone());
        }

        // Try to find copybook in search paths
        for path in &self.copybook_paths {
            let copybook_path = path.join(name);

            // Try with various extensions
            for ext in &["", ".cpy", ".CPY", ".cob", ".COB"] {
                let full_path = if ext.is_empty() {
                    copybook_path.clone()
                } else {
                    PathBuf::from(format!("{}{}", copybook_path.display(), ext))
                };

                if full_path.exists() {
                    match fs::read_to_string(&full_path) {
                        Ok(content) => {
                            self.loaded_copybooks.insert(name.to_string(), content.clone());
                            return Ok(content);
                        }
                        Err(e) => {
                            return Err(format!("Error reading copybook {}: {}", name, e));
                        }
                    }
                }
            }
        }

        Err(format!("Copybook not found: {}", name))
    }

    /// Apply REPLACING clause to copybook content
    fn apply_replacing(&self, content: &str, replacements: &[(String, String)]) -> String {
        let mut result = content.to_string();

        for (from, to) in replacements {
            result = result.replace(from, to);
        }

        result
    }

    /// Process all COPY statements in COBOL source
    pub fn process_all_copy_statements(&mut self, source: &str) -> Result<String, String> {
        let mut result = String::new();
        let lines: Vec<&str> = source.lines().collect();
        let mut i = 0;

        while i < lines.len() {
            let line = lines[i].trim();

            // Check if this is a COPY statement
            if line.starts_with("COPY") || line.trim_start().starts_with("COPY") {
                // Collect full COPY statement (might span multiple lines)
                let mut copy_stmt = line.to_string();
                let mut j = i + 1;

                while !copy_stmt.trim_end().ends_with('.') && j < lines.len() {
                    copy_stmt.push_str(" ");
                    copy_stmt.push_str(lines[j].trim());
                    j += 1;
                }

                // Process the COPY statement
                match self.process_copy_statement(&copy_stmt) {
                    Ok(content) => {
                        result.push_str(&content);
                        result.push('\n');
                    }
                    Err(e) => {
                        return Err(format!("Error processing COPY statement '{}': {}", copy_stmt, e));
                    }
                }

                i = j;
            } else {
                result.push_str(line);
                result.push('\n');
                i += 1;
            }
        }

        Ok(result)
    }

    /// Clear loaded copybooks cache
    pub fn clear_cache(&mut self) {
        self.loaded_copybooks.clear();
    }

    /// Get loaded copybooks count
    pub fn get_loaded_count(&self) -> usize {
        self.loaded_copybooks.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_copy_statement() {
        let handler = CopybookHandler::new();
        let (name, replacements) = handler
            .parse_copy_statement("EMPLOYEE-RECORD REPLACING ==WS== BY ==LS==")
            .unwrap();

        assert_eq!(name, "EMPLOYEE-RECORD");
        assert_eq!(replacements.len(), 1);
        assert_eq!(replacements[0].0, "WS");
        assert_eq!(replacements[0].1, "LS");
    }

    #[test]
    fn test_apply_replacing() {
        let handler = CopybookHandler::new();
        let content = "01 WS-EMPLOYEE-ID PIC 9(6).";
        let replacements = vec![("WS".to_string(), "LS".to_string())];
        let result = handler.apply_replacing(content, &replacements);

        assert_eq!(result, "01 LS-EMPLOYEE-ID PIC 9(6).");
    }

    #[test]
    fn test_multiple_replacements() {
        let handler = CopybookHandler::new();
        let content = "01 WS-EMP-ID PIC 9(6). 01 WS-EMP-NAME PIC X(30).";
        let replacements = vec![
            ("WS".to_string(), "LS".to_string()),
            ("EMP".to_string(), "EMPLOYEE".to_string()),
        ];
        let result = handler.apply_replacing(content, &replacements);

        assert!(result.contains("LS-EMPLOYEE-ID"));
        assert!(result.contains("LS-EMPLOYEE-NAME"));
    }
}
