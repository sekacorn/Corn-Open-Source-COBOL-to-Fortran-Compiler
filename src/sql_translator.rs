// Copyright (c) 2025 sekacorn
// Contact: sekacorn@gmail.com
// All rights reserved.
//
// This file is part of the Corn COBOL-to-Fortran Compiler.
// Licensed under the Corn Dual License.
// See LICENSE-COMMERCIAL.txt and LICENSE-OPEN-SOURCE.txt for details.

//! SQL Translation Module
//! Translates COBOL EXEC SQL statements to Fortran SQL equivalents
//! Supports embedded SQL, cursors, and database operations

use std::collections::HashMap;

/// Represents a parsed SQL statement
#[derive(Debug, Clone)]
pub enum SQLStatement {
    Select {
        columns: Vec<String>,
        into_vars: Vec<String>,
        from_table: String,
        where_clause: Option<String>,
    },
    Insert {
        table: String,
        columns: Vec<String>,
        values: Vec<String>,
    },
    Update {
        table: String,
        set_clauses: Vec<(String, String)>,
        where_clause: Option<String>,
    },
    Delete {
        from_table: String,
        where_clause: Option<String>,
    },
    DeclareCursor {
        cursor_name: String,
        select_stmt: Box<SQLStatement>,
    },
    OpenCursor {
        cursor_name: String,
    },
    FetchCursor {
        cursor_name: String,
        into_vars: Vec<String>,
    },
    CloseCursor {
        cursor_name: String,
    },
    Commit,
    Rollback,
}

/// SQL Translator
pub struct SQLTranslator {
    host_variables: HashMap<String, String>,
    cursors: HashMap<String, String>,
}

impl SQLTranslator {
    pub fn new() -> Self {
        SQLTranslator {
            host_variables: HashMap::new(),
            cursors: HashMap::new(),
        }
    }

    /// Parse EXEC SQL block
    pub fn parse_exec_sql(&mut self, sql_text: &str) -> Result<SQLStatement, String> {
        let sql = sql_text.trim()
            .trim_start_matches("EXEC SQL")
            .trim_end_matches("END-EXEC")
            .trim();

        let tokens: Vec<&str> = sql.split_whitespace().collect();

        if tokens.is_empty() {
            return Err("Empty SQL statement".to_string());
        }

        match tokens[0].to_uppercase().as_str() {
            "SELECT" => self.parse_select(&tokens),
            "INSERT" => self.parse_insert(&tokens),
            "UPDATE" => self.parse_update(&tokens),
            "DELETE" => self.parse_delete(&tokens),
            "DECLARE" => self.parse_declare_cursor(&tokens),
            "OPEN" => self.parse_open_cursor(&tokens),
            "FETCH" => self.parse_fetch_cursor(&tokens),
            "CLOSE" => self.parse_close_cursor(&tokens),
            "COMMIT" => Ok(SQLStatement::Commit),
            "ROLLBACK" => Ok(SQLStatement::Rollback),
            _ => Err(format!("Unsupported SQL statement: {}", tokens[0])),
        }
    }

    fn parse_select(&self, tokens: &[&str]) -> Result<SQLStatement, String> {
        let mut columns = Vec::new();
        let mut into_vars = Vec::new();
        let mut from_table = String::new();
        let mut where_clause = None;

        let mut i = 1; // Skip "SELECT"

        // Parse column list
        while i < tokens.len() && tokens[i].to_uppercase() != "INTO" {
            let col = tokens[i].trim_end_matches(',');
            columns.push(col.to_string());
            i += 1;
        }

        // Parse INTO clause
        if i < tokens.len() && tokens[i].to_uppercase() == "INTO" {
            i += 1;
            while i < tokens.len() && tokens[i].to_uppercase() != "FROM" {
                let var = tokens[i].trim_start_matches(':').trim_end_matches(',');
                into_vars.push(var.to_string());
                i += 1;
            }
        }

        // Parse FROM clause
        if i < tokens.len() && tokens[i].to_uppercase() == "FROM" {
            i += 1;
            if i < tokens.len() {
                from_table = tokens[i].to_string();
                i += 1;
            }
        }

        // Parse WHERE clause
        if i < tokens.len() && tokens[i].to_uppercase() == "WHERE" {
            i += 1;
            let where_parts: Vec<String> = tokens[i..].iter().map(|s| s.to_string()).collect();
            where_clause = Some(where_parts.join(" "));
        }

        Ok(SQLStatement::Select {
            columns,
            into_vars,
            from_table,
            where_clause,
        })
    }

    fn parse_insert(&self, tokens: &[&str]) -> Result<SQLStatement, String> {
        let mut table = String::new();
        let mut columns = Vec::new();
        let mut values = Vec::new();

        let mut i = 1; // Skip "INSERT"

        if i < tokens.len() && tokens[i].to_uppercase() == "INTO" {
            i += 1;
            if i < tokens.len() {
                table = tokens[i].to_string();
                i += 1;
            }
        }

        // Parse column list in parentheses
        if i < tokens.len() {
            let col_part = tokens[i..].iter()
                .take_while(|s| !s.to_uppercase().contains("VALUES"))
                .collect::<Vec<_>>();

            for token in col_part {
                let clean = token.trim_matches('(').trim_matches(')').trim_end_matches(',');
                if !clean.is_empty() {
                    columns.push(clean.to_string());
                }
            }
        }

        // Parse VALUES clause
        let values_idx = tokens.iter().position(|s| s.to_uppercase() == "VALUES");
        if let Some(idx) = values_idx {
            let val_tokens = &tokens[idx + 1..];
            for token in val_tokens {
                let clean = token.trim_matches('(').trim_matches(')').trim_end_matches(',')
                    .trim_start_matches(':');
                if !clean.is_empty() {
                    values.push(clean.to_string());
                }
            }
        }

        Ok(SQLStatement::Insert {
            table,
            columns,
            values,
        })
    }

    fn parse_update(&self, tokens: &[&str]) -> Result<SQLStatement, String> {
        let mut table = String::new();
        let mut set_clauses = Vec::new();
        let mut where_clause = None;

        let mut i = 1; // Skip "UPDATE"

        if i < tokens.len() {
            table = tokens[i].to_string();
            i += 1;
        }

        // Parse SET clause
        if i < tokens.len() && tokens[i].to_uppercase() == "SET" {
            i += 1;
            while i < tokens.len() && tokens[i].to_uppercase() != "WHERE" {
                if let Some(eq_pos) = tokens[i..].iter().position(|s| s.contains('=')) {
                    let col = tokens[i + eq_pos].split('=').next().unwrap_or("").to_string();
                    let val = tokens[i + eq_pos].split('=').nth(1)
                        .or_else(|| tokens.get(i + eq_pos + 1).map(|s| *s))
                        .unwrap_or("")
                        .trim_start_matches(':')
                        .trim_end_matches(',')
                        .to_string();
                    set_clauses.push((col, val));
                    i += eq_pos + 2;
                } else {
                    i += 1;
                }
            }
        }

        // Parse WHERE clause
        if i < tokens.len() && tokens[i].to_uppercase() == "WHERE" {
            i += 1;
            let where_parts: Vec<String> = tokens[i..].iter().map(|s| s.to_string()).collect();
            where_clause = Some(where_parts.join(" "));
        }

        Ok(SQLStatement::Update {
            table,
            set_clauses,
            where_clause,
        })
    }

    fn parse_delete(&self, tokens: &[&str]) -> Result<SQLStatement, String> {
        let mut from_table = String::new();
        let mut where_clause = None;

        let mut i = 1; // Skip "DELETE"

        if i < tokens.len() && tokens[i].to_uppercase() == "FROM" {
            i += 1;
            if i < tokens.len() {
                from_table = tokens[i].to_string();
                i += 1;
            }
        }

        // Parse WHERE clause
        if i < tokens.len() && tokens[i].to_uppercase() == "WHERE" {
            i += 1;
            let where_parts: Vec<String> = tokens[i..].iter().map(|s| s.to_string()).collect();
            where_clause = Some(where_parts.join(" "));
        }

        Ok(SQLStatement::Delete {
            from_table,
            where_clause,
        })
    }

    fn parse_declare_cursor(&mut self, tokens: &[&str]) -> Result<SQLStatement, String> {
        if tokens.len() < 4 {
            return Err("Invalid DECLARE CURSOR syntax".to_string());
        }

        let cursor_name = tokens[1].to_string();

        // Parse the SELECT statement
        let select_start = tokens.iter().position(|s| s.to_uppercase() == "SELECT")
            .ok_or("DECLARE CURSOR requires SELECT statement")?;

        let select_stmt = self.parse_select(&tokens[select_start..])?;

        self.cursors.insert(cursor_name.clone(), "DECLARED".to_string());

        Ok(SQLStatement::DeclareCursor {
            cursor_name,
            select_stmt: Box::new(select_stmt),
        })
    }

    fn parse_open_cursor(&self, tokens: &[&str]) -> Result<SQLStatement, String> {
        if tokens.len() < 2 {
            return Err("Invalid OPEN CURSOR syntax".to_string());
        }

        Ok(SQLStatement::OpenCursor {
            cursor_name: tokens[1].to_string(),
        })
    }

    fn parse_fetch_cursor(&self, tokens: &[&str]) -> Result<SQLStatement, String> {
        let mut cursor_name = String::new();
        let mut into_vars = Vec::new();

        let mut i = 1; // Skip "FETCH"

        if i < tokens.len() {
            cursor_name = tokens[i].to_string();
            i += 1;
        }

        // Parse INTO clause
        if i < tokens.len() && tokens[i].to_uppercase() == "INTO" {
            i += 1;
            while i < tokens.len() {
                let var = tokens[i].trim_start_matches(':').trim_end_matches(',');
                into_vars.push(var.to_string());
                i += 1;
            }
        }

        Ok(SQLStatement::FetchCursor {
            cursor_name,
            into_vars,
        })
    }

    fn parse_close_cursor(&self, tokens: &[&str]) -> Result<SQLStatement, String> {
        if tokens.len() < 2 {
            return Err("Invalid CLOSE CURSOR syntax".to_string());
        }

        Ok(SQLStatement::CloseCursor {
            cursor_name: tokens[1].to_string(),
        })
    }

    /// Generate Fortran SQL code
    pub fn generate_fortran_sql(&self, stmt: &SQLStatement) -> String {
        match stmt {
            SQLStatement::Select { columns, into_vars, from_table, where_clause } => {
                let mut output = String::new();
                output.push_str("  ! EXEC SQL SELECT\n");
                output.push_str(&format!("  EXEC SQL SELECT {} INTO ", columns.join(", ")));

                let vars: Vec<String> = into_vars.iter()
                    .map(|v| format!(":{}", v.replace("-", "_").to_uppercase()))
                    .collect();
                output.push_str(&vars.join(", "));

                output.push_str(&format!(" FROM {}", from_table));

                if let Some(where_part) = where_clause {
                    output.push_str(&format!(" WHERE {}", where_part));
                }

                output.push_str(" END-EXEC\n");
                output.push_str("  IF (SQLCODE /= 0) THEN\n");
                output.push_str("    WRITE(*,*) 'SQL Error: ', SQLCODE\n");
                output.push_str("  END IF\n");
                output
            }

            SQLStatement::Insert { table, columns, values } => {
                let mut output = String::new();
                output.push_str("  ! EXEC SQL INSERT\n");
                output.push_str(&format!("  EXEC SQL INSERT INTO {} (", table));
                output.push_str(&columns.join(", "));
                output.push_str(") VALUES (");

                let vals: Vec<String> = values.iter()
                    .map(|v| {
                        if v.starts_with(':') {
                            format!(":{}", v.trim_start_matches(':').replace("-", "_").to_uppercase())
                        } else {
                            format!(":{}", v.replace("-", "_").to_uppercase())
                        }
                    })
                    .collect();
                output.push_str(&vals.join(", "));
                output.push_str(") END-EXEC\n");
                output.push_str("  IF (SQLCODE /= 0) THEN\n");
                output.push_str("    WRITE(*,*) 'SQL Error: ', SQLCODE\n");
                output.push_str("  END IF\n");
                output
            }

            SQLStatement::Update { table, set_clauses, where_clause } => {
                let mut output = String::new();
                output.push_str("  ! EXEC SQL UPDATE\n");
                output.push_str(&format!("  EXEC SQL UPDATE {} SET ", table));

                let sets: Vec<String> = set_clauses.iter()
                    .map(|(col, val)| {
                        let val_clean = if val.starts_with(':') {
                            format!(":{}", val.trim_start_matches(':').replace("-", "_").to_uppercase())
                        } else {
                            format!(":{}", val.replace("-", "_").to_uppercase())
                        };
                        format!("{} = {}", col, val_clean)
                    })
                    .collect();
                output.push_str(&sets.join(", "));

                if let Some(where_part) = where_clause {
                    output.push_str(&format!(" WHERE {}", where_part));
                }

                output.push_str(" END-EXEC\n");
                output.push_str("  IF (SQLCODE /= 0) THEN\n");
                output.push_str("    WRITE(*,*) 'SQL Error: ', SQLCODE\n");
                output.push_str("  END IF\n");
                output
            }

            SQLStatement::Delete { from_table, where_clause } => {
                let mut output = String::new();
                output.push_str("  ! EXEC SQL DELETE\n");
                output.push_str(&format!("  EXEC SQL DELETE FROM {}", from_table));

                if let Some(where_part) = where_clause {
                    output.push_str(&format!(" WHERE {}", where_part));
                }

                output.push_str(" END-EXEC\n");
                output.push_str("  IF (SQLCODE /= 0) THEN\n");
                output.push_str("    WRITE(*,*) 'SQL Error: ', SQLCODE\n");
                output.push_str("  END IF\n");
                output
            }

            SQLStatement::DeclareCursor { cursor_name, select_stmt } => {
                let mut output = String::new();
                output.push_str("  ! EXEC SQL DECLARE CURSOR\n");
                output.push_str(&format!("  EXEC SQL DECLARE {} CURSOR FOR\n", cursor_name));

                if let SQLStatement::Select { columns, into_vars: _, from_table, where_clause } = select_stmt.as_ref() {
                    output.push_str(&format!("    SELECT {} FROM {}", columns.join(", "), from_table));
                    if let Some(where_part) = where_clause {
                        output.push_str(&format!(" WHERE {}", where_part));
                    }
                }

                output.push_str(" END-EXEC\n");
                output
            }

            SQLStatement::OpenCursor { cursor_name } => {
                format!("  EXEC SQL OPEN {} END-EXEC\n", cursor_name)
            }

            SQLStatement::FetchCursor { cursor_name, into_vars } => {
                let mut output = String::new();
                output.push_str(&format!("  EXEC SQL FETCH {} INTO ", cursor_name));

                let vars: Vec<String> = into_vars.iter()
                    .map(|v| format!(":{}", v.replace("-", "_").to_uppercase()))
                    .collect();
                output.push_str(&vars.join(", "));

                output.push_str(" END-EXEC\n");
                output.push_str("  IF (SQLCODE /= 0 .AND. SQLCODE /= 100) THEN\n");
                output.push_str("    WRITE(*,*) 'SQL Error: ', SQLCODE\n");
                output.push_str("  END IF\n");
                output
            }

            SQLStatement::CloseCursor { cursor_name } => {
                format!("  EXEC SQL CLOSE {} END-EXEC\n", cursor_name)
            }

            SQLStatement::Commit => {
                "  EXEC SQL COMMIT END-EXEC\n".to_string()
            }

            SQLStatement::Rollback => {
                "  EXEC SQL ROLLBACK END-EXEC\n".to_string()
            }
        }
    }

    /// Register host variable for SQL translation
    pub fn register_host_variable(&mut self, cobol_name: String, fortran_name: String) {
        self.host_variables.insert(cobol_name, fortran_name);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_select() {
        let mut translator = SQLTranslator::new();
        let sql = "SELECT EMP_ID, EMP_NAME INTO :ws-emp-id, :ws-emp-name FROM EMPLOYEES WHERE EMP_ID = 123";
        let result = translator.parse_exec_sql(&format!("EXEC SQL {} END-EXEC", sql));
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_insert() {
        let mut translator = SQLTranslator::new();
        let sql = "INSERT INTO EMPLOYEES (EMP_ID, EMP_NAME) VALUES (:ws-emp-id, :ws-emp-name)";
        let result = translator.parse_exec_sql(&format!("EXEC SQL {} END-EXEC", sql));
        assert!(result.is_ok());
    }

    #[test]
    fn test_generate_fortran_select() {
        let translator = SQLTranslator::new();
        let stmt = SQLStatement::Select {
            columns: vec!["EMP_ID".to_string(), "EMP_NAME".to_string()],
            into_vars: vec!["ws-emp-id".to_string(), "ws-emp-name".to_string()],
            from_table: "EMPLOYEES".to_string(),
            where_clause: Some("EMP_ID = 123".to_string()),
        };
        let fortran = translator.generate_fortran_sql(&stmt);
        assert!(fortran.contains("EXEC SQL SELECT"));
        assert!(fortran.contains("SQLCODE"));
    }
}
