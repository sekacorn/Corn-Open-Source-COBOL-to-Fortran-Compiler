// Copyright (c) 2025 sekacorn
// Contact: sekacorn@gmail.com
// All rights reserved.
//
// This file is part of the Corn COBOL-to-Fortran Compiler.
// Licensed under the Corn Dual License.
// See LICENSE-COMMERCIAL.txt and LICENSE-OPEN-SOURCE.txt for details.

//! Enterprise-Grade Error Handling and Audit Trail
//! CRITICAL for banking compliance and regulatory requirements
//! Provides comprehensive error detection, logging, and recovery

use std::collections::HashMap;
use std::fmt;

/// Error severity levels for banking applications
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ErrorSeverity {
    Info,           // Informational message
    Warning,        // Potential issue, but processing continues
    Error,          // Error occurred, but recoverable
    Critical,       // Critical error, immediate attention required
    Fatal,          // Fatal error, processing must stop
}

/// Error categories for banking operations
#[derive(Debug, Clone, PartialEq)]
pub enum ErrorCategory {
    DataValidation,      // Invalid data format or value
    ArithmeticError,     // Overflow, underflow, division by zero
    FileOperation,       // File I/O errors
    DatabaseOperation,   // SQL/database errors
    BusinessLogic,       // Business rule violations
    SecurityViolation,   // Security or authentication errors
    SystemResource,      // Memory, disk space, etc.
    ComplianceViolation, // Regulatory compliance issues
}

/// Detailed error information
#[derive(Debug, Clone)]
pub struct ErrorInfo {
    pub error_code: String,
    pub severity: ErrorSeverity,
    pub category: ErrorCategory,
    pub message: String,
    pub source_location: Option<String>,
    pub timestamp: String,
    pub user_id: Option<String>,
    pub transaction_id: Option<String>,
    pub recovery_action: Option<String>,
}

/// Audit trail entry
#[derive(Debug, Clone)]
pub struct AuditEntry {
    pub timestamp: String,
    pub user_id: String,
    pub transaction_id: String,
    pub operation: String,
    pub entity_type: String,
    pub entity_id: String,
    pub old_value: Option<String>,
    pub new_value: Option<String>,
    pub status: String,
    pub ip_address: Option<String>,
}

/// Enterprise error handler
pub struct EnterpriseErrorHandler {
    error_log: Vec<ErrorInfo>,
    audit_trail: Vec<AuditEntry>,
    error_counts: HashMap<ErrorSeverity, usize>,
    max_errors: HashMap<ErrorSeverity, usize>,
}

impl EnterpriseErrorHandler {
    pub fn new() -> Self {
        let mut max_errors = HashMap::new();
        max_errors.insert(ErrorSeverity::Fatal, 1);
        max_errors.insert(ErrorSeverity::Critical, 10);
        max_errors.insert(ErrorSeverity::Error, 100);
        max_errors.insert(ErrorSeverity::Warning, 1000);

        EnterpriseErrorHandler {
            error_log: Vec::new(),
            audit_trail: Vec::new(),
            error_counts: HashMap::new(),
            max_errors,
        }
    }

    /// Log an error with full details
    pub fn log_error(&mut self, error: ErrorInfo) -> Result<(), String> {
        let severity = error.severity.clone();

        // Update error count
        let count = self.error_counts.entry(severity.clone()).or_insert(0);
        *count += 1;

        // Check if max errors exceeded
        if let Some(max) = self.max_errors.get(&severity) {
            if count > max {
                return Err(format!(
                    "Maximum {} errors ({}) exceeded",
                    self.severity_name(&severity),
                    max
                ));
            }
        }

        self.error_log.push(error);
        Ok(())
    }

    /// Log audit trail entry
    pub fn log_audit(&mut self, entry: AuditEntry) {
        self.audit_trail.push(entry);
    }

    /// Generate Fortran error handling module
    pub fn generate_error_handling_module(&self) -> String {
        let mut output = String::new();

        output.push_str("! Enterprise Error Handling Module\n");
        output.push_str("! Provides comprehensive error detection and recovery\n");
        output.push_str("! Critical for banking and financial applications\n");
        output.push_str("! Copyright (c) 2025 sekacorn\n\n");

        output.push_str("MODULE ERROR_HANDLER\n");
        output.push_str("  IMPLICIT NONE\n");
        output.push_str("  PRIVATE\n");
        output.push_str("  PUBLIC :: INIT_ERROR_HANDLER, LOG_ERROR, LOG_AUDIT\n");
        output.push_str("  PUBLIC :: CHECK_ERROR_STATUS, GET_ERROR_COUNT\n");
        output.push_str("  PUBLIC :: HANDLE_ARITHMETIC_ERROR, HANDLE_FILE_ERROR\n");
        output.push_str("  PUBLIC :: HANDLE_DATABASE_ERROR, HANDLE_VALIDATION_ERROR\n\n");

        // Error severity constants
        output.push_str("  ! Error severity levels\n");
        output.push_str("  INTEGER, PARAMETER :: SEVERITY_INFO = 1\n");
        output.push_str("  INTEGER, PARAMETER :: SEVERITY_WARNING = 2\n");
        output.push_str("  INTEGER, PARAMETER :: SEVERITY_ERROR = 3\n");
        output.push_str("  INTEGER, PARAMETER :: SEVERITY_CRITICAL = 4\n");
        output.push_str("  INTEGER, PARAMETER :: SEVERITY_FATAL = 5\n\n");

        // Error category constants
        output.push_str("  ! Error categories\n");
        output.push_str("  INTEGER, PARAMETER :: CAT_DATA_VALIDATION = 1\n");
        output.push_str("  INTEGER, PARAMETER :: CAT_ARITHMETIC = 2\n");
        output.push_str("  INTEGER, PARAMETER :: CAT_FILE_IO = 3\n");
        output.push_str("  INTEGER, PARAMETER :: CAT_DATABASE = 4\n");
        output.push_str("  INTEGER, PARAMETER :: CAT_BUSINESS_LOGIC = 5\n");
        output.push_str("  INTEGER, PARAMETER :: CAT_SECURITY = 6\n");
        output.push_str("  INTEGER, PARAMETER :: CAT_COMPLIANCE = 7\n\n");

        // Error log structure
        output.push_str("  ! Error log variables\n");
        output.push_str("  INTEGER :: ERROR_COUNT_INFO = 0\n");
        output.push_str("  INTEGER :: ERROR_COUNT_WARNING = 0\n");
        output.push_str("  INTEGER :: ERROR_COUNT_ERROR = 0\n");
        output.push_str("  INTEGER :: ERROR_COUNT_CRITICAL = 0\n");
        output.push_str("  INTEGER :: ERROR_COUNT_FATAL = 0\n");
        output.push_str("  INTEGER :: ERROR_LOG_UNIT = 99\n");
        output.push_str("  INTEGER :: AUDIT_LOG_UNIT = 98\n");
        output.push_str("  LOGICAL :: LOGGING_ENABLED = .TRUE.\n\n");

        // Error type definition
        output.push_str("  TYPE :: ERROR_TYPE\n");
        output.push_str("    CHARACTER(LEN=20) :: error_code\n");
        output.push_str("    INTEGER :: severity\n");
        output.push_str("    INTEGER :: category\n");
        output.push_str("    CHARACTER(LEN=200) :: message\n");
        output.push_str("    CHARACTER(LEN=50) :: source_location\n");
        output.push_str("    CHARACTER(LEN=30) :: timestamp\n");
        output.push_str("  END TYPE ERROR_TYPE\n\n");

        output.push_str("CONTAINS\n\n");

        // Initialize error handler
        output.push_str("  SUBROUTINE INIT_ERROR_HANDLER(log_file)\n");
        output.push_str("    CHARACTER(LEN=*), INTENT(IN) :: log_file\n");
        output.push_str("    CHARACTER(LEN=100) :: audit_file\n");
        output.push_str("    CHARACTER(LEN=30) :: timestamp\n\n");

        output.push_str("    CALL GET_TIMESTAMP(timestamp)\n");
        output.push_str("    OPEN(UNIT=ERROR_LOG_UNIT, FILE=log_file, STATUS='REPLACE', &\n");
        output.push_str("         ACTION='WRITE', POSITION='APPEND')\n");
        output.push_str("    WRITE(ERROR_LOG_UNIT, '(A,A)') 'Error log initialized: ', timestamp\n\n");

        output.push_str("    audit_file = TRIM(log_file) // '.audit'\n");
        output.push_str("    OPEN(UNIT=AUDIT_LOG_UNIT, FILE=audit_file, STATUS='REPLACE', &\n");
        output.push_str("         ACTION='WRITE', POSITION='APPEND')\n");
        output.push_str("    WRITE(AUDIT_LOG_UNIT, '(A,A)') 'Audit trail initialized: ', timestamp\n");
        output.push_str("  END SUBROUTINE INIT_ERROR_HANDLER\n\n");

        // Log error subroutine
        output.push_str("  SUBROUTINE LOG_ERROR(error_code, severity, category, message, source_loc)\n");
        output.push_str("    CHARACTER(LEN=*), INTENT(IN) :: error_code\n");
        output.push_str("    INTEGER, INTENT(IN) :: severity, category\n");
        output.push_str("    CHARACTER(LEN=*), INTENT(IN) :: message\n");
        output.push_str("    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: source_loc\n");
        output.push_str("    CHARACTER(LEN=30) :: timestamp\n");
        output.push_str("    CHARACTER(LEN=20) :: severity_str, category_str\n\n");

        output.push_str("    IF (.NOT. LOGGING_ENABLED) RETURN\n\n");

        output.push_str("    CALL GET_TIMESTAMP(timestamp)\n");
        output.push_str("    CALL GET_SEVERITY_STRING(severity, severity_str)\n");
        output.push_str("    CALL GET_CATEGORY_STRING(category, category_str)\n\n");

        output.push_str("    ! Update error counters\n");
        output.push_str("    SELECT CASE (severity)\n");
        output.push_str("      CASE (SEVERITY_INFO)\n");
        output.push_str("        ERROR_COUNT_INFO = ERROR_COUNT_INFO + 1\n");
        output.push_str("      CASE (SEVERITY_WARNING)\n");
        output.push_str("        ERROR_COUNT_WARNING = ERROR_COUNT_WARNING + 1\n");
        output.push_str("      CASE (SEVERITY_ERROR)\n");
        output.push_str("        ERROR_COUNT_ERROR = ERROR_COUNT_ERROR + 1\n");
        output.push_str("      CASE (SEVERITY_CRITICAL)\n");
        output.push_str("        ERROR_COUNT_CRITICAL = ERROR_COUNT_CRITICAL + 1\n");
        output.push_str("      CASE (SEVERITY_FATAL)\n");
        output.push_str("        ERROR_COUNT_FATAL = ERROR_COUNT_FATAL + 1\n");
        output.push_str("    END SELECT\n\n");

        output.push_str("    ! Write to error log\n");
        output.push_str("    WRITE(ERROR_LOG_UNIT, '(A,1X,A,1X,A,1X,A,1X,A,1X,A)') &\n");
        output.push_str("      timestamp, error_code, TRIM(severity_str), TRIM(category_str), &\n");
        output.push_str("      TRIM(message), TRIM(source_loc)\n\n");

        output.push_str("    ! For critical/fatal errors, also write to stderr\n");
        output.push_str("    IF (severity >= SEVERITY_CRITICAL) THEN\n");
        output.push_str("      WRITE(*, '(A,A,A,A)') 'CRITICAL ERROR [', error_code, ']: ', message\n");
        output.push_str("    END IF\n\n");

        output.push_str("    ! Stop program on fatal error\n");
        output.push_str("    IF (severity == SEVERITY_FATAL) THEN\n");
        output.push_str("      WRITE(*, '(A)') 'FATAL ERROR - Program terminating'\n");
        output.push_str("      CALL CLOSE_ERROR_HANDLER()\n");
        output.push_str("      STOP 1\n");
        output.push_str("    END IF\n");
        output.push_str("  END SUBROUTINE LOG_ERROR\n\n");

        // Arithmetic error handler
        output.push_str("  SUBROUTINE HANDLE_ARITHMETIC_ERROR(error_type, location, value1, value2)\n");
        output.push_str("    CHARACTER(LEN=*), INTENT(IN) :: error_type, location\n");
        output.push_str("    REAL(KIND=8), INTENT(IN), OPTIONAL :: value1, value2\n");
        output.push_str("    CHARACTER(LEN=200) :: message\n\n");

        output.push_str("    SELECT CASE (TRIM(error_type))\n");
        output.push_str("      CASE ('DIVIDE_BY_ZERO')\n");
        output.push_str("        message = 'Division by zero detected'\n");
        output.push_str("        CALL LOG_ERROR('ARITH001', SEVERITY_CRITICAL, CAT_ARITHMETIC, &\n");
        output.push_str("                       message, location)\n");
        output.push_str("      CASE ('OVERFLOW')\n");
        output.push_str("        message = 'Arithmetic overflow detected'\n");
        output.push_str("        CALL LOG_ERROR('ARITH002', SEVERITY_ERROR, CAT_ARITHMETIC, &\n");
        output.push_str("                       message, location)\n");
        output.push_str("      CASE ('UNDERFLOW')\n");
        output.push_str("        message = 'Arithmetic underflow detected'\n");
        output.push_str("        CALL LOG_ERROR('ARITH003', SEVERITY_WARNING, CAT_ARITHMETIC, &\n");
        output.push_str("                       message, location)\n");
        output.push_str("      CASE ('SIZE_ERROR')\n");
        output.push_str("        message = 'COBOL ON SIZE ERROR condition detected'\n");
        output.push_str("        CALL LOG_ERROR('ARITH004', SEVERITY_ERROR, CAT_ARITHMETIC, &\n");
        output.push_str("                       message, location)\n");
        output.push_str("    END SELECT\n");
        output.push_str("  END SUBROUTINE HANDLE_ARITHMETIC_ERROR\n\n");

        // File error handler
        output.push_str("  SUBROUTINE HANDLE_FILE_ERROR(file_name, operation, iostat_value)\n");
        output.push_str("    CHARACTER(LEN=*), INTENT(IN) :: file_name, operation\n");
        output.push_str("    INTEGER, INTENT(IN) :: iostat_value\n");
        output.push_str("    CHARACTER(LEN=200) :: message\n\n");

        output.push_str("    WRITE(message, '(A,A,A,I0)') 'File operation failed: ', &\n");
        output.push_str("          TRIM(operation), ' on file ', TRIM(file_name)\n");
        output.push_str("    CALL LOG_ERROR('FILE001', SEVERITY_ERROR, CAT_FILE_IO, message, '')\n");
        output.push_str("  END SUBROUTINE HANDLE_FILE_ERROR\n\n");

        // Database error handler
        output.push_str("  SUBROUTINE HANDLE_DATABASE_ERROR(sql_code, sql_state, message)\n");
        output.push_str("    INTEGER, INTENT(IN) :: sql_code\n");
        output.push_str("    CHARACTER(LEN=*), INTENT(IN) :: sql_state, message\n");
        output.push_str("    CHARACTER(LEN=200) :: full_message\n\n");

        output.push_str("    WRITE(full_message, '(A,I0,A,A,A,A)') 'SQL Error ', sql_code, &\n");
        output.push_str("          ' (', TRIM(sql_state), '): ', TRIM(message)\n");
        output.push_str("    CALL LOG_ERROR('DB001', SEVERITY_ERROR, CAT_DATABASE, full_message, '')\n");
        output.push_str("  END SUBROUTINE HANDLE_DATABASE_ERROR\n\n");

        // Validation error handler
        output.push_str("  SUBROUTINE HANDLE_VALIDATION_ERROR(field_name, expected, actual)\n");
        output.push_str("    CHARACTER(LEN=*), INTENT(IN) :: field_name, expected, actual\n");
        output.push_str("    CHARACTER(LEN=200) :: message\n\n");

        output.push_str("    WRITE(message, '(A,A,A,A,A,A)') 'Validation failed for ', &\n");
        output.push_str("          TRIM(field_name), ': expected ', TRIM(expected), &\n");
        output.push_str("          ', got ', TRIM(actual)\n");
        output.push_str("    CALL LOG_ERROR('VAL001', SEVERITY_ERROR, CAT_DATA_VALIDATION, &\n");
        output.push_str("                   message, '')\n");
        output.push_str("  END SUBROUTINE HANDLE_VALIDATION_ERROR\n\n");

        // Audit log subroutine
        output.push_str("  SUBROUTINE LOG_AUDIT(user_id, transaction_id, operation, entity_type, &\n");
        output.push_str("                       entity_id, old_value, new_value, status)\n");
        output.push_str("    CHARACTER(LEN=*), INTENT(IN) :: user_id, transaction_id, operation\n");
        output.push_str("    CHARACTER(LEN=*), INTENT(IN) :: entity_type, entity_id\n");
        output.push_str("    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: old_value, new_value, status\n");
        output.push_str("    CHARACTER(LEN=30) :: timestamp\n");
        output.push_str("    CHARACTER(LEN=20) :: status_str\n\n");

        output.push_str("    CALL GET_TIMESTAMP(timestamp)\n");
        output.push_str("    status_str = 'SUCCESS'\n");
        output.push_str("    IF (PRESENT(status)) status_str = status\n\n");

        output.push_str("    ! Write audit trail entry\n");
        output.push_str("    WRITE(AUDIT_LOG_UNIT, '(A,1X,A,1X,A,1X,A,1X,A,1X,A,1X,A,1X,A,1X,A)') &\n");
        output.push_str("      timestamp, user_id, transaction_id, operation, entity_type, &\n");
        output.push_str("      entity_id, old_value, new_value, status_str\n");
        output.push_str("  END SUBROUTINE LOG_AUDIT\n\n");

        // Helper subroutines
        output.push_str("  SUBROUTINE GET_TIMESTAMP(timestamp)\n");
        output.push_str("    CHARACTER(LEN=30), INTENT(OUT) :: timestamp\n");
        output.push_str("    INTEGER :: values(8)\n");
        output.push_str("    CALL DATE_AND_TIME(VALUES=values)\n");
        output.push_str("    WRITE(timestamp, '(I4.4,A,I2.2,A,I2.2,1X,I2.2,A,I2.2,A,I2.2)') &\n");
        output.push_str("      values(1), '-', values(2), '-', values(3), &\n");
        output.push_str("      values(5), ':', values(6), ':', values(7)\n");
        output.push_str("  END SUBROUTINE GET_TIMESTAMP\n\n");

        output.push_str("  SUBROUTINE GET_SEVERITY_STRING(severity, str)\n");
        output.push_str("    INTEGER, INTENT(IN) :: severity\n");
        output.push_str("    CHARACTER(LEN=*), INTENT(OUT) :: str\n");
        output.push_str("    SELECT CASE (severity)\n");
        output.push_str("      CASE (SEVERITY_INFO); str = 'INFO'\n");
        output.push_str("      CASE (SEVERITY_WARNING); str = 'WARNING'\n");
        output.push_str("      CASE (SEVERITY_ERROR); str = 'ERROR'\n");
        output.push_str("      CASE (SEVERITY_CRITICAL); str = 'CRITICAL'\n");
        output.push_str("      CASE (SEVERITY_FATAL); str = 'FATAL'\n");
        output.push_str("      CASE DEFAULT; str = 'UNKNOWN'\n");
        output.push_str("    END SELECT\n");
        output.push_str("  END SUBROUTINE GET_SEVERITY_STRING\n\n");

        output.push_str("  SUBROUTINE GET_CATEGORY_STRING(category, str)\n");
        output.push_str("    INTEGER, INTENT(IN) :: category\n");
        output.push_str("    CHARACTER(LEN=*), INTENT(OUT) :: str\n");
        output.push_str("    SELECT CASE (category)\n");
        output.push_str("      CASE (CAT_DATA_VALIDATION); str = 'VALIDATION'\n");
        output.push_str("      CASE (CAT_ARITHMETIC); str = 'ARITHMETIC'\n");
        output.push_str("      CASE (CAT_FILE_IO); str = 'FILE_IO'\n");
        output.push_str("      CASE (CAT_DATABASE); str = 'DATABASE'\n");
        output.push_str("      CASE (CAT_BUSINESS_LOGIC); str = 'BUSINESS'\n");
        output.push_str("      CASE (CAT_SECURITY); str = 'SECURITY'\n");
        output.push_str("      CASE (CAT_COMPLIANCE); str = 'COMPLIANCE'\n");
        output.push_str("      CASE DEFAULT; str = 'UNKNOWN'\n");
        output.push_str("    END SELECT\n");
        output.push_str("  END SUBROUTINE GET_CATEGORY_STRING\n\n");

        output.push_str("  SUBROUTINE CLOSE_ERROR_HANDLER()\n");
        output.push_str("    IF (ERROR_LOG_UNIT > 0) CLOSE(ERROR_LOG_UNIT)\n");
        output.push_str("    IF (AUDIT_LOG_UNIT > 0) CLOSE(AUDIT_LOG_UNIT)\n");
        output.push_str("  END SUBROUTINE CLOSE_ERROR_HANDLER\n\n");

        output.push_str("  INTEGER FUNCTION GET_ERROR_COUNT(severity)\n");
        output.push_str("    INTEGER, INTENT(IN) :: severity\n");
        output.push_str("    SELECT CASE (severity)\n");
        output.push_str("      CASE (SEVERITY_INFO); GET_ERROR_COUNT = ERROR_COUNT_INFO\n");
        output.push_str("      CASE (SEVERITY_WARNING); GET_ERROR_COUNT = ERROR_COUNT_WARNING\n");
        output.push_str("      CASE (SEVERITY_ERROR); GET_ERROR_COUNT = ERROR_COUNT_ERROR\n");
        output.push_str("      CASE (SEVERITY_CRITICAL); GET_ERROR_COUNT = ERROR_COUNT_CRITICAL\n");
        output.push_str("      CASE (SEVERITY_FATAL); GET_ERROR_COUNT = ERROR_COUNT_FATAL\n");
        output.push_str("      CASE DEFAULT; GET_ERROR_COUNT = 0\n");
        output.push_str("    END SELECT\n");
        output.push_str("  END FUNCTION GET_ERROR_COUNT\n\n");

        output.push_str("END MODULE ERROR_HANDLER\n");

        output
    }

    fn severity_name(&self, severity: &ErrorSeverity) -> &str {
        match severity {
            ErrorSeverity::Info => "INFO",
            ErrorSeverity::Warning => "WARNING",
            ErrorSeverity::Error => "ERROR",
            ErrorSeverity::Critical => "CRITICAL",
            ErrorSeverity::Fatal => "FATAL",
        }
    }

    /// Get error summary report
    pub fn get_error_summary(&self) -> String {
        let mut summary = String::new();

        summary.push_str("Error Summary Report\n");
        summary.push_str("===================\n\n");

        for (severity, count) in &self.error_counts {
            summary.push_str(&format!("{}: {}\n", self.severity_name(severity), count));
        }

        summary.push_str(&format!("\nTotal errors logged: {}\n", self.error_log.len()));
        summary.push_str(&format!("Total audit entries: {}\n", self.audit_trail.len()));

        summary
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_handler_creation() {
        let handler = EnterpriseErrorHandler::new();
        assert_eq!(handler.error_log.len(), 0);
        assert_eq!(handler.audit_trail.len(), 0);
    }

    #[test]
    fn test_log_error() {
        let mut handler = EnterpriseErrorHandler::new();

        let error = ErrorInfo {
            error_code: "TEST001".to_string(),
            severity: ErrorSeverity::Warning,
            category: ErrorCategory::DataValidation,
            message: "Test error".to_string(),
            source_location: Some("test.rs:100".to_string()),
            timestamp: "2025-01-01 12:00:00".to_string(),
            user_id: Some("USER001".to_string()),
            transaction_id: Some("TXN001".to_string()),
            recovery_action: None,
        };

        let result = handler.log_error(error);
        assert!(result.is_ok());
        assert_eq!(handler.error_log.len(), 1);
    }

    #[test]
    fn test_generate_error_module() {
        let handler = EnterpriseErrorHandler::new();
        let module = handler.generate_error_handling_module();

        assert!(module.contains("MODULE ERROR_HANDLER"));
        assert!(module.contains("LOG_ERROR"));
        assert!(module.contains("LOG_AUDIT"));
        assert!(module.contains("HANDLE_ARITHMETIC_ERROR"));
    }
}
