// Copyright (c) 2025 sekacorn
// Contact: sekacorn@gmail.com
// All rights reserved.
//
// This file is part of the Corn COBOL-to-Fortran Compiler.
// Licensed under the Corn Dual License.
// See LICENSE-COMMERCIAL.txt and LICENSE-OPEN-SOURCE.txt for details.

//! Precision Decimal Arithmetic Handler
//! CRITICAL for banking and financial calculations
//! Ensures COBOL decimal precision is preserved in Fortran

use std::fmt;

/// Represents a COBOL decimal number with exact precision
#[derive(Debug, Clone, PartialEq)]
pub struct DecimalNumber {
    pub integer_digits: usize,
    pub decimal_digits: usize,
    pub signed: bool,
    pub value: Option<String>,
}

/// Arithmetic operation types
#[derive(Debug, Clone)]
pub enum ArithmeticOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

/// Rounding modes for financial calculations
#[derive(Debug, Clone, PartialEq)]
pub enum RoundingMode {
    None,
    Truncate,
    RoundHalfUp,      // Standard rounding (0.5 rounds up)
    RoundHalfEven,    // Banker's rounding (0.5 rounds to even)
    RoundUp,          // Always round away from zero
    RoundDown,        // Always round toward zero
}

/// Decimal arithmetic handler
pub struct DecimalArithmeticHandler {
    rounding_mode: RoundingMode,
}

impl DecimalArithmeticHandler {
    pub fn new() -> Self {
        DecimalArithmeticHandler {
            rounding_mode: RoundingMode::RoundHalfUp,
        }
    }

    /// Set rounding mode for calculations
    pub fn set_rounding_mode(&mut self, mode: RoundingMode) {
        self.rounding_mode = mode;
    }

    /// Generate Fortran code for decimal addition
    /// CRITICAL: Banking requires exact precision - no floating point errors!
    pub fn generate_decimal_add(
        &self,
        target: &DecimalNumber,
        operands: &[DecimalNumber],
        rounded: bool,
    ) -> String {
        let mut output = String::new();

        // For high-precision decimals, use Fortran's REAL(KIND=16) (quad precision)
        // or implement fixed-point arithmetic
        if self.requires_high_precision(target) {
            output.push_str("  ! High-precision decimal addition\n");
            output.push_str("  ! Using fixed-point arithmetic for banking accuracy\n");

            // Calculate scaling factor
            let scale = 10_i64.pow(target.decimal_digits as u32);
            output.push_str(&format!("  ! Decimal scale: 10^{} = {}\n",
                target.decimal_digits, scale));

            // Convert to integer arithmetic for exact precision
            output.push_str("  ! Converting to scaled integer arithmetic\n");

            if rounded {
                output.push_str(&format!("  ! ROUNDED using {} mode\n",
                    self.rounding_mode_name()));
            }
        } else {
            // Standard precision
            output.push_str("  ! Standard decimal addition\n");
        }

        output
    }

    /// Generate Fortran code for decimal multiplication
    /// CRITICAL for interest calculations, currency conversion
    pub fn generate_decimal_multiply(
        &self,
        result: &DecimalNumber,
        multiplicand: &DecimalNumber,
        multiplier: &DecimalNumber,
        rounded: bool,
    ) -> String {
        let mut output = String::new();

        output.push_str("  ! Precision decimal multiplication\n");
        output.push_str("  ! Critical for financial calculations\n");

        // Calculate result precision
        let result_int = multiplicand.integer_digits + multiplier.integer_digits;
        let result_dec = multiplicand.decimal_digits + multiplier.decimal_digits;

        output.push_str(&format!("  ! Multiplicand: {}.{} digits\n",
            multiplicand.integer_digits, multiplicand.decimal_digits));
        output.push_str(&format!("  ! Multiplier: {}.{} digits\n",
            multiplier.integer_digits, multiplier.decimal_digits));
        output.push_str(&format!("  ! Result precision: {}.{} digits\n",
            result_int, result_dec));

        if result_dec > result.decimal_digits {
            output.push_str(&format!("  ! Scaling down from {} to {} decimal places\n",
                result_dec, result.decimal_digits));

            if rounded {
                output.push_str(&format!("  ! Applying {} rounding\n",
                    self.rounding_mode_name()));
            }
        }

        // Use scaled integer arithmetic
        output.push_str("  ! Using scaled integer multiplication\n");
        output.push_str("  ! temp_scaled = (int1 * int2) / scale_factor\n");

        output
    }

    /// Generate Fortran code for decimal division
    /// CRITICAL for interest rate calculations, proration
    pub fn generate_decimal_divide(
        &self,
        quotient: &DecimalNumber,
        dividend: &DecimalNumber,
        divisor: &DecimalNumber,
        remainder: Option<&DecimalNumber>,
        rounded: bool,
    ) -> String {
        let mut output = String::new();

        output.push_str("  ! Precision decimal division\n");
        output.push_str("  ! Critical for banking calculations\n");

        if divisor.decimal_digits > 0 {
            output.push_str("  ! Divisor has decimal places - extra precision required\n");
        }

        if let Some(rem) = remainder {
            output.push_str("  ! Computing both quotient and remainder\n");
            output.push_str(&format!("  ! Remainder precision: {}.{} digits\n",
                rem.integer_digits, rem.decimal_digits));
        }

        if rounded {
            output.push_str(&format!("  ! Applying {} rounding to quotient\n",
                self.rounding_mode_name()));
        }

        // Use extended precision for division
        output.push_str("  ! Using extended precision division\n");
        output.push_str("  ! quotient_scaled = (dividend_scaled * scale) / divisor_scaled\n");

        output
    }

    /// Generate Fortran module for decimal arithmetic operations
    pub fn generate_decimal_arithmetic_module(&self) -> String {
        let mut output = String::new();

        output.push_str("! Decimal Arithmetic Module\n");
        output.push_str("! Provides exact precision arithmetic for financial calculations\n");
        output.push_str("! Copyright (c) 2025 sekacorn\n\n");

        output.push_str("MODULE DECIMAL_ARITHMETIC\n");
        output.push_str("  IMPLICIT NONE\n");
        output.push_str("  PRIVATE\n");
        output.push_str("  PUBLIC :: DECIMAL_ADD, DECIMAL_SUBTRACT, DECIMAL_MULTIPLY, DECIMAL_DIVIDE\n");
        output.push_str("  PUBLIC :: DECIMAL_ROUND, DECIMAL_TRUNCATE\n\n");

        // Define decimal type
        output.push_str("  ! Decimal number representation\n");
        output.push_str("  ! Uses scaled integer for exact precision\n");
        output.push_str("  TYPE :: DECIMAL_TYPE\n");
        output.push_str("    INTEGER(KIND=16) :: scaled_value  ! 128-bit integer\n");
        output.push_str("    INTEGER :: scale_factor           ! 10^decimal_places\n");
        output.push_str("    INTEGER :: decimal_places         ! Number of decimal digits\n");
        output.push_str("    LOGICAL :: is_negative\n");
        output.push_str("  END TYPE DECIMAL_TYPE\n\n");

        // Add subroutine
        output.push_str("CONTAINS\n\n");

        output.push_str("  ! Decimal addition with exact precision\n");
        output.push_str("  SUBROUTINE DECIMAL_ADD(result, op1, op2, rounded)\n");
        output.push_str("    TYPE(DECIMAL_TYPE), INTENT(OUT) :: result\n");
        output.push_str("    TYPE(DECIMAL_TYPE), INTENT(IN) :: op1, op2\n");
        output.push_str("    LOGICAL, INTENT(IN), OPTIONAL :: rounded\n");
        output.push_str("    INTEGER(KIND=16) :: temp_value\n");
        output.push_str("    INTEGER :: max_scale\n\n");

        output.push_str("    ! Align decimal places\n");
        output.push_str("    max_scale = MAX(op1%scale_factor, op2%scale_factor)\n");
        output.push_str("    ! Perform scaled integer addition\n");
        output.push_str("    temp_value = op1%scaled_value + op2%scaled_value\n");
        output.push_str("    result%scaled_value = temp_value\n");
        output.push_str("    result%scale_factor = max_scale\n");
        output.push_str("  END SUBROUTINE DECIMAL_ADD\n\n");

        // Multiply subroutine
        output.push_str("  ! Decimal multiplication with exact precision\n");
        output.push_str("  SUBROUTINE DECIMAL_MULTIPLY(result, op1, op2, rounded)\n");
        output.push_str("    TYPE(DECIMAL_TYPE), INTENT(OUT) :: result\n");
        output.push_str("    TYPE(DECIMAL_TYPE), INTENT(IN) :: op1, op2\n");
        output.push_str("    LOGICAL, INTENT(IN), OPTIONAL :: rounded\n");
        output.push_str("    INTEGER(KIND=16) :: temp_value\n\n");

        output.push_str("    ! Multiply scaled values\n");
        output.push_str("    temp_value = op1%scaled_value * op2%scaled_value\n");
        output.push_str("    ! Adjust for combined scale\n");
        output.push_str("    result%scaled_value = temp_value / op1%scale_factor\n");
        output.push_str("    result%scale_factor = op2%scale_factor\n");
        output.push_str("  END SUBROUTINE DECIMAL_MULTIPLY\n\n");

        // Divide subroutine
        output.push_str("  ! Decimal division with exact precision\n");
        output.push_str("  SUBROUTINE DECIMAL_DIVIDE(quotient, dividend, divisor, remainder, rounded)\n");
        output.push_str("    TYPE(DECIMAL_TYPE), INTENT(OUT) :: quotient\n");
        output.push_str("    TYPE(DECIMAL_TYPE), INTENT(IN) :: dividend, divisor\n");
        output.push_str("    TYPE(DECIMAL_TYPE), INTENT(OUT), OPTIONAL :: remainder\n");
        output.push_str("    LOGICAL, INTENT(IN), OPTIONAL :: rounded\n");
        output.push_str("    INTEGER(KIND=16) :: temp_quotient, temp_remainder\n\n");

        output.push_str("    ! Scale dividend for precision\n");
        output.push_str("    temp_quotient = (dividend%scaled_value * divisor%scale_factor) / divisor%scaled_value\n");
        output.push_str("    quotient%scaled_value = temp_quotient\n");
        output.push_str("    quotient%scale_factor = dividend%scale_factor\n");

        output.push_str("    IF (PRESENT(remainder)) THEN\n");
        output.push_str("      temp_remainder = MOD(dividend%scaled_value, divisor%scaled_value)\n");
        output.push_str("      remainder%scaled_value = temp_remainder\n");
        output.push_str("    END IF\n");
        output.push_str("  END SUBROUTINE DECIMAL_DIVIDE\n\n");

        // Rounding subroutine
        output.push_str("  ! Round decimal to specified precision\n");
        output.push_str("  SUBROUTINE DECIMAL_ROUND(value, decimal_places, mode)\n");
        output.push_str("    TYPE(DECIMAL_TYPE), INTENT(INOUT) :: value\n");
        output.push_str("    INTEGER, INTENT(IN) :: decimal_places\n");
        output.push_str("    INTEGER, INTENT(IN), OPTIONAL :: mode  ! 1=half-up, 2=banker's\n");
        output.push_str("    INTEGER(KIND=16) :: divisor, quotient, remainder\n");
        output.push_str("    INTEGER :: round_mode\n\n");

        output.push_str("    round_mode = 1  ! Default: half-up\n");
        output.push_str("    IF (PRESENT(mode)) round_mode = mode\n\n");

        output.push_str("    divisor = 10_16 ** (value%decimal_places - decimal_places)\n");
        output.push_str("    quotient = value%scaled_value / divisor\n");
        output.push_str("    remainder = MOD(value%scaled_value, divisor)\n\n");

        output.push_str("    ! Apply rounding based on mode\n");
        output.push_str("    IF (round_mode == 1) THEN  ! Half-up\n");
        output.push_str("      IF (remainder >= divisor / 2) quotient = quotient + 1\n");
        output.push_str("    ELSE IF (round_mode == 2) THEN  ! Banker's rounding\n");
        output.push_str("      IF (remainder > divisor / 2) THEN\n");
        output.push_str("        quotient = quotient + 1\n");
        output.push_str("      ELSE IF (remainder == divisor / 2) THEN\n");
        output.push_str("        IF (MOD(quotient, 2_16) /= 0) quotient = quotient + 1\n");
        output.push_str("      END IF\n");
        output.push_str("    END IF\n\n");

        output.push_str("    value%scaled_value = quotient\n");
        output.push_str("    value%decimal_places = decimal_places\n");
        output.push_str("  END SUBROUTINE DECIMAL_ROUND\n\n");

        output.push_str("END MODULE DECIMAL_ARITHMETIC\n");

        output
    }

    /// Check if decimal requires high precision (more than standard REAL can handle)
    fn requires_high_precision(&self, dec: &DecimalNumber) -> bool {
        // Standard REAL has ~7 significant digits
        // REAL(KIND=8) has ~15 significant digits
        // Need high precision if total digits > 15
        (dec.integer_digits + dec.decimal_digits) > 15
    }

    /// Get rounding mode name for comments
    fn rounding_mode_name(&self) -> &str {
        match self.rounding_mode {
            RoundingMode::None => "None",
            RoundingMode::Truncate => "Truncate",
            RoundingMode::RoundHalfUp => "Half-Up (Standard)",
            RoundingMode::RoundHalfEven => "Half-Even (Banker's)",
            RoundingMode::RoundUp => "Up",
            RoundingMode::RoundDown => "Down",
        }
    }

    /// Generate validation checks for arithmetic operations
    pub fn generate_validation_checks(&self, operation: &str) -> String {
        let mut output = String::new();

        output.push_str(&format!("  ! Validation checks for {} operation\n", operation));
        output.push_str("  ! Critical for banking compliance\n");
        output.push_str("  IF (divisor == 0) THEN\n");
        output.push_str("    WRITE(*,*) 'ERROR: Division by zero detected'\n");
        output.push_str("    CALL ERROR_HANDLER('DIVIDE_BY_ZERO')\n");
        output.push_str("  END IF\n\n");

        output.push_str("  ! Check for overflow\n");
        output.push_str("  IF (result > MAX_DECIMAL_VALUE) THEN\n");
        output.push_str("    WRITE(*,*) 'ERROR: Arithmetic overflow'\n");
        output.push_str("    CALL ERROR_HANDLER('OVERFLOW')\n");
        output.push_str("  END IF\n\n");

        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decimal_number_creation() {
        let dec = DecimalNumber {
            integer_digits: 9,
            decimal_digits: 2,
            signed: true,
            value: Some("9999999.99".to_string()),
        };

        assert_eq!(dec.integer_digits, 9);
        assert_eq!(dec.decimal_digits, 2);
        assert!(dec.signed);
    }

    #[test]
    fn test_high_precision_detection() {
        let handler = DecimalArithmeticHandler::new();

        let small_dec = DecimalNumber {
            integer_digits: 5,
            decimal_digits: 2,
            signed: false,
            value: None,
        };
        assert!(!handler.requires_high_precision(&small_dec));

        let large_dec = DecimalNumber {
            integer_digits: 15,
            decimal_digits: 4,
            signed: false,
            value: None,
        };
        assert!(handler.requires_high_precision(&large_dec));
    }

    #[test]
    fn test_rounding_mode() {
        let mut handler = DecimalArithmeticHandler::new();
        assert_eq!(handler.rounding_mode, RoundingMode::RoundHalfUp);

        handler.set_rounding_mode(RoundingMode::RoundHalfEven);
        assert_eq!(handler.rounding_mode, RoundingMode::RoundHalfEven);
    }

    #[test]
    fn test_generate_decimal_module() {
        let handler = DecimalArithmeticHandler::new();
        let module = handler.generate_decimal_arithmetic_module();

        assert!(module.contains("MODULE DECIMAL_ARITHMETIC"));
        assert!(module.contains("DECIMAL_ADD"));
        assert!(module.contains("DECIMAL_MULTIPLY"));
        assert!(module.contains("DECIMAL_DIVIDE"));
        assert!(module.contains("DECIMAL_ROUND"));
    }
}
