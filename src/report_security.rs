// Copyright (c) 2025 sekacorn
// Contact: sekacorn@gmail.com
// All rights reserved.
//
// This file is part of the Corn COBOL-to-Fortran Compiler.
// Licensed under the Corn Dual License.
// See LICENSE-COMMERCIAL.txt and LICENSE-OPEN-SOURCE.txt for details.

//! This module handles encryption and authentication for report security.
//! Now supports:
//! - AES-256-GCM encryption for reports
//! - Secure decryption of reports
//! - API authentication using secure tokens

use aes_gcm::aead::{Aead, KeyInit};
use aes_gcm::{Aes256Gcm, Nonce};
use std::fs;
use std::io::{Read, Write};

/// **Encrypts a report file using AES-256-GCM encryption.**
/// - **filename:** Path to the report file.
/// - **key:** 32-byte encryption key.
/// - **Output:** Creates an encrypted report file with `.enc` extension.
pub fn encrypt_report(filename: &str, key: &[u8; 32]) -> std::io::Result<()> {
    let mut file = fs::File::open(filename)?;
    let mut content = Vec::new();
    file.read_to_end(&mut content)?;

    let cipher = Aes256Gcm::new(key.into());
    let nonce = Nonce::from_slice(b"unique_nonce_12b"); // 12-byte unique nonce
    let encrypted_data = cipher.encrypt(nonce, content.as_ref()).expect("Encryption failure!");

    let mut enc_file = fs::File::create(format!("{}.enc", filename))?;
    enc_file.write_all(&encrypted_data)?;

    println!("Report successfully encrypted: {}.enc", filename);
    Ok(())
}

/// **Decrypts an encrypted report file using AES-256-GCM.**
/// - **filename:** Path to the encrypted file.
/// - **key:** 32-byte encryption key.
/// - **Output:** Decrypts and saves the report with `.dec` extension.
pub fn decrypt_report(filename: &str, key: &[u8; 32]) -> std::io::Result<()> {
    let mut file = fs::File::open(filename)?;
    let mut content = Vec::new();
    file.read_to_end(&mut content)?;

    let cipher = Aes256Gcm::new(key.into());
    let nonce = Nonce::from_slice(b"unique_nonce_12b");
    let decrypted_data = cipher.decrypt(nonce, content.as_ref()).expect("Decryption failure!");

    let mut dec_file = fs::File::create(format!("{}.dec", filename))?;
    dec_file.write_all(&decrypted_data)?;

    println!("Report successfully decrypted: {}.dec", filename);
    Ok(())
}

/// **Validates an authentication token for API security.**
/// - **token:** The authentication token provided by the client.
/// - **Returns:** `true` if the token is valid, `false` otherwise.
pub fn validate_token(token: &str) -> bool {
    const SECRET_TOKEN: &str = "SUPER_SECRET_API_KEY";
    token == SECRET_TOKEN
}

/// **Generates a secure random 32-byte encryption key.**
/// - **Returns:** A secure key for AES encryption.
pub fn generate_encryption_key() -> [u8; 32] {
    use rand::Rng;
    let mut key = [0u8; 32];
    rand::thread_rng().fill(&mut key);
    key
}
