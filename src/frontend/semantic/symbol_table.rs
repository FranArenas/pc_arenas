use crate::utils::tmp_var_counter::TMP_VAR_COUNT;
use std::{collections::HashMap, sync::atomic::Ordering};

/// Generates a unique var name adding a suffix
fn add_unique_suffix(name: &str) -> String {
    let id = TMP_VAR_COUNT.fetch_add(1, Ordering::Relaxed);
    format!("{}.{}", name, id)
}

pub struct SymbolTable {
    table: HashMap<String, SymbolInfo>, // The string key is the user defined name
}

pub struct SymbolInfo {
    /// Internal unique identifier for the symbol
    pub id: String,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            table: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: impl Into<String>) {
        let name = name.into();
        self.table.insert(
            name.clone(),
            SymbolInfo {
                id: add_unique_suffix(&name),
            },
        );
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<&SymbolInfo> {
        self.table.get(name.as_ref())
    }

    pub fn contains(&self, name: impl AsRef<str>) -> bool {
        self.table.contains_key(name.as_ref())
    }
}
