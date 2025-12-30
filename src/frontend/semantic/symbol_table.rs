use crate::utils::tmp_var_counter::TMP_VAR_COUNT;
use std::{collections::HashMap, sync::atomic::Ordering};

/// Generates a unique var name adding a suffix
fn add_unique_suffix(name: &str) -> String {
    let id = TMP_VAR_COUNT.fetch_add(1, Ordering::Relaxed);
    format!("{}.{}", name, id)
}
#[derive(Clone, Debug)]
pub struct SymbolTable {
    scopes: Vec<HashMap<String, SymbolInfo>>, // Each index is a scope level
}

#[derive(Clone, Debug)]
pub struct SymbolInfo {
    /// Internal unique identifier for the symbol
    pub id: String,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![HashMap::new()], // Start with global scope
        }
    }

    pub fn insert(&mut self, name: impl Into<String>) {
        let name = name.into();
        let current_scope = self.scopes.last_mut().expect("No scope available");
        current_scope.insert(
            name.clone(),
            SymbolInfo {
                id: add_unique_suffix(&name),
            },
        );
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<&SymbolInfo> {
        let name = name.as_ref();
        // Search from current scope down to global scope
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }

    pub fn contains(&self, name: impl AsRef<str>) -> bool {
        self.get(name).is_some()
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    pub fn is_in_current_scope(&self, name: impl AsRef<str>) -> bool {
        self.scopes
            .last()
            .map(|scope| scope.contains_key(name.as_ref()))
            .unwrap_or(false)
    }
}
