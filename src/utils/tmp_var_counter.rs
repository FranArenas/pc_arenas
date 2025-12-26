use std::sync::atomic::AtomicUsize;

/// Global atomic counter for generating unique variable names
pub static TMP_VAR_COUNT: AtomicUsize = AtomicUsize::new(0);
