mod utils;
mod semantics;

use std::any::Any;
use std::collections::{HashMap, VecDeque};
use crate::core::omnia_types::Type;
use crate::core::omnia_types::Type::{BOOL, BYTE, CHAR, DECIMAL, INT, LONG, NULL, OMNI, UBYTE, UINT, ULONG};

pub const TYPES_SIZES: &[(Type, u64); 11] = &[
    (BOOL, 1),
    (BYTE, 1),
    (UBYTE, 1),
    (CHAR, 2),
    (INT, 4),
    (UINT, 4),
    (LONG, 8),
    (ULONG, 8),
    (DECIMAL, 8),
    (NULL, 8),
    (OMNI, 16)
];

