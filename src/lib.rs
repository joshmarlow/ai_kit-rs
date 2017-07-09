// Needed for tests
#![recursion_limit="128"]

extern crate itertools;
extern crate permutohedron;
extern crate serde;
#[macro_use]
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
extern crate uuid;

#[macro_use]
pub mod utils;
#[macro_use]
pub mod constraints;
#[macro_use]
pub mod core;
#[macro_use]
pub mod datum;
pub mod infer;
pub mod pedigree;
pub mod planner;
