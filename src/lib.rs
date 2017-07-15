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
#[cfg(feature = "with-constraint")]
#[macro_use]
pub mod constraints;
#[macro_use]
pub mod core;
#[cfg(feature = "with-datum")]
#[macro_use]
pub mod datum;
#[cfg(feature = "with-infer")]
pub mod infer;
#[cfg(feature = "with-pedigree")]
pub mod pedigree;
#[cfg(feature = "with-planner")]
pub mod planner;
#[cfg(feature = "with-rule")]
pub mod rule;
