// Needed for tests
#![recursion_limit="128"]

extern crate itertools;
extern crate serde;
#[allow(unused_imports)]
#[macro_use]
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
extern crate uuid;

#[cfg(test)]
#[macro_use]
mod test_utils;
#[cfg(feature = "with-constraint")]
pub mod constraints;
#[macro_use]
pub mod core;
#[cfg(feature = "with-datum")]
pub mod datum;
#[cfg(feature = "with-forward-inference")]
pub mod infer;
#[cfg(feature = "with-pedigree")]
pub mod pedigree;
#[cfg(feature = "with-planner")]
pub mod planner;
#[cfg(feature = "with-rule")]
pub mod rule;
pub mod utils;
