//! The core module contains the core data structures and traits used by all other modules.

#[cfg(feature = "with-constraint")]
use constraints;

use serde::{Deserialize, Serialize};
use std::collections::{BTreeSet, HashMap};
use std::fmt::{Debug, Display, Formatter, Result};
use std::iter::{Extend, FromIterator};

/// A type must implement BindingsValue in order to be used as the value for some variable.
pub trait BindingsValue
    : Clone + Debug + Default + Deserialize + Display + Eq + PartialEq + PartialOrd + Serialize
    {
    /// Construct a BindingsValue variable using the specified string as it's name
    fn variable(_s: &String) -> Option<Self> {
        None
    }
    /// Extract the name of this BindingsValue variable (if it is a variable)
    fn to_variable(&self) -> Option<String> {
        None
    }
}

/// Bindings is used for storing variables and operating on variables and their bindings.
#[derive(Debug, Deserialize, Clone, Serialize)]
pub struct Bindings<T: BindingsValue> {
    #[serde(default)]
    data: HashMap<String, T>,
    #[serde(default)]
    equivalences: HashMap<String, BTreeSet<String>>,
}

impl<T: BindingsValue> PartialEq for Bindings<T> {
    fn eq(&self, other: &Bindings<T>) -> bool {
        self.data.eq(&other.data) && self.equivalences.eq(&other.equivalences)
    }
}
impl<T: BindingsValue> Eq for Bindings<T> {}

impl<T: BindingsValue> Default for Bindings<T> {
    fn default() -> Self {
        Bindings {
            data: HashMap::new(),
            equivalences: HashMap::new(),
        }
    }
}

impl<T: BindingsValue> Bindings<T> {
    pub fn new() -> Bindings<T> {
        Bindings::default()
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn has_binding(&self, variable: &String) -> bool {
        self.data.contains_key(variable)
    }

    pub fn set_binding(&self, variable: &String, val: T) -> Bindings<T> {
        let mut bindings_copy = self.clone();

        bindings_copy.set_binding_mut(variable, val);

        bindings_copy
    }

    fn set_binding_mut(&mut self, variable: &String, val: T) {
        self.ensure_equivalence_exists_mut(variable);

        if let Some(variable2) = val.to_variable() {
            self.add_equivalence(variable, &variable2);
        } else {
            for equivalent_variable in self.equivalences.get(variable).unwrap().iter() {
                self.data.insert(equivalent_variable.clone(), val.clone());
            }
        }
    }

    fn add_equivalence(&mut self, variable: &String, variable2: &String) {
        self.ensure_equivalence_exists_mut(&variable2);
        self.merge_equivalences_mut(variable, &variable2);
    }

    fn ensure_equivalence_exists_mut(&mut self, variable: &String) {
        if !self.equivalences.contains_key(variable) {
            self.equivalences.insert(variable.clone(),
                                     vec![variable.clone()].into_iter().collect());
        }
    }

    fn merge_equivalences_mut(&mut self, variable: &String, variable2: &String) {
        let mut merge = self.equivalences.get(variable).cloned().unwrap();
        merge.extend(self.equivalences.get(variable2).cloned().unwrap());

        self.equivalences.insert(variable.clone(), merge.clone());
        self.equivalences.insert(variable2.clone(), merge);
    }

    pub fn get_binding(&self, variable: &String) -> Option<T> {
        match self.data.get(variable) {
            Some(val) => Some(val.clone()),
            None => None,
        }
    }

    pub fn update_bindings(&self, variable: &String, value: &T) -> Option<Self> {
        // If we are setting a variable to itself, then do nothing
        if Some(variable.clone()) == value.to_variable() {
            return Some(self.clone());
        }
        match self.get_binding(&variable) {
            Some(ref val) if val == value => Some(self.clone()),
            Some(_) => None,
            None => Some(self.set_binding(variable, value.clone())),
        }
    }

    pub fn merge(&self, other: &Self) -> Self {
        let mut bindings = self.clone();

        // Merge in equivalences
        for (ref key, ref equivalences) in other.equivalences.iter() {
            for equivalent_key in equivalences.iter() {
                bindings.ensure_equivalence_exists_mut(key);
                bindings.add_equivalence(key, equivalent_key);
            }
        }

        // Merge in values
        for (key, value) in other.data.iter() {
            bindings.set_binding_mut(&key, value.clone())
        }

        bindings
    }

    pub fn equivalences_string(&self) -> String {
        let equivalent_v: Vec<String> = self.equivalences.iter().map(|(key, value)| format!("{} => {:?}", key, value)).collect();
        equivalent_v.join(",")
    }
}

impl<T: BindingsValue> FromIterator<(String, T)> for Bindings<T> {
    fn from_iter<I: IntoIterator<Item = (String, T)>>(iter: I) -> Bindings<T> {
        let mut bindings: Bindings<T> = Bindings::new();
        for (key, value) in iter {
            bindings.set_binding_mut(&key, value);
        }
        bindings
    }
}

impl<T: BindingsValue> Extend<(String, T)> for Bindings<T> {
    fn extend<It>(&mut self, iter: It)
        where It: IntoIterator<Item = (String, T)>
    {
        for (key, value) in iter {
            self.set_binding_mut(&key, value);
        }
    }
}

impl<T: BindingsValue> Display for Bindings<T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        try!(write!(f, "("));
        let mut sorted_keys: Vec<String> = self.data.keys().cloned().collect();
        sorted_keys.sort();
        for key in sorted_keys.into_iter() {
            let ref val = self.data[&key];
            try!(write!(f, "{} => {}, ", key, val));
        }
        try!(write!(f, "Equivalences: {}", self.equivalences_string()));
        write!(f, ")")
    }
}

/// A type must implement the Unify trait for it to be unifiable -  this only requirement for a data structure
/// that the algorithms in this library operate on.
pub trait Unify<T: BindingsValue>
    : Clone + Debug + Display + Eq + Serialize + Deserialize + PartialEq {
    /// Check if this structure can be unified with another of the same type.
    fn unify(&self, &Self, &Bindings<T>) -> Option<Bindings<T>>;

    /// Given some bindings, construct a new instance with any variables replaced by their values
    fn apply_bindings(&self, &Bindings<T>) -> Option<Self>;

    /// Return all variables in this structure
    fn variables(&self) -> Vec<String>;

    /// Rename any variables in this structure with another variable name
    fn rename_variables(&self, &HashMap<String, String>) -> Self;

    /// Return a 'nil' sentinel value unique to this type of structure
    fn nil() -> Self;
}

/// A type that implements Operation constructs new Unifys from existing Unifys that match it's input patterns.
pub trait Operation<T: BindingsValue, U: Unify<T>>
    : Clone + Debug + Display + Eq + PartialEq + Deserialize + Serialize {
    // NOTE: replace constraints with validate_bindings?
    #[cfg(feature = "with-constraint")]
    fn constraints<'a>(&'a self) -> Vec<&'a constraints::Constraint>;

    /// Creates a new instance of this Operation where all variables are unique
    fn snowflake(&self, String) -> Self;

    /// Return a vector of input patterns that must be unified with in order to apply this Operation.
    fn input_patterns(&self) -> Vec<U> {
        Vec::new()
    }

    /// Given some bindings, construct a set of output patterns
    fn apply_match(&self, _bindings: &Bindings<T>) -> Option<Vec<U>>;

    /// Given some bindings, construct a set of input patterns that would match
    fn r_apply_match(&self, _fact: &U) -> Option<(Vec<U>, Bindings<T>)>;
}

#[cfg(test)]
mod tests;
