//! Internal utilities

use itertools::FoldWhile::{Continue, Done};

use itertools::Itertools;

/// Map across the iterator, terminating early if a mapping returns None
pub fn map_while_some<A, E>(iter: &mut Iterator<Item = E>, f: &Fn(E) -> Option<A>) -> Option<Vec<A>> {
    let mut results: Vec<A> = Vec::new();
    for x in iter {
        if let Some(result) = f(x) {
            results.push(result);
        } else {
            return None;
        }
    }
    return Some(results);
}

pub fn fold_while_some<A, E>(init_acc: A, iter: &mut Iterator<Item = E>, f: &Fn(A, E) -> Option<A>) -> Option<A> {
    iter.fold_while(Some(init_acc), |acc, x| match f(acc.unwrap(), x) {
        Some(value) => Continue(Some(value)),
        None => Done(None),
    })
}

/// Create a string with the specified number of tabs
pub fn concat_tabs(ntabs: usize) -> String {
    let tabv: Vec<String> = (0..ntabs).map(|_| "\t".to_string()).collect();
    tabv.join("")
}
