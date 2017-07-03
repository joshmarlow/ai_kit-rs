use std;

use itertools::Itertools;
use itertools::FoldWhile::{Continue, Done};
use permutohedron;
use sexp::{Atom, Sexp};

use core;

/// Map across the iterator, terminating early if a mapping returns None
pub fn filter_map_all<A, E>(iter: &mut Iterator<Item = E>, f: &Fn(E) -> Option<A>) -> Option<Vec<A>> {
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

pub fn fold_while_ok<A, I, E: std::fmt::Debug>(init_acc: A, iter: &mut Iterator<Item = I>, f: &Fn(A, I) -> Result<A, E>) -> Result<A, E> {
    iter.fold_while(Ok(init_acc), |acc, x| match f(acc.unwrap(), x) {
        Ok(value) => Continue(Ok(value)),
        Err(err) => Done(Err(err)),
    })
}

pub fn to_sexp_helper(head: &str, rest: Sexp) -> Sexp {
    /// A helper function for creating s-expressions of the form (head (<REST>))
    Sexp::List(vec![Sexp::Atom(Atom::S(head.to_string())), rest])
}

pub fn from_sexp_helper<A>(expected_head: &str,
                           s_exp: &Sexp,
                           expected_arg_count: usize,
                           f: &Fn(&Vec<Sexp>) -> std::result::Result<A, core::FromSexpError>)
                           -> std::result::Result<A, core::FromSexpError> {
    /// A helper function for parsing s-expressions of the form (head <REST>)
    fn err<A>(msg: String) -> std::result::Result<A, core::FromSexpError> {
        Err(core::FromSexpError { message: msg })
    }
    match *s_exp {
        Sexp::List(ref elements) if elements.len() == 2 => {
            match (&elements[0], &elements[1]) {
                (&Sexp::Atom(Atom::S(ref head)), &Sexp::List(ref args)) if head.as_str() == expected_head && args.len() == expected_arg_count => {
                    f(args)
                }
                (&Sexp::Atom(ref head), &Sexp::List(ref args)) => {
                    err(format!("from_sexp_helper:: Expected head: '{}' with {} args, found '{}' with args {} ({:?})",
                                expected_head,
                                expected_arg_count,
                                head,
                                args.len(),
                                args))
                }
                (&Sexp::Atom(_), &Sexp::Atom(_)) => {
                    err(format!("from_sexp_helper: Expected head {}, expected (atom list), but received (atom atom)",
                                expected_head))
                }
                (&Sexp::List(_), &Sexp::Atom(_)) => {
                    err(format!("from_sexp_helper: Expected head {}, expected (atom list), but received (list atom)",
                                expected_head))
                }
                (&Sexp::List(_), &Sexp::List(_)) => {
                    err(format!("from_sexp_helper: Expected head {}, expected (atom list), but received (list list)",
                                expected_head))
                }
            }
        }
        _ => {
            err(format!("from_sexp_helper: Expected format '({} (ARG_0 .. ARG_{}))' but found '{}'",
                        expected_head,
                        expected_arg_count,
                        s_exp))
        }
    }
}

/*
* Return a vector containing the values in `data` specifed by `indexes`.
*/
pub fn multi_index<T: Clone>(data: &Vec<T>, indexes: &Vec<usize>) -> Vec<T> {
    let mut values: Vec<T> = Vec::with_capacity(indexes.len());
    for idx in indexes {
        match data.get(*idx) {
            Some(value) => values.push(value.clone()),
            None => {
                panic!(format!("multi_index called with index: {} on vector of len {}",
                               idx,
                               data.len()))
            }
        }
    }
    values
}

pub fn permutations<T: Clone>(data: Vec<T>, n: usize) -> Vec<Vec<T>> {
    let mut permutations: Vec<Vec<T>> = Vec::new();

    for combination in data.into_iter().combinations(n) {
        permutohedron::heap_recursive(combination.clone().as_mut_slice(),
                                      |permutation| { permutations.push(permutation.to_vec()); });
    }

    permutations
}

pub fn display_graph(rendering: String, base_filename: String) {
    use std::fs::File;
    use std::io::prelude::*;
    use std::path::Path;
    use std::process::Command;

    let dot_filepath = format!("{}.dot", base_filename);
    let png_filepath = format!("{}.png", dot_filepath);
    File::create(&Path::new(&dot_filepath))
        .and_then(|mut file| file.write_all(rendering.as_str().as_bytes()))
        .and_then(|_| {
            Command::new("dot")
                .arg("-Tpng")
                .arg(dot_filepath.as_str())
                .arg("-O")
                .output()
                .or_else(|err| panic!("{:?}", err))
                .and_then(|_| {
                    Command::new("open")
                        .arg(png_filepath.as_str())
                        .output()
                        .or_else(|err| panic!("{:?}", err))
                })
                .and_then(|_| Ok(()))
        })
        .unwrap()
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_multi_index() {
        let v = vec![1, 2, 3, 4];
        assert_eq!(super::multi_index(&v, &vec![0, 2]), vec![v[0], v[2]]);
    }

    #[test]
    fn test_permutations() {
        let v = vec![1, 2, 3];

        let mut actual_permutations = super::permutations(v, 2);
        let mut expected_permutations = vec![vec![1, 2], vec![1, 3], vec![2, 3], vec![2, 1], vec![3, 1], vec![3, 2]];
        actual_permutations.sort();
        expected_permutations.sort();
        assert_eq!(actual_permutations, expected_permutations);
    }
}
