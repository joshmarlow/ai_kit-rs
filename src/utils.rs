use std;

use itertools::Itertools;
use itertools::FoldWhile::{Continue, Done};
use permutohedron;

macro_rules! assert_some_value {
($x:expr, $y:expr) => (match $x {
    Some(val) => assert_eq!(val, $y),
    None => panic!("Expected value but received 'None'"),
    })
}

macro_rules! assert_none {
($x:expr) => (match $x {
    None => (),
    Some(val) => panic!("Expected 'None' received {}", val),
    })
}

macro_rules! from_json {
    ($type: ty, $json: tt) => ({
        use serde_json;
        let x: $type = serde_json::from_value(json!($json)).expect("Expected json decoding");
        x
    })
}

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
