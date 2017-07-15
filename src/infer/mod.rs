use std::collections::{BTreeMap, BTreeSet};
use std::marker::PhantomData;

use constraints::ConstraintValue;
use core::{Apply, Bindings, Unify};
use pedigree::{Origin, Pedigree, RenderType};
use utils;

#[derive(Clone, Debug, PartialEq)]
pub struct OriginCache {
    items: BTreeSet<Origin>,
}

impl OriginCache {
    pub fn new() -> Self {
        OriginCache { items: BTreeSet::new() }
    }

    pub fn has_item(&self, item: &Origin) -> bool {
        self.items.contains(item)
    }

    pub fn insert_item_mut(&mut self, item: Origin) {
        self.items.insert(item);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct InferenceEngine<'a, T, U, A>
    where T: 'a + ConstraintValue,
          U: 'a + Unify<T>,
          A: 'a + Apply<T, U>
{
    pub rules: BTreeMap<&'a String, &'a A>,
    pub facts: BTreeMap<&'a String, &'a U>,
    // Facts derived from this inference process
    pub derived_facts: BTreeMap<String, U>,
    pub pedigree: Pedigree,
    pub prefix: String,
    // Used to check if an inference has already been performed,
    // allowing us to short-circuit a potentially expensive unification process.
    pub origin_cache: OriginCache,
    _marker: PhantomData<T>,
}

impl<'a, T, U, A> InferenceEngine<'a, T, U, A>
    where T: 'a + ConstraintValue,
          U: 'a + Unify<T>,
          A: 'a + Apply<T, U>
{
    pub fn new(prefix: String, rules: BTreeMap<&'a String, &'a A>, facts: BTreeMap<&'a String, &'a U>) -> Self {
        InferenceEngine {
            rules: rules,
            facts: facts,
            derived_facts: BTreeMap::new(),
            pedigree: Pedigree::new(),
            prefix: prefix,
            origin_cache: OriginCache::new(),
            _marker: PhantomData,
        }
    }

    pub fn all_facts(&'a self) -> Vec<(&'a String, &'a U)> {
        self.derived_facts
            .iter()
            .map(|(key, value)| (key, value))
            .chain(self.facts.iter().map(|(key, value)| (key.clone(), value.clone())))
            .collect()
    }

    pub fn chain_until_match(&self, max_iterations: usize, goal: &U) -> (Option<(U, String)>, Self) {
        self.chain_until(max_iterations,
                         &|f| goal.unify(f, &Bindings::new()).is_some())
    }

    pub fn chain_until(&self, max_iterations: usize, satisfied: &Fn(&U) -> bool) -> (Option<(U, String)>, Self) {
        let mut engine = self.clone();
        let mut target: Option<(U, String)> = None;
        for _idx in 0..max_iterations {
            for (fact, _bindings, origin) in engine.chain_forward().into_iter() {
                let id = engine.construct_id(&fact);

                if satisfied(&fact) {
                    target = Some((fact.clone(), id.clone()));
                }

                engine.pedigree.insert_mut(id.clone(), origin);
                engine.derived_facts.insert(id, fact);
            }
            if target.is_some() {
                break;
            }
        }
        (target, engine)
    }

    pub fn chain_forward(&mut self) -> Vec<(U, Bindings<T>, Origin)> {
        let mut origin_cache = self.origin_cache.clone();
        let results = chain_forward(self.all_facts(),
                                    self.rules.iter().map(|(id, rule)| (id.clone(), rule.clone())).collect(),
                                    &mut origin_cache);
        self.origin_cache = origin_cache;
        results
    }

    fn construct_id(&self, _fact: &U) -> String {
        format!("{}-{}", self.prefix, self.derived_facts.len())
    }

    pub fn render_inference_tree(&self, id: &String, render_type: RenderType) -> String {
        let node_renderer = |x| {
            self.facts
                .get(&x)
                .and_then(|y| Some(format!("{}", y)))
                .or_else(|| self.derived_facts.get(&x).and_then(|y| Some(format!("{}", y))))
                .or_else(|| self.rules.get(&x).and_then(|y| Some(format!("{}", y))))
                .unwrap_or(format!("{}?", x))
        };

        self.pedigree.render_inference_tree(id,
                                            &node_renderer,
                                            &node_renderer,
                                            &|x, _y| x.clone(),
                                            render_type)
    }
}

pub fn chain_forward<T, U, A>(facts: Vec<(&String, &U)>, rules: Vec<(&String, &A)>, origin_cache: &mut OriginCache) -> Vec<(U, Bindings<T>, Origin)>
    where T: ConstraintValue,
          U: Unify<T>,
          A: Apply<T, U>
{
    let mut new_facts: Vec<(U, Bindings<T>, Origin)> = Vec::new();

    for (ref rule_id, ref rule) in rules.into_iter() {
        for fact_arg_indexes in construct_fact_args_for_rule(rule.arg_count(), facts.len()).into_iter() {
            let (fact_ids, facts_data): (Vec<_>, Vec<_>) = utils::multi_index(&facts, &fact_arg_indexes)
                .into_iter()
                .unzip();
            let origin = Origin {
                source_id: (*rule_id).clone(),
                args: fact_ids.into_iter().cloned().collect(),
            };
            if origin_cache.has_item(&origin) {
                continue;
            } else {
                origin_cache.insert_item_mut(origin.clone());
            }
            match rule.apply(&facts_data, &Bindings::new()) {
                Some((new_fact, bindings)) => {
                    if is_new_fact(&new_fact, &facts) {
                        new_facts.push((new_fact, bindings, origin))
                    }
                }
                None => (),
            }
        }
    }
    new_facts
}

fn construct_fact_args_for_rule(rule_arg_count: usize, fact_count: usize) -> Vec<Vec<usize>> {
    if rule_arg_count == 0 {
        return vec![Vec::new()];
    }
    if fact_count == 0 {
        return Vec::new();
    }
    let fact_indexes: Vec<usize> = (0..fact_count).collect();
    utils::permutations(fact_indexes, rule_arg_count)
}

fn is_new_fact<T, U>(f: &U, facts: &Vec<(&String, &U)>) -> bool
    where T: ConstraintValue,
          U: Unify<T>
{
    for &(_id, fact) in facts.iter() {
        if fact.equiv(f) {
            return false;
        }
    }
    return true;
}

#[cfg(test)]
mod tests;
