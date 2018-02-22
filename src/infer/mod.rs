//! The infer module implements basic forward chaining inference by applying any applicable Operations to a vector of Unifys.

use constraints::ConstraintValue;
use core::{Bindings, BindingsValue, Operation, Unify};
use pedigree::{Origin, Pedigree, RenderType};
use planner::{ConjunctivePlanner, Goal, PlanningConfig};
use serde_json;
use std;
use std::collections::{BTreeMap, BTreeSet};
use std::collections::HashMap;
use std::marker::PhantomData;
use utils;

#[derive(Clone, Debug, Deserialize, PartialEq, PartialOrd, Serialize)]
pub struct Negatable<B: BindingsValue, U: Unify<B>> {
    content: U,
    #[serde(default)]
    is_negative: bool,
    #[serde(default)]
    _marker: PhantomData<B>,
}

impl<B, U> Eq for Negatable<B, U>
where
    B: BindingsValue,
    U: Unify<B>,
{
}

impl<B, U> std::fmt::Display for Negatable<B, U>
where
    B: BindingsValue,
    U: Unify<B>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

impl<B, U> Unify<B> for Negatable<B, U>
where
    B: BindingsValue,
    U: Unify<B>,
{
    fn unify(&self, other: &Self, bindings: &Bindings<B>) -> Option<Bindings<B>> {
        self.content.unify(&other.content, bindings)
    }
    fn apply_bindings(&self, bindings: &Bindings<B>) -> Option<Self> {
        self.content
            .apply_bindings(bindings)
            .and_then(|bound_content| {
                Some(Negatable {
                    content: bound_content,
                    is_negative: self.is_negative,
                    _marker: PhantomData,
                })
            })
    }
    fn variables(&self) -> Vec<String> {
        self.content.variables()
    }
    fn rename_variables(&self, renamed_variables: &HashMap<String, String>) -> Self {
        Negatable {
            content: self.content.rename_variables(renamed_variables),
            is_negative: self.is_negative,
            _marker: PhantomData,
        }
    }
    fn nil() -> Self {
        Negatable {
            content: U::nil(),
            is_negative: false,
            _marker: PhantomData,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct OriginCache {
    items: BTreeSet<Origin>,
}

impl OriginCache {
    pub fn new() -> Self {
        OriginCache {
            items: BTreeSet::new(),
        }
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
where
    T: 'a + ConstraintValue,
    U: 'a + Unify<T>,
    A: 'a + Operation<T, U>,
{
    pub rules: Vec<(&'a String, &'a A)>,
    pub facts: Vec<(&'a String, &'a U)>,
    // Facts derived from this inference process
    pub derived_facts: Vec<(String, U)>,
    pub pedigree: Pedigree,
    pub prefix: String,
    // Used to check if an inference has already been performed,
    // allowing us to short-circuit a potentially expensive unification process.
    pub origin_cache: OriginCache,
    _marker: PhantomData<T>,
}

impl<'a, T, U, A> InferenceEngine<'a, T, U, A>
where
    T: 'a + ConstraintValue,
    U: 'a + Unify<T>,
    A: 'a + Operation<T, U>,
{
    pub fn new(prefix: String, rules: Vec<(&'a String, &'a A)>, facts: Vec<(&'a String, &'a U)>) -> Self {
        InferenceEngine {
            rules: rules,
            facts: facts,
            derived_facts: Vec::new(),
            pedigree: Pedigree::new(),
            prefix: prefix,
            origin_cache: OriginCache::new(),
            _marker: PhantomData,
        }
    }

    pub fn all_facts(&'a self) -> Vec<(&'a String, &'a U)> {
        self.derived_facts
            .iter()
            .map(|&(ref id, ref f)| (id, f))
            .chain(self.facts.iter().map(|&(id, f)| (id, f)))
            .collect()
    }

    pub fn chain_until_match(&self, max_iterations: usize, goal: &U) -> (Option<(U, String)>, Self) {
        self.chain_until(max_iterations, &|f| {
            goal.unify(f, &Bindings::new()).is_some()
        })
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
                engine.derived_facts.push((id, fact));
            }
            if target.is_some() {
                break;
            }
        }
        (target, engine)
    }

    pub fn chain_forward(&mut self) -> Vec<(U, Bindings<T>, Origin)> {
        let mut origin_cache = self.origin_cache.clone();
        let results = chain_forward(self.all_facts(), self.rules.clone(), &mut origin_cache);
        self.origin_cache = origin_cache;
        results
    }

    fn construct_id(&self, _fact: &U) -> String {
        format!("{}-{}", self.prefix, self.derived_facts.len())
    }

    pub fn render_inference_tree(&'a self, id: &String, render_type: RenderType) -> String {
        let all_facts_map: BTreeMap<&'a String, &'a U> = self.all_facts().into_iter().collect();
        let rule_map: BTreeMap<&'a String, &'a A> = self.rules.clone().into_iter().collect();

        let node_renderer = |x| {
            all_facts_map
                .get(&x)
                .and_then(|y| Some(format!("{}", y)))
                .or_else(|| rule_map.get(&x).and_then(|y| Some(format!("{}", y))))
                .unwrap_or(format!("{}?", x))
        };

        self.pedigree.render_inference_tree(
            id,
            &node_renderer,
            &node_renderer,
            &|x, _y| x.clone(),
            render_type,
        )
    }
}

pub fn chain_forward<T, U, A>(facts: Vec<(&String, &U)>, rules: Vec<(&String, &A)>, origin_cache: &mut OriginCache) -> Vec<(U, Bindings<T>, Origin)>
where
    T: ConstraintValue,
    U: Unify<T>,
    A: Operation<T, U>,
{
    let mut derived_facts: Vec<(U, Bindings<T>, Origin)> = Vec::new();
    let just_the_facts: Vec<&U> = facts.iter().map(|&(_id, u)| u).collect();

    for (ref rule_id, ref rule) in rules.into_iter() {
        let planner: ConjunctivePlanner<T, U, A> = ConjunctivePlanner::new(
            rule.input_patterns()
                .into_iter()
                .map(Goal::with_pattern)
                .collect(),
            &Bindings::new(),
            &PlanningConfig::default(),
            just_the_facts.clone(),
            Vec::new(),
        );
        let application_successful =
            |(input_goals, bindings): (Vec<Goal<T, U, A>>, Bindings<T>)| -> Option<(Vec<Goal<T, U, A>>, Vec<U>, Bindings<T>)> {
                let bound_input_goals: Vec<Goal<T, U, A>> = input_goals
                    .iter()
                    .map(|input_goal| {
                        input_goal
                            .apply_bindings(&bindings)
                            .expect("Should be applicable")
                    })
                    .collect();
                rule.apply_match(&bindings)
                    .and_then(|new_facts| Some((bound_input_goals, new_facts, bindings)))
            };

        for (matched_inputs, new_facts, bindings) in planner.filter_map(application_successful) {
            let fact_ids: Vec<String> = extract_datum_indexes(&matched_inputs)
                .iter()
                .map(|idx| facts[*idx].0.clone())
                .collect();
            let origin = Origin {
                source_id: (*rule_id).clone(),
                args: fact_ids,
            };
            if origin_cache.has_item(&origin) {
                continue;
            } else {
                origin_cache.insert_item_mut(origin.clone());
            }
            for new_fact in new_facts {
                if is_new_fact(&new_fact, &facts) {
                    derived_facts.push((new_fact, bindings.clone(), origin.clone()))
                }
            }
        }
    }
    derived_facts
}

pub fn chain_forward_with_negative_goals<T, IU, A>(
    facts: Vec<(&String, &Negatable<T, IU>)>,
    rules: Vec<(&String, &A)>,
    origin_cache: &mut OriginCache,
) -> Vec<(Negatable<T, IU>, Bindings<T>, Origin)>
where
    T: ConstraintValue,
    IU: Unify<T>,
    A: Operation<T, Negatable<T, IU>>,
{
    let mut derived_facts: Vec<(Negatable<T, IU>, Bindings<T>, Origin)> = Vec::new();
    let just_the_facts: Vec<&Negatable<T, IU>> = facts.iter().map(|&(_id, u)| u).collect();

    for (ref rule_id, ref rule) in rules.into_iter() {
        let (negative_inputs, positive_inputs): (Vec<Negatable<T, IU>>, Vec<Negatable<T, IU>>) = rule.input_patterns()
            .into_iter()
            .partition(|input| input.is_negative);
        let planner: ConjunctivePlanner<T, Negatable<T, IU>, A> = ConjunctivePlanner::new(
            positive_inputs
                .into_iter()
                .map(Goal::with_pattern)
                .collect(),
            &Bindings::new(),
            &PlanningConfig::default(),
            just_the_facts.clone().into_iter().collect(),
            Vec::new(),
        );

        let negative_patterns_are_satisfied = |(input_goals, bindings)| {
            utils::map_while_some(&mut negative_inputs.iter(), &|pattern| {
                pattern.apply_bindings(&bindings)
            }).and_then(|bound_negative_patterns| {
                if any_patterns_match(&bound_negative_patterns.iter().collect(), &just_the_facts) {
                    None
                } else {
                    Some((input_goals, bindings))
                }
            })
        };
        let application_successful = |(input_goals, bindings)| {
            rule.apply_match(&bindings)
                .and_then(|new_facts| Some((input_goals, new_facts, bindings)))
        };

        for (matched_inputs, new_facts, bindings) in planner
            .filter_map(negative_patterns_are_satisfied)
            .filter_map(application_successful)
        {
            let fact_ids: Vec<String> = extract_datum_indexes(&matched_inputs)
                .iter()
                .map(|idx| facts[*idx].0.clone())
                .collect();
            let origin = Origin {
                source_id: (*rule_id).clone(),
                args: fact_ids,
            };
            if origin_cache.has_item(&origin) {
                continue;
            } else {
                origin_cache.insert_item_mut(origin.clone());
            }
            for new_fact in new_facts {
                if is_new_fact(&new_fact, &facts) {
                    derived_facts.push((new_fact, bindings.clone(), origin.clone()))
                }
            }
        }
    }
    derived_facts
}

fn any_patterns_match<B, U>(patterns: &Vec<&U>, patterns2: &Vec<&U>) -> bool
where
    B: BindingsValue,
    U: Unify<B>,
{
    let empty_bindings: Bindings<B> = Bindings::new();
    patterns.iter().any(|patt| {
        patterns2
            .iter()
            .any(|f| f.unify(patt, &empty_bindings).is_some())
    })
}

fn extract_datum_indexes<T, U, A>(goals: &Vec<Goal<T, U, A>>) -> Vec<usize>
where
    T: ConstraintValue,
    U: Unify<T>,
    A: Operation<T, U>,
{
    goals
        .iter()
        .map(|goal| {
            goal.unification_index
                .datum_idx()
                .expect("Only datum idx should be here!")
        })
        .collect()
}

fn is_new_fact<T, U>(f: &U, facts: &Vec<(&String, &U)>) -> bool
where
    T: ConstraintValue,
    U: Unify<T>,
{
    let empty_bindings = Bindings::new();
    !facts
        .iter()
        .any(|&(_id, fact)| fact.unify(f, &empty_bindings).is_some())
}

#[cfg(test)]
mod tests;
