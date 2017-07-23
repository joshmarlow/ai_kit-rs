use itertools::Itertools;
use serde_json;
use std;
use std::collections::btree_map::BTreeMap;
use std::collections::btree_set::BTreeSet;

#[cfg(test)]
mod tests;

#[derive(Clone, Debug)]
pub enum RenderType {
    Full,
    Pedigree,
}

/// Represent the origin of a particular datum
#[derive(Clone, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
pub struct Origin {
    /// What Actor does this Origin correspond to
    pub source_id: String,
    /// What datums did the source use to construct the entity that this Origin corresponds to
    pub args: Vec<String>,
}

impl std::fmt::Display for Origin {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

impl Origin {
    pub fn new() -> Self {
        Origin {
            source_id: String::new(),
            args: Vec::new(),
        }
    }

    pub fn with_source(source: String) -> Self {
        Origin {
            source_id: source,
            args: Vec::new(),
        }
    }

    pub fn ancestors(&self) -> Vec<&String> {
        self.args.iter().chain(std::iter::once(&self.source_id)).collect()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InferenceGraphBackwardIterator<'a> {
    inf_graph: &'a InferenceGraph,
    next_generation: Option<usize>,
}

impl<'a> Iterator for InferenceGraphBackwardIterator<'a> {
    type Item = (Vec<(&'a String, Option<&'a Origin>)>, Vec<&'a BTreeSet<String>>);

    fn next(&mut self) -> Option<(Vec<(&'a String, Option<&'a Origin>)>, Vec<&'a BTreeSet<String>>)> {
        let current_generation = self.next_generation;

        self.next_generation = self.next_generation.and_then(|idx| if idx == 0 { None } else { Some(idx - 1) });

        let construct_id_origin_tuple = |current_id| {
            (current_id,
             self.inf_graph
                 .pedigree
                 .get_ancestor(current_id))
        };

        let construct_id_origin_tuples_for_generation = |generation: &'a BTreeSet<String>| {
            generation.iter()
                .map(construct_id_origin_tuple)
                .collect()
        };

        current_generation.and_then(|generation_idx| {
            self.inf_graph.entries_by_generation.get(generation_idx).and_then(|generation| {
                Some((construct_id_origin_tuples_for_generation(generation), self.inf_graph.subsequent_inferences((generation_idx + 1))))
            })
        })
    }
}

/// Provide a convenient interface to a particular inference graph.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InferenceGraph {
    pedigree: Pedigree,
    /// Track which generation each datum was created in; the root ocurrs in the last generation
    entries_by_generation: Vec<BTreeSet<String>>,
    /// Inverse of entries_by_generation; map each id into the generation it was introduced in
    entries_to_generation: BTreeMap<String, usize>,
    /// The endpoint data for the inference graph
    /// For forward chaining, this is the initial data used in the inference.
    /// For backward chaining, this is the final grounded results.
    leaves: BTreeSet<String>,
    /// The goal of the infernece
    root: String,
}

impl<'a> InferenceGraph {
    pub fn new(root: String) -> Self {
        InferenceGraph {
            pedigree: Pedigree::new(),
            entries_by_generation: Vec::new(),
            entries_to_generation: BTreeMap::new(),
            leaves: BTreeSet::new(),
            root: root,
        }
    }

    pub fn back_iter(&self) -> InferenceGraphBackwardIterator {
        InferenceGraphBackwardIterator {
            inf_graph: self,
            next_generation: Some(self.entries_by_generation.len() - 1),
        }
    }

    pub fn root(&self) -> &String {
        &self.root
    }

    pub fn leaves(&'a self) -> &'a BTreeSet<String> {
        &self.leaves
    }

    pub fn ancestor(&self, id: &String) -> Option<&Origin> {
        self.pedigree.get_ancestor(id)
    }

    pub fn descendent_inferences(&'a self, id: &String) -> Option<&'a BTreeSet<String>> {
        /// Return datums derived from this one
        self.pedigree.get_descendents(id)
    }

    pub fn subsequent_inferences(&'a self, generation: usize) -> Vec<&'a BTreeSet<String>> {
        /// Return all datums derived in and after the specified generation
        let mut subsequent_inferences = Vec::new();

        for entries in self.entries_by_generation
            .iter()
            .skip(generation) {
            subsequent_inferences.push(entries);
        }

        subsequent_inferences
    }

    pub fn all_ids(&'a self) -> BTreeSet<&'a String> {
        self.entries_to_generation.keys().collect()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InferenceGraphBuilder {
    pedigree: Pedigree,
    entries_by_generation: Vec<BTreeSet<String>>,
    entries_to_generation: BTreeMap<String, usize>,
    leaves: BTreeSet<String>,
    root: String,
}

impl InferenceGraphBuilder {
    pub fn new() -> Self {
        InferenceGraphBuilder {
            pedigree: Pedigree::new(),
            entries_by_generation: Vec::new(),
            entries_to_generation: BTreeMap::new(),
            leaves: BTreeSet::new(),
            root: String::new(),
        }
    }

    pub fn finalize(self) -> InferenceGraph {
        InferenceGraph {
            pedigree: self.pedigree,
            entries_by_generation: self.entries_by_generation,
            entries_to_generation: self.entries_to_generation,
            leaves: self.leaves,
            root: self.root,
        }
    }

    pub fn pedigree(self, pedigree: Pedigree) -> Self {
        let mut igraph = self.clone();
        igraph.pedigree = pedigree;
        igraph
    }

    pub fn update_pedigree(self, id: String, origin: Origin) -> Self {
        let mut igraph = self.clone();
        igraph.pedigree = igraph.pedigree.insert(id, origin);
        igraph
    }

    pub fn entries_by_generation(self, entries_by_generation: Vec<BTreeSet<String>>) -> Self {
        let mut igraph = self.clone();
        igraph.entries_by_generation = entries_by_generation;
        igraph
    }

    pub fn extend_entries_by_generation(self, generation_idx: usize, entries: Vec<String>) -> Self {
        let mut entries_by_generation = self.entries_by_generation.clone();
        let new_entries: BTreeSet<String> = entries.into_iter().collect();

        if entries_by_generation.len() <= generation_idx {
            entries_by_generation.push(new_entries);
        } else {
            if let Some(generation) = entries_by_generation.get_mut(generation_idx) {
                generation.extend(new_entries);
            }
        }

        let mut igraph = self.clone();
        igraph.entries_by_generation = entries_by_generation;
        igraph
    }

    pub fn entries_to_generation(self, entries_to_generation: BTreeMap<String, usize>) -> Self {
        let mut igraph = self.clone();
        igraph.entries_to_generation = entries_to_generation;
        igraph
    }

    pub fn leaves(self, leaves: BTreeSet<String>) -> Self {
        let mut igraph = self.clone();
        igraph.leaves = leaves;
        igraph
    }

    pub fn update_leaves(self, id: String) -> Self {
        let mut igraph = self.clone();
        igraph.leaves.insert(id);
        igraph
    }

    pub fn root(self, root: String) -> Self {
        let mut igraph = self.clone();
        igraph.root = root;
        igraph
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct InferenceChain {
    pub elements: Vec<(Vec<(String, Option<Origin>)>)>,
}

impl std::fmt::Display for InferenceChain {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Pedigree {
    ancestors: BTreeMap<String, Origin>,
    descendents: BTreeMap<String, BTreeSet<String>>,
}

impl Pedigree {
    pub fn new() -> Self {
        Pedigree {
            ancestors: BTreeMap::new(),
            descendents: BTreeMap::new(),
        }
    }

    pub fn insert(&self, id: String, org: Origin) -> Self {
        let mut pedigree = self.clone();
        pedigree.insert_mut(id, org);
        pedigree
    }

    pub fn insert_mut(&mut self, id: String, org: Origin) {
        let source_id = org.source_id.clone();
        self.ancestors.insert(id.clone(), org);

        if !self.descendents.contains_key(&source_id) {
            self.descendents.insert(source_id.clone(), BTreeSet::new());
        }
        let mut inner_descendents = self.descendents.get_mut(&source_id).unwrap();
        inner_descendents.insert(id);
    }

    pub fn get_ancestor(&self, id: &String) -> Option<&Origin> {
        self.ancestors.get(id)
    }

    pub fn get_descendents(&self, id: &String) -> Option<&BTreeSet<String>> {
        self.descendents.get(id)
    }

    /// Remove all reference to the specified id
    pub fn purge(&self, id: &String) -> Self {
        let mut pedigree = self.clone();
        pedigree.purge_mut(id);
        pedigree
    }

    pub fn purge_mut(&mut self, id: &String) {
        self.ancestors.remove(id);

        let mut descendents = self.descendents.clone();

        for (ancestor_id, these_descendents) in self.descendents.iter() {
            if these_descendents.contains(id) {
                let mut these_descendents = these_descendents.clone();
                these_descendents.remove(id);
                descendents.insert(ancestor_id.clone(), these_descendents);
            }
        }

        self.descendents = descendents;
    }

    pub fn extract_inference_chain(&self, root: &String) -> InferenceChain {
        InferenceChain {
            elements: self.extract_inference_graph(root)
                .back_iter()
                .map(|(generation, _subsequent_generations)| {
                    generation.into_iter().map(|(ref id, ref origin)| ((*id).clone(), (*origin).cloned())).collect()
                })
                .collect(),
        }
    }

    pub fn extract_inference_graph(&self, root: &String) -> InferenceGraph {
        let builder = InferenceGraphBuilder::new().root(root.clone());

        // Note: we construct the entries_by_generation vector in reverse since we don't know in advance the how big it will be
        let mut builder = self.extract_inference_graph_helper(builder, vec![root], 0);

        // Reverse the order of the entries_by_generation so initial inferences are first and the root is last
        builder.entries_by_generation.reverse();
        let mut entries_to_generation: BTreeMap<String, usize> = BTreeMap::new();
        for (idx, entries) in builder.entries_by_generation.iter().enumerate() {
            for entry in entries.iter() {
                entries_to_generation.insert(entry.clone(), idx);
            }
        }
        builder.entries_to_generation = entries_to_generation;
        builder.finalize()
    }

    fn extract_inference_graph_helper(&self,
                                      builder: InferenceGraphBuilder,
                                      current_generation: Vec<&String>,
                                      generations_from_root: usize)
                                      -> InferenceGraphBuilder {
        if current_generation.len() > 0 {
            let builder = builder.extend_entries_by_generation(generations_from_root,
                                                               current_generation.iter().cloned().cloned().collect());
            current_generation.into_iter().fold(builder,
                                                |builder, current_id| match self.get_ancestor(current_id).cloned() {
                                                    None => builder,
                                                    Some(ref origin) => {
                                                        let builder = builder.update_pedigree(current_id.clone(), origin.clone());
                                                        let builder = if origin.args.is_empty() {
                                                            builder.update_leaves(current_id.clone())
                                                        } else {
                                                            builder
                                                        };
                                                        self.extract_inference_graph_helper(builder,
                                                                                            origin.ancestors()
                                                                                                .iter()
                                                                                                .cloned()
                                                                                                .collect(),
                                                                                            generations_from_root + 1)
                                                    }
                                                })
        } else {
            builder
        }
    }

    pub fn render_inference_tree(&self,
                                 d_id: &String,
                                 root_renderer: &Fn(String) -> String,
                                 node_renderer: &Fn(String) -> String,
                                 relation_renderder: &Fn(String, String) -> String,
                                 render_type: RenderType)
                                 -> String {
        let s = match render_type {
            RenderType::Pedigree => self.render_inference_tree_pedigree(d_id, node_renderer),
            RenderType::Full => self.render_inference_tree_full(d_id, node_renderer, relation_renderder),
        };
        format!("graph \"inference chain for {}\" {{\n{}\n}}",
                root_renderer(d_id.clone()),
                s)
    }

    pub fn render_inference_tree_full(&self,
                                      d_id: &String,
                                      node_renderer: &Fn(String) -> String,
                                      relation_renderder: &Fn(String, String) -> String)
                                      -> String {
        let inf_graph = self.extract_inference_graph(d_id);
        let mut relationships: Vec<String> = Vec::new();

        for (current_generation, future_generations) in inf_graph.back_iter() {
            // For each generation
            for future_generation in future_generations.iter() {
                // For each following generation
                for (&(ref current_id, ref _origin), descendent_id) in
                    current_generation.iter()
                        .cartesian_product(future_generation.iter()) {
                    // For each pairing of current generation
                    relationships.push(format!("\"{}\" -- \"{}\" \"{}\"",
                                               node_renderer((*current_id).clone()),
                                               node_renderer(descendent_id.clone()),
                                               relation_renderder((*current_id).clone(), (*descendent_id).clone())));
                }
            }
        }
        relationships.iter().join(";\n")
    }

    pub fn render_inference_tree_pedigree(&self, d_id: &String, node_renderer: &Fn(String) -> String) -> String {
        let mut relationships: Vec<String> = Vec::new();
        if let Some(origin) = self.ancestors.get(d_id) {
            // Render the relationship to the parent actor
            relationships.push(format!(r#""{}" -- "{}""#,
                                       node_renderer(d_id.clone()),
                                       node_renderer(origin.source_id.clone())));
            for arg in origin.args.iter() {
                // Render the relationship to each parent actor argument
                relationships.push(format!(r#""{}" -- "{}""#,
                                           node_renderer(d_id.clone()),
                                           node_renderer(arg.clone())));
                // Render each parent actor argument's relationships
                relationships.push(self.render_inference_tree_pedigree(arg, node_renderer));
            }
        }
        relationships.iter().join(";\n")
    }
}
