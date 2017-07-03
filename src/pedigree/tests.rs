use pedigree::{Pedigree, InferenceGraphBuilder, Origin};

#[test]
fn test_backward_iterator() {
    // Expected in the inference graph
    let d_id1 = "test::datum::1".to_string();
    let d_id2 = "test::datum::2".to_string();
    let d_id3 = "test::datum::3".to_string();

    let a_id0 = "test::actor::0".to_string();
    let a_id1 = "test::actor::1".to_string();
    let a_id2 = "test::actor::2".to_string();

    let inf_graph = vec![(d_id3.clone(),
                          Origin {
                              source_id: a_id2.clone(),
                              args: vec![d_id2.clone()],
                          }),
                         (d_id2.clone(),
                          Origin {
                              source_id: a_id1.clone(),
                              args: vec![d_id1.clone()],
                          }),
                         (d_id1.clone(),
                          Origin {
                              source_id: a_id0.clone(),
                              args: Vec::new(),
                          })]
        .into_iter()
        .fold(Pedigree::new(),
              |ancs, (id, origin)| ancs.insert(id, origin))
        .extract_inference_graph(&d_id3);

    let mut iter = inf_graph.back_iter();
    assert_eq!(iter.next(),
               Some((vec![(&d_id3,
                           Some(&Origin {
                               source_id: a_id2.clone(),
                               args: vec![d_id2.clone()],
                           }))],
                     Vec::new())));
    assert_eq!(iter.next(),
               Some((vec![(&a_id2, None),
                          (&d_id2,
                           Some(&Origin {
                               source_id: a_id1.clone(),
                               args: vec![d_id1.clone()],
                           }))],
                     vec![&vec![d_id3.clone()].into_iter().collect()].into_iter().collect())));
    assert_eq!(iter.next(),
               Some((vec![(&a_id1, None),
                          (&d_id1,
                           Some(&Origin {
                               source_id: a_id0.clone(),
                               args: Vec::new(),
                           }))],
                     vec![&vec![a_id2.clone(), d_id2.clone()].into_iter().collect(), &vec![d_id3.clone()].into_iter().collect()]
                         .into_iter()
                         .collect())));
    assert_eq!(iter.next(),
               Some((vec![(&a_id0, None)],
                     vec![&vec![a_id1.clone(), d_id1.clone()].into_iter().collect(),
                          &vec![a_id2.clone(), d_id2.clone()].into_iter().collect(),
                          &vec![d_id3.clone()].into_iter().collect()])));
    assert_eq!(iter.next(), None);
}


#[test]
fn test_extract_inference_graph() {
    // Expected in the inference graph
    let d_id1 = "test::datum::1".to_string();
    let d_id2 = "test::datum::2".to_string();
    let d_id3 = "test::datum::3".to_string();

    let a_id0 = "test::actor::0".to_string();
    let a_id1 = "test::actor::1".to_string();
    let a_id2 = "test::actor::2".to_string();

    // Not expected in the inference graph
    let d_id4 = "test::datum::4".to_string();

    let pedigree = vec![(d_id3.clone(),
                         Origin {
                             source_id: a_id2.clone(),
                             args: vec![d_id2.clone()],
                         }),
                        (d_id2.clone(),
                         Origin {
                             source_id: a_id1.clone(),
                             args: vec![d_id1.clone()],
                         }),
                        (d_id1.clone(),
                         Origin {
                             source_id: a_id0.clone(),
                             args: Vec::new(),
                         }),
                        (d_id4.clone(),
                         Origin {
                             source_id: a_id0.clone(),
                             args: Vec::new(),
                         })]
        .into_iter()
        .fold(Pedigree::new(),
              |ancs, (id, origin)| ancs.insert(id, origin));

    let expected_pedigree = vec![(d_id3.clone(),
                                  Origin {
                                      source_id: a_id2.clone(),
                                      args: vec![d_id2.clone()],
                                  }),
                                 (d_id2.clone(),
                                  Origin {
                                      source_id: a_id1.clone(),
                                      args: vec![d_id1.clone()],
                                  }),
                                 (d_id1.clone(),
                                  Origin {
                                      source_id: a_id0.clone(),
                                      args: Vec::new(),
                                  })]
        .into_iter()
        .fold(Pedigree::new(),
              |ancs, (id, origin)| ancs.insert(id, origin));

    let expected_inference_graph = InferenceGraphBuilder::new()
        .root(d_id3.clone())
        .leaves(vec![d_id1.clone()].into_iter().collect())
        .pedigree(expected_pedigree)
        .entries_by_generation(vec![vec![a_id0.clone()].into_iter().collect(),
                                    vec![d_id1.clone(), a_id1.clone()].into_iter().collect(),
                                    vec![d_id2.clone(), a_id2.clone()].into_iter().collect(),
                                    vec![d_id3.clone()].into_iter().collect()])
        .entries_to_generation(vec![(a_id0.clone(), 0),
                                    (d_id1.clone(), 1),
                                    (a_id1.clone(), 1),
                                    (d_id2.clone(), 2),
                                    (a_id2.clone(), 2),
                                    (d_id3.clone(), 3)]
            .into_iter()
            .collect())
        .finalize();

    let actual_inference_graph = pedigree.extract_inference_graph(&d_id3);
    assert_eq!(actual_inference_graph.entries_by_generation,
               expected_inference_graph.entries_by_generation,
               "Checking entries_by_generation");
    assert_eq!(actual_inference_graph.entries_to_generation,
               expected_inference_graph.entries_to_generation,
               "Checking entries_to_generation");
    assert_eq!(actual_inference_graph.leaves,
               expected_inference_graph.leaves,
               "Checking leaves");
    assert_eq!(actual_inference_graph.root,
               expected_inference_graph.root,
               "Checking root");
    assert_eq!(actual_inference_graph.pedigree,
               expected_inference_graph.pedigree,
               "Checking pedigree");
    assert_eq!(actual_inference_graph,
               expected_inference_graph,
               "Checking all");
}
