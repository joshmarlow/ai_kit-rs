AI_Kit
======

AI_Kit aims to be a single dependency for various clssic AI algorithms.

Core project goals are:
    - use a few core traits that all algorithms operate on
    - convenient and ergonomic interfaces to various algorithms
    - easy to understand implementations
    - only build what you need through the use of feature flags

All of the algorithms (documented below) operate on several core traits, `BindingsValue`, `Unify`, `Operation`.
`ai_kit` provides optional data structures that implement these traits, allowing all algorithms to be usable out of the box - see [Datum](#datum-module) and [Rule](#rule-module).
Quick examples are provided before, followed by more in-depth documentation.

Quick examples
==============

Sudoku Solver
=============

Core
====

`Bindings` - similar to a key/value lookup, but with utilities for ensuring that two (or more) keys have the same value.

`BindingsValue` - a trait allowing a data structure to be used by the `Bindings` data structure.

`Unify` - a trait for data structure can be unified with another of the same type.

`Operation` - a trait for mapping some number of `Unify` instances to some number of other `Unify`s.
This is used for implementing [Forward](#forward-inference) and [Backward](#backward-inference) inferencing.

Algorithms
==========

Constraints
===========

Forward Inference
=================

Feature `with-forward-inference`

Backward Inference
==================

Feature `with-planner`

Pedigree
========

Forward Inference
Default Trait Implementations
=============================

Datum
=====

Feature `with-datum`.

Rule
====

Feature `with-datum`.

Feature Matrix
==============
