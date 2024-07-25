# Changelog for `LTLplanSAT`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.0.0 - 2024-07-25
The repository of the project has been made public, featuring
* A command line interface for using the solver
* Reading PDDL and SAS input files
* Reading non-nested, quantifier free pddl-constraints
* Soft constraints can be transformed to hard constraints (either randomly, or by stating which constraints should be converted)
* Planning problems can be translated to SAT either via a sequential or an exists-step parallel encoding
* Interaction with a SAT-solver using Ersatz
* Pddl-constraints can be either directly translated to SAT, or first translated to LTL and then to SAT
* The plan obtained from the SAT-solver can be verified using VAL, including the selected soft constraints
