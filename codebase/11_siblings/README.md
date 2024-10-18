## Find siblings in Numident data

- `01_BunmdPruning.R` — Pare down full BUNMD with cleaned names to matchable sample
- `02a_StandardSiblingLinkage.R`— Link siblings by exact match on parents' names
- `02b_JaroSiblingLinkage.R`— Link siblings using Jaro-Winkler string distance to find close matches on parents' names
- `03_SiblingCleaning.R` — Make final sibling datasets with both matches, including removing very large sibling groups

