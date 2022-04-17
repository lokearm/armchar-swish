# Ontology

Use make to generate the actual ontology (.ttl) and inference logic (.rules) files.
Each file is split into multiple parts (.include and .r respectively) which are
concatenated by make.

The resulting files are
1. arm.ttl - schema for the data model
2. resources.ttl - resources, like lists of abilities, spells, etc.
3. cieran.ttl - sample character
4. contested.ttl - sample saga
5. logic.rules - main inference rules
6. basic.rules - a few cheap inference rules handled separately
