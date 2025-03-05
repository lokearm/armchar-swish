---
tags:
  - armchar/swish/cli
aliases:
  - "#armchar/swish/cli"
---

## Command line tool

```
( cd Ontology ; make )
cabal run armchar-cli -- -c Test/sylvain.ttl -s Test/saga.ttl -o charactersheet.md
```

There are test files in the `Test` directory.  Please note that the
saga files depend on ontology files and relative paths.  The commands must
be run from the root of the repository.

