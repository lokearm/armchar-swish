

+ `ArM.Types.Advancement` exports three functions
	+ `getPregameAdvancements`
	+ `getIngameAdvancements`
	+ `getAllAdvancements`
+ and the `Advancement` type
+ the three functions are essentially the same (two different subsets and the whole)

```
getAllAdvancements g c = getAdvancements g $ queryGraph inGameAdv c
```

+ `g` is the graph
+ `c` is the character's RDFLabel
+ `inGameAdv` is the class of advancements to be selected
+ The `queryGraph` gets the arc $(s,p,o)$ and $l_p$ where $s$ is the advancement and and $p$ is any property with an RDFS label $l_p$

```
queryGraph c = listToRDFGraph  . f c
   where f c1 c2 = [ arc (Var "id") typeRes c1
            , arc (Var "id") (Var "property") (Var "value")
            , arc (Var "id") (armRes  "advanceCharacter") c2
            , arc (Var "property") resRdfsLabel (Var "label") ]

```

The critical function is thus this
```
getAdvancements g = fixAdvancements g . map toAdvancement . arcListSplit . getGenQuads g
```

Hence we go through several steps
+ `getGenQuads` makes the query and turns the result into arcs $(s,p,o)$
+ `arcListSplit` splits the list into a list of lists per subject (advancement) $s$
+ `toAdvancement` parses the advancement properties
+ `fixAdvancements` parses the traits