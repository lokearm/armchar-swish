
+ [[RDF Classes.canvas|RDF Classes]]
+ [[Traits and Possessions]]


## Generic features

### Class membership

+ Special ArM property to identify non-transitive class membership
	+ `arm:traitClass`
	+ `arm:advancementClass`
+ This facilitates some reasoning, being able to identify the direct membership of a key class, without being caught up in generic classes (like `owl:Object`)
### Label  and Description

+ All ArM classes and objects have label and description
+ The label is a short string, suitable for listing
+ The description is a longer description
+ Label and description can be defined both at class and instance level
+ Class level description (`hasDescription` and `hasLabel`) are generic
	+ inherited by every instance
+ Instance level description (`instaceDescription` and `instanceLabel`) are more specific
