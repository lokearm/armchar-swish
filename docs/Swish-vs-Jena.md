
# Problems upon transition from Jena to Swish

There are several limitations in Swish compared to Jena
as far as I can tell:

+ No prebuilt OWL and RDFS reasoners.  
    - However, reasoning is expensive and only a fraction of the OWL
      and RDFS inferences are actually useful for us.
+ No ready to use function to apply rulesets.
    - However, such generic functions could be expensive.
    - A recursive function `fwdApplyListR` has been implemented
      to solve this problem and it seems to work well.
+ No JSON-LD support
    - However, not using JSON-LD may make the client a lot easier
      to implement
+ No noValue clause
    - However, the noValue clause makes the reasoner expensive.
    - Coding the inference without noValue is more efficient.
+ Rules cannot make new blank nodes.
    - Hence reasoning steps that require this have to be custom made.
+ No arithmetic operations or support for RDF linked lists
    - This also requires some custom made reasoning steps.
