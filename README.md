# armchar-swish

ArM character server implementation using Haskell and Swish.

## Problems

There are several limitations in Swish compared to Jena
As far as I can tell:

+ No prebuilt OWL and RDFS reasoners.  
+ No ready to use function to apply rulesets.
+ Rules cannot easily be defined in a separate file in a separate
  rules language.  The focus of Swish has been the script language.
+ No JSON-LD support
