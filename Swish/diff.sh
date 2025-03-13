#!/bin/sh

for i in Test/*.ttl
do
   j=`dirname $i`/`basename $i .ttl`
   cs=$j.md
   ad=$j.md
   if test x$1 = xreset
   then
         if test -r $i.md 
         then 
            cp $j.ttl-advancement.md  $j-advancement.md 
            cp $j.ttl-chargen.md $j-chargen.md 
            cp $i.md $cs 
         fi
   else
      if test -r $i.md 
      then 
         echo diff $cs $i.md
         diff $j-advancement.md $j.ttl-advancement.md
         diff $j-chargen.md $j.ttl-chargen.md
         diff $cs $i.md
      fi
   fi
done
