#!/bin/sh

if test x$1 -eq xreset
then
   for i in Test/*.ttl
   do
      j=`dirname $i`/`basename $i .ttl`
      cs=$j.md
      ad=$j.md
      if test -r $i.md 
      then 
         echo diff $cs $i.md
         diff $j-advancement.md $j.ttl-advancement.md
         diff $j-chargen.md $j.ttl-chargen.md
         diff $cs $i.md
      fi
   done
else
   for i in Test/*.ttl
   do
      j=`dirname $i`/`basename $i .ttl`
      cs=$j.md
      ad=$j.md
      if test -r $i.md 
      then 
         echo diff $cs $i.md
         cp $j.ttl-advancement.md  $j-advancement.md 
         cp $j.ttl-chargen.md $j-chargen.md 
         cp $i.md $cs 
      fi
   done
fi
