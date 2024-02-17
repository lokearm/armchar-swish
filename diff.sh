#!/bin/sh

for i in Test/*.ttl
do
   j=`dirname $i`/`basename $i .ttl`
   cs=$j.md
   ad=$j.md
   if test -r $i.md 
   then 
      echo diff $cs $i.md
      diff $cs $i.md
   fi
done

