#!/bin/bash

# get tract file
fin="$1"

# get directory of tract file
odir=$(dirname "${fin}")

# check for already made tract_split_* files and remove if so
splfiles=$(ls "${odir}" | awk "{print $9}" | grep "tract_split_*")

for f in $splfiles;do
    if [ -f "${odir}/${f}" ];
    then
	echo "Removing existing: ${odir}/${f}"
	rm "${odir}/${f}"
    fi
done

# open filet
cat "$1" | 			# open file
    grep -v "\.$" | 		# drop bad lines
    awk '(NR>1) && ($1 < 57)' |	# remove FIPS above 56
    awk -F "\"*,\"*" '{print $1$2$3","$5","$6}' |   # concat geoid
    sed 's/\+//g' | 				    # drop '+'
    split -l 1000 - ${odir}/tract_split_	    # split into 1000 line files

# store these files
splfiles=$(ls "$odir" | awk "{print $9}" | grep "tract_split_*")

# counter
i=0

# rename files to give number ending
for f in $splfiles;do
    echo "Converting $f to ${f:0:12}${i}.txt"
    mv ${odir}/$f "${odir}/${f:0:12}${i}.txt"
    ((i+=1))
done

# end
