#!/bin/bash

# folder of csv files
dir="$1"
stub="$2"

# cat into single file
cat ${dir}${stub}*.csv |
    grep -v "^\"geoid" > ${dir}${stub}us.csv

# return header to file
head -1 ${dir}${stub}1.csv |
    cat - ${dir}${stub}us.csv > ~/tmp/out &&
    mv ~/tmp/out ${dir}${stub}us.csv
