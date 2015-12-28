#!/bin/bash
 
# This needs heirloom-mailx
from=$1
to=$2
subject=$3
body=$4
declare -a attachments
attachments=( $5 )
 
declare -a attargs
for att in "${attachments[@]}"; do
  attargs+=( "-a"  "$att" )  
done
 
mail -s "$subject" -r "$from" "${attargs[@]}" "$to" <<< "$body"