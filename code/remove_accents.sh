#!/usr/bin/env bash
# Note: input should end with a newline

# LINES=''
# while read LINE; do
#     LINES="${LINES}\n${LINE}"
# done

input_file=$1

sed -i -e 's/á/a/g' -e 's/Á/A/g' -e 's/é/e/g' -e 's/É/E/g' -e 's/í/i/g' -e 's/Í/I/g' -e 's/ó/o/g' -e 's/Ó/O/g' -e 's/ú/u/g' -e 's/Ú/U/g' -e 's/ñ/n/g' -e 's/Ñ/N/g' -e 's/ü/u/g' -e 's/Ü/U/g' $input_file
