#!/usr/bin/env bash

output_path='/mnt/c/Users/Monica/Desktop/primer_entregable.pdf'

pdflatex main.tex
bibtex main
pdflatex main.tex
pdflatex main.tex
cp main.pdf $output_path
echo "Copied to:"$output_path
