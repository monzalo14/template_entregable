#!/usr/bin/env bash

# Horrible fix for viewing pdf output in a regular PDF Viewer
output_path='/mnt/c/Users/Monica/Desktop/tercer_entregable.pdf'

pdflatex main.tex
bibtex main
pdflatex main.tex
pdflatex main.tex
cp main.pdf $output_path
echo "Copied to:"$output_path
