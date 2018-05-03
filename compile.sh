#!/usr/bin/env bash

pdflatex main.tex
bibtext main
pdflatex main.tex
pdflatex main.tex
cp main.pdf /mnt/c/Users/Monica/Desktop/primer_entregable.pdf
