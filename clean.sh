#!/bin/bash
echo "Cleaning OCaml build artifacts..."

# Liste des extensions à nettoyer
find . \( \
    -name "*.cmi" -o \
    -name "*.cmx" -o \
    -name "*.o" -o \
    -name "*.a" -o \
    -name "*.cmo" -o \
    -name "*.cmxa" -o \
    -name "*.annot" -o \
    -name "*.byte" -o \
    -name "*.opt" -o \
    -name "*.native" -o \
    -name "a.out" \
\) -type f -delete

echo "Clean complete."
