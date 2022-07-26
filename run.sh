python use_replace.py -f wfc2.ml -o bin/wfc.ml
ocamlopt -O3 bin/wfc.ml -o bin/wfc
./bin/wfc 