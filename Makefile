all:
	ocamlfind ocamlopt main.ml \
	-package nocrypto.unix,calendar,hex,stringext \
	-linkpkg -o Dga
