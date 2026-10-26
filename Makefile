release: help
	cd frontend && elm make src/Main.elm --optimize --output=public/continugo.js \
		&& uglifyjs public/continugo.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
		| uglifyjs --mangle --output public/continugo.js

help: frontend/src/HelpContent.elm

frontend/src/HelpContent.elm: docs/help.md Makefile
	node -p '"module HelpContent exposing (markdown)\n\n\nmarkdown : String\nmarkdown =\n    " + JSON.stringify(require("fs").readFileSync("docs/help.md", "utf8"))' > $@

develop: help serve
	find frontend/src frontend/tests frontend/benchmarks docs \( -name '*.elm' ! -path 'frontend/src/HelpContent.elm' -o -path 'docs/help.md' \) | entr -s 'make help && cd frontend && elm make src/Main.elm --debug --output=public/continugo.js'

serve:
	serve frontend/public -p 9071 --single &

.PHONY: release help develop serve
