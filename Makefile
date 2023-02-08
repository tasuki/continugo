release:
	elm make src/Main.elm --optimize --output=continugo.js \
		&& uglifyjs continugo.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
		| uglifyjs --mangle --output continugo.js

develop: serve
	find . -name '*.elm' | entr elm make src/Main.elm --debug --output=continugo.js

serve:
	php -S localhost:8000 &
