release:
	elm make src/Main.elm --optimize --output=continugo.js

develop: serve
	find . -name '*.elm' | entr elm make src/Main.elm --debug --output=continugo.js

serve:
	php -S localhost:8000 &
