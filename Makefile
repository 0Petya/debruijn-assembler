.PHONY: all clean
all: node_modules bundle.js elm.js

elm_files = $(wildcard src/elm/*)
js_files = $(wildcard src/js/*)

node_modules: package.json package-lock.json
	npm install

bundle.js: $(js_files) elm.js
	browserify src/js/main.js -o bundle.js

elm.js: $(elm_files)
	elm make --debug src/elm/Main.elm --output=elm.js

clean:
	rm -rf node_modules
	rm -rf elm-stuff
	rm bundle.js
	rm elm.js
