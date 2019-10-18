env = dev

.PHONY: all clean
all: node_modules build/bundle.js

elm_files = $(wildcard src/elm/*)
js_files = $(wildcard src/js/*)

node_modules: package.json package-lock.json
	npm install

build/bundle.js: $(js_files) build/elm.js
	browserify src/js/main.js > build/bundle.js

build/elm.js: $(elm_files)
ifeq ($(env), dev)
	elm make --debug src/elm/Main.elm --output=build/elm.js
else
ifeq ($(env), prod)
	elm make --optimize src/elm/Main.elm --output=build/elm.js
endif
endif

clean:
	rm -rf build
	rm -rf node_modules
	rm -rf elm-stuff
