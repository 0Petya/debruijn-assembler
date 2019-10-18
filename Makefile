env = dev

.PHONY: all clean
all: node_modules build/bundle.min.js

elm_files = $(wildcard src/elm/*)
js_files = $(wildcard src/js/*)

node_modules: package.json package-lock.json
	npm install

build/bundle.min.js: $(js_files) build/elm.min.js
	browserify src/js/main.js | uglifyjs > build/bundle.min.js

build/elm.min.js: $(elm_files)
ifeq ($(env), dev)
	elm make --debug src/elm/Main.elm --output=build/elm.js
else
ifeq ($(env), prod)
	elm make --optimize src/elm/Main.elm --output=build/elm.js
endif
endif
	uglifyjs build/elm.js > build/elm.min.js

clean:
	rm -rf build
	rm -rf node_modules
	rm -rf elm-stuff
