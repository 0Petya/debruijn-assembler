env = dev

.PHONY: all clean
all: node_modules bundle.min.js

elm_files = $(wildcard src/elm/*)
js_files = $(wildcard src/js/*)

node_modules: package.json package-lock.json
	npm install

bundle.min.js: $(js_files) elm.min.js
	browserify src/js/main.js -o bundle.min.js
	# uglifyjs build/bundle.js -c -m -o bundle.min.js

elm.min.js: $(elm_files)
ifeq ($(env), dev)
	elm make --debug src/elm/Main.elm --output=build/elm.js
else
ifeq ($(env), prod)
	elm make --optimize src/elm/Main.elm --output=build/elm.js
endif
endif
	uglifyjs build/elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=elm.min.js


clean:
	rm -rf build
	rm -rf node_modules
	rm -rf elm-stuff
	rm bundle.min.js
	rm elm.min.js
