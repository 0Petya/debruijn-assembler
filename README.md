[![Build Status](https://travis-ci.org/0Petya/debruijn-generator.svg?branch=master)](https://travis-ci.org/0Petya/debruijn-generator)

# De Bruijn Graph Generator

An online De Bruijn graph generator.

Check it out [here](https://0petya.github.io/debruijn-generator/)!

# Setup

To setup locally for development, you'll need a few dependencies:
* [Make](https://www.gnu.org/software/make/)
* [Elm](https://elm-lang.org)
* [Node.js](https://nodejs.org/en/)
* [Browserify](http://browserify.org/)
* [UglifyJS](https://www.npmjs.com/package/uglify-js)

To build the project, you just need to execute `make`:
```
make
```

To build for production use:
```
make env=prod
```
