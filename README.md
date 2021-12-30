# Collox

[![Build Status](https://travis-ci.org/kingcons/collox.svg?branch=master)](https://travis-ci.org/kingcons/collox)

> A Common Lisp Lox interprer. (Pronounced like bollocks.)

## Usage

Don't. ... Yet.

## Installation

`git clone` this project into your quicklisp local-projects folder and quickload.

## Testing

Tests written with fiasco.

```
(asdf:test-system :collox)
```

Or if running tests at the REPL...

````
;; Make sure you have quickloaded :collox/tests
(fiasco:run-tests '(collox/tests))
````

## Author

* Brit Butler (brit@kingcons.io)

## Copyright

Copyright (c) 2021 Brit Butler (brit@kingcons.io)

## License

Licensed under the BSD License.
