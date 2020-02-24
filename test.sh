#!/usr/bin/env bash

sbcl --eval "(asdf:test-system :collox)" --eval "(sb-ext:exit)"
