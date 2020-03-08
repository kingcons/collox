#!/usr/bin/env bash

sbcl --eval "(ql:quickload :collox)" --eval "(asdf:make :collox)"
