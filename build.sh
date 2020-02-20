#!/usr/bin/env bash

sbcl --eval "(ql:quickload :collox)" --eval "(collox::deploy)"
