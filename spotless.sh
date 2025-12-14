#!/usr/bin/env bash

# A primitive attempt to have very standardized codebase

printf "%s\n" "Removing unused vars..."
clj -M:carve --paths src --interactive false &&
printf "%s\n" "Formatting fils..."
clj -M:format/zprint! src/*.clj deps.edn