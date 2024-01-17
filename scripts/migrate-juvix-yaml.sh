#!/usr/bin/env bash

# Projects must be migrated depth-first because Package.juvix is searched before juvix.yaml
# (so Package.juvix in parent directories are used before juvix.yaml in the current directory)
find . -type f -name "juvix.yaml" | awk -F'/' '{print NF-1 " " $0}' | sort -nr | cut -d' ' -f2- | while IFS= read -r file; do
    dir=$(dirname "$file")
    echo "migrating: $dir"
    (cd "$dir" && juvix dev migrate-juvix-yaml)
done
