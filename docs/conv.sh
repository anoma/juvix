#!/bin/zsh
# Adapted from https://jonaprieto.github.io/synthetic-graph-theory/conv.sh
#
# 1. Install pandoc from http://johnmacfarlane.net/pandoc/
# 2. Copy this script into the directory containing the .md files
# 3. Ensure that the script has all the permissions to be executed
# $ chmod +x conv.sh
# 4. Run the script
# $ ./conv.sh

PANDOCVERSION=$(pandoc --version | head -n 1)

FILES="*.md"

for f in $FILES
do
  filename="${f%.*}"
  html=$filename.html
  logdata=$(git log --pretty="format:(%as)(%h) %Creset%s by %cl" --no-merges -20)
  echo "--------------------------------------------------------------------------------"
  pandoc --standalone \
         --metadata-file="_config.yml" \
         --template=template.html5 \
         "$f" \
         --from markdown+tex_math_dollars+tex_math_double_backslash+latex_macros+lists_without_preceding_blankline \
         --to=html  \
         --mathjax \
         -o "$html" \
         --variable=updated:"$(date +'%A, %B %e, %Y, %I:%M %p')" \
         --variable=lastCommit:"$logdata" \
         --variable=pandocVersion:"${PANDOCVERSION:u}" \
         --variable=file:"src/$filename"
done
cp README.html index.html
