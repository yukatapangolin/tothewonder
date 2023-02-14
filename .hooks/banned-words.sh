#!/usr/bin/env bash
set -euo pipefail

bad_words=(socks
           "subhuman trash"
           "\bkill\b"
           "\bdie[^dt]{1}"
           "fuck"
           "\bshit\b"
           "exterminate"
           "localhost"
           "murder"
           "data-raw/ERS10/ERS-2010-2fcqzlr.tar.xz"
           "data-raw/ERS10/counties10-zqvz0r.csv"
           "data-raw/2020_US_County_Level_Presidential_Results.csv"
           "data-raw/ERS10/aiannh.tar.xz")

CHANGED_FILES=$(git diff --name-only --cached --diff-filter=ACMR)
FILES=$(echo "$CHANGED_FILES" | sed 's/\.hooks\/banned-words.sh\b//')
for i in "${bad_words[@]}"
do
    echo "$FILES" | xargs grep --with-filename -n "$i" &&
        echo "COMMIT REJECTED! Found '$i'" &&
        exit 1
done

exit 0
