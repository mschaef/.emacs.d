#!/bin/sh

git status | grep deleted | cut -d: -f2 | cut -d/ -f-2 | sort | uniq | xargs git rm -r

git status | grep -v deleted | grep elpa | xargs git add

git commit -m 'Upgrade packages.'
