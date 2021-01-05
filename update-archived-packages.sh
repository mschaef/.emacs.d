#!/bin/sh

# Instructions for end-to-end package upgrade:
#
# 1) M-x list-packages
# 2) U (package-menu-mark-upgrades)
# 3) x (package-menu-execute)
# 4) ./update-archived-packages.sh in .emacs.d/

git status | grep deleted | cut -d: -f2 | cut -d/ -f-2 | sort | uniq | xargs git rm -r

git status | grep -v deleted | grep elpa | xargs git add

git commit -m 'Upgrade packages.'
