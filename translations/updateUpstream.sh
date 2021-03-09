#!/bin/bash

echo "Attempting to push any changed files."

git config user.name github-actions
git config user.email github-actions@github.com

echo "$(git status)"

git add "po" "inst/qml/translations" "*.po" "*.pot"
git diff-index --quiet HEAD

if [ $? = 0 ] ; then
	echo "No translations files were updated, not comitting anything."
	exit 0
fi

# also print the commands for some nicer feedback
echo "$(git status)"
echo "$(git commit -m 'updated translation files')"
echo "$(git status)"
echo "$(git push)"
echo "$(git status)"

if [ $? = 0 ] ; then
	echo "pushed directly to master."
	exit 0;
else
	echo "pushing failed, probably another (force) push happened while this was in progress."
	exit 1;
fi
