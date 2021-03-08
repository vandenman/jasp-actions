#!/bin/bash

# this script tries to push directly to upstream. If that fails it opens a new branch and a pr.

git config user.name github-actions
git config user.email github-actions@github.com
git status

git diff-index --quiet HEAD

if [ $? = 0 ] ; then
	echo "No translations files were updated."
	exit 0
fi

git add "*.po$" "*.pot$"
git status
git commit -m "updated translation files"
git status
git remote -v
git push

if [ $? = 0 ] ; then
	echo "pushed directly to master."
else
	branchName="translations_update_$(date +"%Y_%m_%d_%H_%M_%S")"
	echo "pushing failed, creating a new branch called ${branchName} and opening a pull request."
	git checkout -b branchName
	# TODO: who is origin here?
	git push origin branchName
	# TODO: open a pr
fi