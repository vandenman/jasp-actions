#!/bin/bash

owner_repo="$1"
author='weblate'

get_url="https://api.github.com/search/issues?q=is:pr+repo:${owner_repo}+author:${author}+state:open"

curl_reply=$(curl \
	-H "Accept: application/vnd.github.v3+json" \
	$get_url \
)

no_open_webplate_prs=$(echo "$curl_reply" | tr '\r\n' ' ' | jq '.total_count')
if [ -z "${no_open_webplate_prs}" ] ; then
	no_open_webplate_prs="-1"
fi

if [ "${no_open_webplate_prs}" = "null" ] ; then
	echo "Error: did not get an expected reply from the github api. This probably requires human intervention. The response was:"
	echo
	echo "${curl_reply}"
	exit 1
fi

echo
case ${no_open_webplate_prs} in
	-1)

		echo "Error: did not get an expected reply from the github api. This probably requires human intervention. The response was:"
		echo
		echo "${curl_reply}"
		exit 1;;

	0)
		echo "found no open weblate PRs, aborting early."
		# TODO: we could abort the entire check early now but exiting with nonzero causes the check to fail.
		exit 0;;

	1)
		pull_id=$(echo "$curl_reply" | tr '\r\n' ' ' | jq '.items[0].number')

		echo "found 1 open weblate PR with number ${pull_id}. Merging it locally."

		git config user.name github-actions
		git config user.email github-actions@github.com
		echo "$(git status)"
		echo "$(git merge refs/pull/${pull_id}/head)"

		exit 0;;

	*)
		echo "Found 2 or more open weblate PRs. This probably requires human intervention. The response was:"
		echo
		echo "$curl_reply"
		exit 1;;

esac
