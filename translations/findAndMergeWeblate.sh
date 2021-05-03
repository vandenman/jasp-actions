#!/bin/bash

owner_repo="$1"
author='weblate'
pr_number1=''
pr_number2=''

pr_list=$(gh pr list -R $owner_repo -A $author)
first_line=$(echo "$pr_list" | sed -n '1p')
second_line=$(echo "$pr_list" | sed -n '2p')
pr_number1=$(echo "$first_line" | grep -oE '^[0-9]+')
pr_number2=$(echo "$second_line" | grep -oE '^[0-9]+')

if [ -z "$pr_number1" ]
then
  echo "No Pull Request"
else
  echo "Pull Request ID: $pr_number1"
  echo $(gh pr review --approve $pr_number1 -R $owner_repo)
  echo $(gh pr merge --squash $pr_number1 -R $owner_repo)
  
  if [ -z "$pr_number2" ]
  then
    echo "No other Pull Request"
  else
    echo "2nd Pull Request ID: $pr_number2"
    echo $(gh pr review --approve $pr_number2 -R $owner_repo)
    echo $(gh pr merge --squash $pr_number2 -R $owner_repo)
  fi
fi
