lock_repo() {
  if [ -z "$1" ]
  then 
    echo "No $3 Weblate repo"
  else
    wlc --key $2 --url https://hosted.weblate.org/api/ lock jasp/$1
}

lock_repo $QML_REPO $WEBLATE_KEY "QML"

lock_repo $R_REPO $WEBLATE_KEY "R"

