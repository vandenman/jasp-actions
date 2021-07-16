lock_repo() {
   echo "unlock $1"
   wlc --key $2 --url https://hosted.weblate.org/api/ reset jasp/$1
   wlc --key $2 --url https://hosted.weblate.org/api/ unlock jasp/$1
}

if [ "$QML_REPO" ]
then
  lock_repo $QML_REPO $WEBLATE_KEY
fi

if [ "$R_REPO" ]
then
  lock_repo $R_REPO $WEBLATE_KEY
fi
