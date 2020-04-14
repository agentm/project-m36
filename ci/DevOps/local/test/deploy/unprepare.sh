#!/usr/bin/env bash
if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/../../../common/common.sh

init_with_root_or_sudo "$0"

begin_banner "project-m36" "deploy unprepare"

if [ -d /var/project-m36 ]; then
    info "/var/project-m36 directory found, delete it..."
    sudo rm -fr /var/project-m36
fi

set +e
myUser2=$(awk -F":" '{print $1}' /etc/passwd | grep -w project-m36)
if [ "X${myUser2}" != "X" ]; then
    info "project-m36 user defined, delete it..."
    sudo userdel -fr project-m36
fi

myGroup2=$(awk -F":" '{print $1}' /etc/group | grep -w project-m36)
if [ "X${myGroup2}" != "X" ]; then
    info "project-m36 group defined, delete it..."
    sudo groupdel -f project-m36
fi
set -e

MY_TO_REMOVE_IMAGES=$(sudo sg docker -c "docker images"|grep -w project-m36|awk '{print $3}')
for MY_TO_REMOVE_IMAGE in ${MY_TO_REMOVE_IMAGES}
do
    sudo sg docker -c "docker image rm -f ${MY_TO_REMOVE_IMAGE}"
done

done_banner "project-m36" "deploy unprepare"
