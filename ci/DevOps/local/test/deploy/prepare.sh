#!/usr/bin/env bash
if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/../../../common/common.sh

init_with_root_or_sudo "$0"

begin_banner "project-m36" "deploy prepare"

if [ ! -L ${SCRIPT_ABS_PATH}/../../../../result ]; then
    warn "no project-m36 build result found, suppose that the image would be pull from registry"
else
    LOCAL_IMAGE_LOAD_RESULT=$(sudo sg docker -c "docker load -i ${SCRIPT_ABS_PATH}/../../../../result")
    LOCAL_IMAGE_TAG=$(echo ${LOCAL_IMAGE_LOAD_RESULT} | awk -F"image: " '{print $NF}')
    info "local image tag: ${LOCAL_IMAGE_TAG}"
    info "tagging the image as projectm36/${LOCAL_IMAGE_TAG}"
    sudo sg docker -c "docker tag ${LOCAL_IMAGE_TAG} projectm36/${LOCAL_IMAGE_TAG}"
    info "pushing the tag projectm36/${LOCAL_IMAGE_TAG} to docker.io hub"
    sudo sg docker -c "docker login -u \"${project_m36_DOCKER_HUB_USERNAME}\" -p \"${project_m36_DOCKER_HUB_PASSWORD}\""
    sudo sg docker -c "docker push projectm36/${LOCAL_IMAGE_TAG}"
fi

set +e
myGroup2=$(awk -F":" '{print $1}' /etc/group | grep -w project-m36)
set -e
if [ "X${myGroup2}" = "X" ]; then
    info "no project-m36 group defined yet, create it..."
    sudo groupadd -f --gid 90009 project-m36
fi

set +e
myUser2=$(awk -F":" '{print $1}' /etc/passwd | grep -w project-m36)
set -e
if [ "X${myUser2}" = "X" ]; then
    info "no project-m36 user defined yet, create it..."
    sudo useradd -G docker -m -p Passw0rd --uid 90009 --gid 90009 project-m36
fi

if [ ! -d /var/project-m36 ]; then
    info "no /var/project-m36 directory found, create it..."
    sudo mkdir -p /var/project-m36/data
    sudo mkdir -p /var/project-m36/config
    sudo chown -R project-m36:project-m36 /var/project-m36
fi

sudo cp ${SCRIPT_ABS_PATH}/docker-compose.yml /var/project-m36/docker-compose-project-m36.yml.orig
sudo chown project-m36:project-m36 /var/project-m36/docker-compose-project-m36.yml.orig

sudo sed "s:project-m36_config_path:/var/project-m36/config:g" < /var/project-m36/docker-compose-project-m36.yml.orig | sudo su -p -c "dd of=/var/project-m36/docker-compose-project-m36.yml.01" project-m36 
sudo sed "s:project-m36_data_path:/var/project-m36/data:g" < /var/project-m36/docker-compose-project-m36.yml.01 | sudo su -p -c "dd of=/var/project-m36/docker-compose-project-m36.yml.02" project-m36
sudo sed "s/DOCKER_HUB_IMAGE_TAG/projectm36\/${LOCAL_IMAGE_TAG}/g" < /var/project-m36/docker-compose-project-m36.yml.02 | sudo su -p -c "dd of=/var/project-m36/docker-compose-project-m36.yml" project-m36

done_banner "project-m36" "deploy prepare"
