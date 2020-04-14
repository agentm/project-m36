#!/usr/bin/env bash
if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/../../../common/common.sh

init_with_root_or_sudo "$0"

begin_banner "Top level" "deploy prepare"

if ! type docker >/dev/null 2>&1; then
    info "no docker found, trying to install it"
    case ${THE_DISTRIBUTION_ID} in
        rhel) my_exit "Don't know how to install docker-ce on RHEL, will abort." 1
              ;;
        debian) THE_DISTRIBUTION_VERSION_CODENAME=$(grep -w "VERSION_CODENAME" /etc/os-release |awk -F"=" '{print $NF}'|sed 's/"//g')
                my_arch=$(uname -m)
                if [ ${my_arch} = "aarch64" ]; then
                    docker_arm="arm64"
                else
                    docker_arm="amd64"
                fi
                if dpkg-query -l | grep docker.io ; then
                    sudo apt-get purge -y docker.io 
                fi
                if dpkg-query -l | grep docker-engine ; then
                    sudo apt-get purge -y docker-engine 
                fi
                if dpkg-query -l | grep docker ; then
                    sudo apt-get purge -y docker
                fi
                if dpkg-query -l | grep runc ; then
                    sudo apt-get purge -y runc
                fi
                sudo apt-get update
                sudo apt-get install -y apt-transport-https ca-certificates curl gnupg2 software-properties-common
                curl -fsSL https://download.docker.com/linux/debian/gpg | sudo apt-key add -
                sudo add-apt-repository "deb [arch=${docker_arm}] https://download.docker.com/linux/debian ${THE_DISTRIBUTION_VERSION_CODENAME} stable"
                sudo apt-get update
                sudo apt-get install -y docker-ce docker-ce-cli containerd.io
                sudo /usr/sbin/usermod -a -G docker $(whoami)
                ;;
        centos) sudo yum install -y yum-utils device-mapper-persistent-data lvm2
                sudo yum-config-manager --add-repo https://download.docker.com/linux/centos/docker-ce.repo
                sudo yum install -y docker-ce docker-ce-cli containerd.io
                sudo systemctl enable docker
                sudo systemctl start docker
                sudo /usr/sbin/usermod -a -G docker $(whoami)
                ;;
        Darwin) if type brew > /dev/null 2>&1; then
                    brew cask install docker
                else
                    #wget -c https://download.docker.com/mac/stable/Docker.dmg
                    warn "Cannot install docker desktop for MacOS automatically without homebrew,"
                    warn "please go to following web page to download and isntall the package."
                    warn "https://hub.docker.com/?overlay=onboarding"
                fi
                ;;
        *) my_exit "Not supported distribution OS" 1
           ;;
    esac
fi

if ! type docker-compose >/dev/null 2>&1; then
    info "no docker-compose found, trying to install it"
    case ${THE_DISTRIBUTION_ID} in
        rhel) my_exit "docker-compose depends on docker, yet don't know how to install docker-ce on RHEL, so abord." 1
              ;;
        debian) sudo apt-get update
                sudo apt-get install -y docker-compose
                ;;
        centos) sudo curl -L "https://github.com/docker/compose/releases/download/1.24.1/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
                sudo chmod +x /usr/local/bin/docker-compose
                sudo ln -s /usr/local/bin/docker-compose /usr/bin/docker-compose
                ;;
        Darwin) info "docker-compose will installed with docker desktop for macos. skip."
                ;;
        *) my_exit "Not supported distribution OS" 1
           ;;
    esac
fi

done_banner "Top level" "deploy prepare"
