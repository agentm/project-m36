#!/usr/bin/env bash
set -Eeuo pipefail

guard_bash_error () {
    set -Eeuo pipefail
}

# Log levels
INFO=0
WARN=1
ERROR=2
FATAL=3
DEBUG=4
DEFAULT_LOG_LEVEL=${ERROR}

my_exit () {
    echo "EXIT: - [HOST:$(hostname)]: - $(date +"%Y-%m-%d %H:%M:%S") - $1"
    exit "$2"
}

msg () {
    if [ $1 -le ${DEFAULT_LOG_LEVEL} ]; then
        echo "[HOST:$(hostname)]: - $(date +"%Y-%m-%d %H:%M:%S") - $2"
    fi
}

info () {
    msg ${INFO} "INFO: - $1"
}

warn () {
    msg ${WARN} "WARNING: - $1"
}

error () {
    msg ${ERROR} "ERROR: - $1"
}

fatal () {
    msg ${FATAL} "FATAL: - $1"
}

debug () {
    msg ${DEBUG} "DEBUG: - $1"
}

begin_banner () {
    info "$1 - $2 phase - begin"
}

done_banner () {
    info "$1 - $2 phase - done"
}

### turn path within script into absolute path
### must pass the calling string of the script as the first parameter
### e.g., ./path_to_script/script.sh
### or, /root/path_to_script/script.sh
### return the absolute path to the script with "echo" command
turn_to_absolute_path () {
    local SCRIPT_ABS_PATH_RAW="$(dirname "$1")"
    # turn SCRIPT_ABS_PATH into absolute path
    case ${SCRIPT_ABS_PATH_RAW} in
        /*) echo "${SCRIPT_ABS_PATH_RAW}" ;;
        \.\.*) echo "$PWD/${SCRIPT_ABS_PATH_RAW}" ;;
        \.*) echo "$PWD/${SCRIPT_ABS_PATH_RAW}" ;;
        *) echo "$PWD" ;;
    esac
}

### change CD to up to the project root directory
### must pass the absolute path to the script as the first parameter
change_CD_to_project_root () {
    cd "$1"
    local up_level=..
    local my_loop=10 # guard not to loop forever
    until ls "${up_level}"|grep -w DevOps > /dev/null 2>&1 && [ ${my_loop} -gt 0 ]
    do
        up_level=${up_level}/..
        my_loop=$(expr ${my_loop} - 1)
    done
    if [ ${my_loop} -eq 0 ]; then
        my_exit "Too many level up within the searching for DevOps directory,abort." 1
    fi
    cd "$1/${up_level}"
}

### check OS and distribution
### return the OS distribution and ID with "echo" command
check_dist_or_OS () {
    local MY_THE_DISTRIBUTION_ID=""
    local MY_THE_DISTRIBUTION_VERSION=""
    if [ -e /etc/os-release ]; then
        MY_THE_DISTRIBUTION_ID=$(grep -w "ID" /etc/os-release |awk -F"=" '{print $NF}'|sed 's/"//g')
        MY_THE_DISTRIBUTION_VERSION=$(grep -w "VERSION_ID" /etc/os-release |awk -F"=" '{print $NF}'|awk -F"." '{print $1}'|sed 's/"//g')
        echo "${MY_THE_DISTRIBUTION_ID} ${MY_THE_DISTRIBUTION_VERSION}"
    else if type uname > /dev/null 2>&1; then
             MY_THE_DISTRIBUTION_ID=$(uname -s)
             MY_THE_DISTRIBUTION_VERSION=$(uname -r)
             echo "${MY_THE_DISTRIBUTION_ID} ${MY_THE_DISTRIBUTION_VERSION}"
         else
             echo ""
         fi
    fi
}

### guard that the caller of the script must be root or has sudo right
guard_root_or_sudo () {
    if [[ $EUID > 0 ]] && ! sudo -v >/dev/null 2>&1; then
        return 1
    else
        return 0
    fi
}

### init script with check if root or sudo
init_with_root_or_sudo () {
    guard_bash_error

    if ! guard_root_or_sudo; then
        my_exit "You must be root or you must be sudoer to prepare the env for CI/CD." 1
    fi

    SCRIPT_ABS_PATH=$(turn_to_absolute_path $0)

    change_CD_to_project_root ${SCRIPT_ABS_PATH}

    THE_DISTRIBUTION_ID_VERSION=$(check_dist_or_OS)
    THE_DISTRIBUTION_ID=$(echo ${THE_DISTRIBUTION_ID_VERSION}|awk '{print $1}')
    THE_DISTRIBUTION_VERSION=$(echo ${THE_DISTRIBUTION_ID_VERSION}|awk '{print $2}')
}

### init script without check if root or sudo
init_without_root_or_sudo () {
    guard_bash_error

    SCRIPT_ABS_PATH=$(turn_to_absolute_path $0)

    change_CD_to_project_root ${SCRIPT_ABS_PATH}

    THE_DISTRIBUTION_ID_VERSION=$(check_dist_or_OS)
    THE_DISTRIBUTION_ID=$(echo ${THE_DISTRIBUTION_ID_VERSION}|awk '{print $1}')
    THE_DISTRIBUTION_VERSION=$(echo ${THE_DISTRIBUTION_ID_VERSION}|awk '{print $2}')
}
