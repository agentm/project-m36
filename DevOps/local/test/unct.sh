#!/usr/bin/env bash
if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/../../common/common.sh

init_with_root_or_sudo "$0"

begin_banner "Top level" "Cont. UnTest"

warn "Nothing can be filled by default, you must build the top level UnCT pipeline yourself."
warn "Refer to the unct.sh of some specific sub project for some example"

#${SCRIPT_ABS_PATH}/prepare.sh
#${SCRIPT_ABS_PATH}/test/prepare.sh

#${SCRIPT_ABS_PATH}/../../../MY_SUB_PROJECT_NAME/DevOps/MY_LOCATION_NAME/MY_PHASE_NAME/test/unfinishing.sh
#${SCRIPT_ABS_PATH}/../../../MY_SUB_PROJECT_NAME/DevOps/MY_LOCATION_NAME/MY_PHASE_NAME/test/untest.sh
#${SCRIPT_ABS_PATH}/../../../MY_SUB_PROJECT_NAME/DevOps/MY_LOCATION_NAME/MY_PHASE_NAME/test/unprepare.sh
#${SCRIPT_ABS_PATH}/../../../MY_SUB_PROJECT_NAME/DevOps/MY_LOCATION_NAME/MY_PHASE_NAME/unfinishing.sh
#${SCRIPT_ABS_PATH}/../../../MY_SUB_PROJECT_NAME/DevOps/MY_LOCATION_NAME/MY_PHASE_NAME/unprepare.sh
#${SCRIPT_ABS_PATH}/../../../MY_SUB_PROJECT_NAME/DevOps/MY_LOCATION_NAME/MY_PHASE_NAME/prepare.sh
#${SCRIPT_ABS_PATH}/../../../MY_SUB_PROJECT_NAME/DevOps/MY_LOCATION_NAME/MY_PHASE_NAME/test/prepare.sh
#${SCRIPT_ABS_PATH}/../../../MY_SUB_PROJECT_NAME/DevOps/MY_LOCATION_NAME/MY_PHASE_NAME/test/test.sh
#${SCRIPT_ABS_PATH}/../../../MY_SUB_PROJECT_NAME/DevOps/MY_LOCATION_NAME/MY_PHASE_NAME/test/finishing.sh
#${SCRIPT_ABS_PATH}/../../../MY_SUB_PROJECT_NAME/DevOps/MY_LOCATION_NAME/MY_PHASE_NAME/finishing.sh

#${SCRIPT_ABS_PATH}/test/test.sh
#${SCRIPT_ABS_PATH}/test/finishing.sh
#${SCRIPT_ABS_PATH}/finishing.sh

done_banner "Top level" "Cont. UnTest"
