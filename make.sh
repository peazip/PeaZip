#!/usr/bin/env bash
# if you compile first time you must change variable "lazpath" and "lcl"
# after it execute this script with parameter "all" at awgg dir
# "./build.sh all" it build awgg
#                                                 by Segator
# You can execute this script with different parameters:
# default - compiling AWGG only (using by default)

function f_log
{
    declare -rAi TAG=(
        [error]=31
        [info]=32
        [audit]=33
    )
    printf '%(%y-%m-%d_%T)T\x1b[%dm\t%s:\t%b\x1b[0m\n' -1 "${TAG[${1,,:?}]}" "${1^^}" "${2:?}" 1>&2
    if [[ ${1} == 'error' ]]; then
        return 1
    fi
}

function f_build
{
    lazbuild --add-package 'peazip-sources/dev/metadarkstyle/metadarkstyle.lpk'
    lazbuild 'peazip-sources/dev/project_peach.lpi'
    lazbuild 'peazip-sources/dev/project_pea.lpi'
}

function f_main
{
    set -eo pipefail
    if !(which lazbuild); then
        source '/etc/os-release'
        case ${ID:?} in
            debian | ubuntu)
                sudo apt-get update
                sudo apt-get install -y lazarus
            ;;
        esac
    fi
    case ${1} in
        build) f_build;;
    esac
}

f_main "${@}"
