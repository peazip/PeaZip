#!/usr/bin/env bash

function priv_clippit
(
    cat <<EOF
Usage: bash ${0} [OPTIONS]
Options:
    build   Build program
EOF
)

function priv_main
(
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
    if ((${#})); then
        case ${1} in
            build) pub_build ;;
        esac
    else
        priv_clippit
    fi
)

function pub_build
(
    if {
        pushd 'peazip-sources/dev'
    }; then
        lazbuild --add-package 'metadarkstyle/metadarkstyle.lpk'
        lazbuild 'project_peach.lpi'
        lazbuild 'project_pea.lpi'
        strip peach pea
    fi
)

priv_main "${@}"
