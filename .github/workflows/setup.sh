#!/usr/bin/env bash
################################################################################

set -xeuo pipefail
if [[ -f '/etc/os-release' ]]; then
    sudo bash -c '
        apt-get update
        apt-get install -y lazarus libgtk2.0-dev
    ' >/dev/null
    declare -rx INSTANTFPCOPTIONS='-Fu/usr/lib/lazarus/*/components/lazutils'
elif [[ -f '/System/Library/CoreServices/SystemVersion.plist' ]]; then
    brew install --cask lazarus >/dev/null
    OPENSSL=$(brew --prefix openssl@1.1)
    PATH+=":/Applications/Lazarus:${OPENSSL%\n}/bin"
    declare -rx DYLD_LIBRARY_PATH="${OPENSSL%\n}/lib" \
        INSTANTFPCOPTIONS='-Fu/Applications/Lazarus/components/lazutils'
fi
