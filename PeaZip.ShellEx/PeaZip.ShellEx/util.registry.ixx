module;
#include "pch.h"

export module util.registry;

import std;

export LSTATUS RegGetString(
    const HKEY hKey,
    const std::wstring_view subKey,
    const std::wstring_view value,
    std::wstring& data
) noexcept;
