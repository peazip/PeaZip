/*
 * This file is part of PeaZip.
 *
 * PeaZip is free software: you can redistribute it and/or modify it under the terms of
 * the GNU Lesser General Public License as published by the Free Software Foundation,
 * either version 3 of the License, or (at your option) any later version.
 *
 * PeaZip is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with PeaZip.
 * If not, see <https://www.gnu.org/licenses/>.
 */

 /*
  * PROJECT:   PeaZip
  * FILE:      util.registry.cpp
  * PURPOSE:   Reimplementation for the RegGetString function
  *
  * LICENSE:   LGPL-3
  *
  * DEVELOPER: Makoto Sakaguchi (ycco34vx@gmail.com)
  */

module;
#include "pch.h"

module util.registry;

import std;

LSTATUS RegGetString(const HKEY hKey,
                     const std::wstring_view subKey,
                     const std::wstring_view value,
                     std::wstring& data) noexcept
{
    // Get a large enough size to allocate destination string buffer
    DWORD dataSize = 0;
    auto retCode = RegGetValue(
        hKey,
        subKey.data(),
        value.data(),
        RRF_RT_REG_SZ,
        nullptr,
        nullptr,
        &dataSize
    );

    if (retCode != ERROR_SUCCESS)
    {
        return retCode;
    }

    // Allocate a proper buffer size for the output
    data.resize(dataSize / sizeof(wchar_t));

    // Read string value from the Registry into output string buffer
    retCode = RegGetValue(
        hKey,
        subKey.data(),
        value.data(),
        RRF_RT_REG_SZ,
        nullptr,
        &data[0],
        &dataSize
    );

    if (retCode != ERROR_SUCCESS)
    {
        return retCode;
    }

    // -1 means exclude the NUL written by the Win32 API
    auto actualStringLength = (dataSize / sizeof(wchar_t)) - 1;
    data.resize(actualStringLength);

    return retCode;
}
