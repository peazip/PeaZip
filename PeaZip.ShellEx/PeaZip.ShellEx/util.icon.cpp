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
  * FILE:      util.icon.cpp
  * PURPOSE:   Implementation for PeaZip Shell Extension menu icon util
  *
  * LICENSE:   LGPL-3
  *
  * DEVELOPER: Makoto Sakaguchi (ycco34vx@gmail.com)
  */

module;
#include "pch.h"

module util.icon;

import std;

using namespace winrt;
using namespace Windows::ApplicationModel;

std::wstring g_main_app_icon_path;
std::wstring g_lock_icon_path;
std::wstring g_test_icon_path;
std::wstring g_add_archive_icon_path;
std::wstring g_mail_icon_path;
std::wstring g_extract_icon_path;
std::wstring g_sprit_icon_path;
std::wstring g_delete_icon_path;
std::wstring g_convert_icon_path;
std::wstring g_browse_path_icon_path;
std::wstring g_analyze_icon_path;
std::wstring g_add_7z_icon_path;
std::wstring g_add_sfx_icon_path;
std::wstring g_add_zip_icon_path;
std::wstring g_extract_folder_icon_path;

inline void init_icon_path()
{
    std::filesystem::path module_path{ Package::Current().EffectivePath().c_str() };

    {
        module_path.replace_filename(L"peazip.exe");
        std::wstringstream ss;
        ss << module_path;
        g_main_app_icon_path = std::move(ss.str());
    }

    // Icon path
    module_path.replace_filename(LR"(res\share\icons\peazip_seven.icl)");

    {
        std::wstringstream ss;
        ss << module_path << ",0";
        g_lock_icon_path = std::move(ss.str());
    }

    {
        std::wstringstream ss;
        ss << module_path << ",1";
        g_test_icon_path = std::move(ss.str());
    }

    {
        std::wstringstream ss;
        ss << module_path << ",2";
        g_add_archive_icon_path = std::move(ss.str());
    }

    {
        std::wstringstream ss;
        ss << module_path << ",3";
        g_mail_icon_path = std::move(ss.str());
    }

    {
        std::wstringstream ss;
        ss << module_path << ",4";
        g_extract_icon_path = std::move(ss.str());
    }

    {
        std::wstringstream ss;
        ss << module_path << ",5";
        g_sprit_icon_path = std::move(ss.str());
    }

    {
        std::wstringstream ss;
        ss << module_path << ",6";
        g_delete_icon_path = std::move(ss.str());
    }

    {
        std::wstringstream ss;
        ss << module_path << ",7";
        g_convert_icon_path = std::move(ss.str());
    }

    {
        std::wstringstream ss;
        ss << module_path << ",8";
        g_browse_path_icon_path = std::move(ss.str());
    }

    {
        std::wstringstream ss;
        ss << module_path << ",9";
        g_analyze_icon_path = std::move(ss.str());
    }

    {
        std::wstringstream ss;
        ss << module_path << ",10";
        g_add_7z_icon_path = std::move(ss.str());
    }

    {
        std::wstringstream ss;
        ss << module_path << ",11";
        g_add_sfx_icon_path = std::move(ss.str());
    }

    {
        std::wstringstream ss;
        ss << module_path << ",13";
        g_add_zip_icon_path = std::move(ss.str());
    }

    {
        std::wstringstream ss;
        ss << module_path << ",14";
        g_extract_folder_icon_path = std::move(ss.str());
    }
}
