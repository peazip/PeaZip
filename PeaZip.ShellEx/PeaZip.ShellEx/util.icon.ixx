module;
#include "pch.h"

export module util.icon;

import std;

export __declspec(selectany) std::wstring g_main_app_icon_path;
export __declspec(selectany) std::wstring g_lock_icon_path;
export __declspec(selectany) std::wstring g_test_icon_path;
export __declspec(selectany) std::wstring g_add_archive_icon_path;
export __declspec(selectany) std::wstring g_mail_icon_path;
export __declspec(selectany) std::wstring g_extract_icon_path;
export __declspec(selectany) std::wstring g_sprit_icon_path;
export __declspec(selectany) std::wstring g_delete_icon_path;
export __declspec(selectany) std::wstring g_convert_icon_path;
export __declspec(selectany) std::wstring g_browse_path_icon_path;
export __declspec(selectany) std::wstring g_analyze_icon_path;
export __declspec(selectany) std::wstring g_add_7z_icon_path;
export __declspec(selectany) std::wstring g_add_sfx_icon_path;
export __declspec(selectany) std::wstring g_add_zip_icon_path;
export __declspec(selectany) std::wstring g_extract_folder_icon_path;

export inline void init_icon_path();
