#pragma once

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include <unknwn.h>
#include <shlwapi.h>
#include <shobjidl_core.h>
#include <shellapi.h>

#include <wil/cppwinrt.h>
#include <winrt/base.h>
#include <winrt/Windows.Foundation.h>
#include <winrt/Windows.ApplicationModel.h>
#include <winrt/Microsoft.Windows.ApplicationModel.Resources.h>

#pragma comment(lib, "Shlwapi")
#pragma comment(lib, "Shell32")
