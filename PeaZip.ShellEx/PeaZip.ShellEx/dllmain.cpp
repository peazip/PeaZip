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
 * FILE:      dllmain.cpp
 * PURPOSE:   Implementation for PeaZip Shell Extension entry point
 *
 * LICENSE:   LGPL-3
 *
 * DEVELOPER: Makoto Sakaguchi (ycco34vx@gmail.com)
 */

#include "pch.h"
#include "BrowsePathCommand.h"
#include "ExplorerCommandRoot.h"

import std;
import util.icon;

using namespace winrt;

template<class T>
struct ClassFactory : implements<ClassFactory<T>, IClassFactory>
{
    HRESULT STDMETHODCALLTYPE CreateInstance(
        _In_opt_ IUnknown* pUnkOuter,
        _In_ REFIID riid,
        _COM_Outptr_ void** ppvObject) noexcept override final
    {
        UNREFERENCED_PARAMETER(pUnkOuter);

        try
        {
            return winrt::make<T>()->QueryInterface(riid, ppvObject);
        }
        catch (...)
        {
            return winrt::to_hresult();
        }
    }

    HRESULT STDMETHODCALLTYPE LockServer(BOOL fLock) noexcept override final
    {
        if (fLock)
        {
            ++winrt::get_module_lock();
        }
        else
        {
            --winrt::get_module_lock();
        }

        return S_OK;
    }
};

HRESULT __stdcall DllCanUnloadNow()
{
    if (winrt::get_module_lock())
    {
        return S_FALSE;
    }

    winrt::clear_factory_cache();
    return S_OK;
}

HRESULT __stdcall DllGetClassObject(_In_ REFCLSID rclsid, _In_ REFIID riid, _Outptr_ void** instance)
{
    try
    {
        *instance = nullptr;

        if (rclsid == __uuidof(ExplorerCommandRoot))
        {
            return make<ClassFactory<ExplorerCommandRoot>>()->QueryInterface(riid, instance);
        }
        else if (rclsid == __uuidof(BrowsePathCommand))
        {
            return make<ClassFactory<BrowsePathCommand>>()->QueryInterface(riid, instance);
        }

        return winrt::hresult_class_not_available().to_abi();
    }
    catch (...)
    {
        return winrt::to_hresult();
    }
}

BOOL APIENTRY DllMain(HMODULE hModule, DWORD  ul_reason_for_call, LPVOID lpReserved)
{
    switch (ul_reason_for_call)
    {
        case DLL_PROCESS_ATTACH:
            init_icon_path();
            break;
        case DLL_THREAD_ATTACH:
        case DLL_THREAD_DETACH:
        case DLL_PROCESS_DETACH:
            break;
    }
    return TRUE;
}
