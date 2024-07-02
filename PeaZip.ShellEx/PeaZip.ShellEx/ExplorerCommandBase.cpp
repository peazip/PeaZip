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
 * FILE:      AnalyzeCommand.cpp
 * PURPOSE:   Implementation of the command base class for PeaZip context menu
 *
 * LICENSE:   LGPL-3
 *
 * DEVELOPER: Makoto Sakaguchi (ycco34vx@gmail.com)
 */

#include "pch.h"
#include "ExplorerCommandBase.h"

import std;
import util.registry;

using namespace winrt;
using namespace Windows::ApplicationModel;
using namespace Microsoft::Windows::ApplicationModel::Resources;

ExplorerCommandBase::ExplorerCommandBase(
    const std::wstring_view msgId,
    const std::wstring_view icon,
    const std::wstring&& option)
{
    title = std::move(resource_loader().GetString(msgId));
    iconPath = icon;
    this->option = std::move(option);
}

ExplorerCommandBase::~ExplorerCommandBase() {}

#pragma region IExplorerCommand
HRESULT __stdcall ExplorerCommandBase::GetTitle(IShellItemArray*, LPWSTR* ppszName) noexcept
{
    return SHStrDupW(title.c_str(), ppszName);
}

HRESULT __stdcall ExplorerCommandBase::GetIcon(IShellItemArray*, LPWSTR* ppszIcon) noexcept
{
    return SHStrDupW(iconPath.data(), ppszIcon);
}

HRESULT __stdcall ExplorerCommandBase::GetToolTip(IShellItemArray*, LPWSTR* ppszInfotip) noexcept
{
    *ppszInfotip = nullptr;
    return E_NOTIMPL;
}

HRESULT __stdcall ExplorerCommandBase::GetCanonicalName(GUID* guidCommandName) noexcept
{
    *guidCommandName = GUID_NULL;
    return S_OK;
}

HRESULT __stdcall ExplorerCommandBase::GetState(IShellItemArray*, BOOL, EXPCMDSTATE* pCmdState) noexcept
{
    *pCmdState = ECS_ENABLED;
    return S_OK;
}

HRESULT __stdcall ExplorerCommandBase::Invoke(IShellItemArray* items, IBindCtx*) noexcept
{
    try
    {
        InvokeAsync(items);
    }
    catch (const hresult_error& ex)
    {
        MessageBox(nullptr, ex.message().c_str(), L"Error", MB_ICONERROR | MB_OK);
        return ex.code();
    }

    return S_OK;
}

HRESULT __stdcall ExplorerCommandBase::GetFlags(EXPCMDFLAGS* pFlags) noexcept
{
    *pFlags = ECF_DEFAULT;
    return S_OK;
}

HRESULT __stdcall ExplorerCommandBase::EnumSubCommands(IEnumExplorerCommand** ppEnum) noexcept
{
    *ppEnum = nullptr;
    return E_NOTIMPL;
}
#pragma endregion

const wchar_t* ExplorerCommandBase::Application() const noexcept
{
    static const std::wstring app = L"peazip.exe";
    return app.c_str();
}

const ResourceLoader& ExplorerCommandBase::resource_loader() noexcept
{
    static const ResourceLoader resourceLoader{ ResourceLoader::GetDefaultResourceFilePath(), L"PeaZip.ShellEx/Resources" };
    return resourceLoader;
}

void ExplorerCommandBase::LaunchAppWithArgs(const std::wstring&& commandLine) const noexcept
{
    const std::wstring param = std::move(commandLine);
    SHELLEXECUTEINFOW shExecInfo
    {
        sizeof(SHELLEXECUTEINFOW),
        SEE_MASK_UNICODE | SEE_MASK_NO_CONSOLE | SEE_MASK_ASYNCOK,
        nullptr,
        nullptr,
        Application(),
        param.c_str(),
        nullptr,
        SW_SHOWNORMAL,
        nullptr
    };

    ShellExecuteExW(&shExecInfo);
}

fire_and_forget ExplorerCommandBase::InvokeAsync(_In_opt_ IShellItemArray* selection) noexcept
{
    com_ptr<IShellItemArray> selectionCopy;
    selectionCopy.copy_from(selection);
    agile_ref<IShellItemArray> agileSelectionCopy{ selectionCopy };

    // Prevent destruction while coroutine is running
    auto strongThis = get_strong();

    co_await resume_background();

    com_ptr<IShellItemArray> selectionProxy{ agileSelectionCopy.get() };

    DWORD count;
    check_hresult(selectionProxy->GetCount(&count));

    com_ptr<IShellItem> item;
    for (DWORD i = 0; i < count; i++)
    {
        check_hresult(selectionProxy->GetItemAt(i, item.put()));

        LPWSTR path;
        if (SUCCEEDED(item->GetDisplayName(SIGDN_FILESYSPATH, &path)))
        {
            std::wstringstream stream;
            stream << option << '"' << path << '"';

            LaunchAppWithArgs(stream.str());

            CoTaskMemFree(path);
        }

        item = nullptr;
    }
}
