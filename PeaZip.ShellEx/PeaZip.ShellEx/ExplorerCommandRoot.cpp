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
 * PURPOSE:   Implementation of the root command for PeaZip context menu
 *            (for files and directories)
 *
 * LICENSE:   LGPL-3
 *
 * DEVELOPER: Makoto Sakaguchi (ycco34vx@gmail.com)
 */

#include "pch.h"
#include "Commands.h"
#include "ExplorerCommandRoot.h"
#include "BrowsePathCommand.h"
#include "AnalyzeCommand.h"
#include "ExplorerCommandBase.h"

import std;
import util.registry;
import util.icon;

using namespace winrt;

namespace ProgID
{
    const std::wstring Add7zCommand = L"PeaZip.add2separate7z";
    const std::wstring Add7zEncryptCommand = L"PeaZip.add2separate7zencrypt";
    const std::wstring AddArchiveCommand = L"PeaZip.add2separate";
    const std::wstring AddGzCommand = L"PeaZip.add2separategz";
    const std::wstring AddSeparateArchiveCommand = L"PeaZip.add2separatesingle";
    const std::wstring AddSfxCommand = L"PeaZip.add2separatesfx";
    const std::wstring AddXzCommand = L"PeaZip.add2separatexz";
    const std::wstring AddZipCommand = L"PeaZip.add2separatezip";
    const std::wstring AddZipMailCommand = L"PeaZip.add2separatezipmail";
    const std::wstring AddZstdCommand = L"PeaZip.add2separatezstd";
    const std::wstring AnalyzeCommand = L"PeaZip.analyze";
    const std::wstring BrowseArchiveCommand = L"PeaZip.ext2browseasarchive";
    const std::wstring BrowsePathCommand = L"PeaZip.ext2browsepath";
    const std::wstring ConvertCommand = L"PeaZip.add2convert";
    const std::wstring ExtractFolderCommand = L"PeaZip.ext2folder";
    const std::wstring ExtractHereCommand = L"PeaZip.ext2here";
    const std::wstring ExtractMainCommand = L"PeaZip.ext2main";
    const std::wstring ExtractSmartCommand = L"PeaZip.ext2smart";
    const std::wstring ExtractTestCommand = L"PeaZip.ext2test";
    const std::wstring SplitCommand = L"PeaZip.add2split";
    const std::wstring WipeCommand = L"PeaZip.add2wipe";
}

struct Separator : implements<Separator, IExplorerCommand>
{
    HRESULT __stdcall GetTitle(IShellItemArray*, LPWSTR* ppszName) noexcept override
    {
        *ppszName = nullptr;
        return S_FALSE;
    }

    HRESULT __stdcall GetIcon(IShellItemArray*, LPWSTR* ppszIcon) noexcept override
    {
        *ppszIcon = nullptr;
        return E_NOTIMPL;
    }

    HRESULT __stdcall GetToolTip(IShellItemArray*, LPWSTR* ppszInfotip) noexcept override
    {
        *ppszInfotip = nullptr;
        return E_NOTIMPL;
    }

    HRESULT __stdcall GetCanonicalName(GUID* pguidCommandName) noexcept override
    {
        *pguidCommandName = GUID_NULL;
        return E_NOTIMPL;
    }

    HRESULT __stdcall GetState(IShellItemArray*, BOOL, EXPCMDSTATE* pCmdState) noexcept override
    {
        *pCmdState = ECS_ENABLED;
        return S_OK;
    }

    HRESULT __stdcall Invoke(IShellItemArray*, IBindCtx*) noexcept override { return S_OK; }

    HRESULT __stdcall GetFlags(EXPCMDFLAGS* pFlags) noexcept override
    {
        *pFlags = ECF_ISSEPARATOR;
        return S_OK;
    }

    HRESULT __stdcall EnumSubCommands(IEnumExplorerCommand** ppEnum) noexcept override
    {
        *ppEnum = nullptr;
        return E_NOTIMPL;
    }
};

ExplorerCommandRoot::ExplorerCommandRoot()
{
    // TODO: Replace the registry to "App data".

    std::wstring subCommands;
    auto retCode = RegGetString(HKEY_CLASSES_ROOT, LR"(AllFilesystemObjects\shell\PeaZip)", L"SubCommands", subCommands);
    if (retCode == ERROR_SUCCESS)
    {
        m_commands.reserve(25);

        std::erase(subCommands, L' ');
        std::wistringstream ss(subCommands);
        std::wstring command;
        bool hadSeparator = false;
        while (std::getline(ss, command, L';'))
        {
            if (command == ProgID::ExtractMainCommand)
            {
                m_commands.push_back(make<ExplorerCommandBase>(EXTRACT_MAIN_COMMAND_ARGS));
            }
            else if (command == ProgID::ExtractHereCommand)
            {
                m_commands.push_back(make<ExplorerCommandBase>(EXTRACT_HERE_COMMAND_ARGS));
            }
            else if (command == ProgID::ExtractSmartCommand)
            {
                m_commands.push_back(make<ExplorerCommandBase>(EXTRACT_SMART_COMMAND_ARGS));
            }
            else if (command == ProgID::ExtractFolderCommand)
            {
                m_commands.push_back(make<ExplorerCommandBase>(EXTRACT_FOLDER_COMMAND_ARGS));
            }
            else if (command == ProgID::ExtractTestCommand)
            {
                m_commands.push_back(make<ExplorerCommandBase>(EXTRACT_TEST_COMMAND_ARGS));
            }
            else if (command == ProgID::BrowseArchiveCommand)
            {
                m_commands.push_back(make<Separator>());
                m_commands.push_back(make<ExplorerCommandBase>(BROWSE_ARCHIVE_COMMAND_ARGS));
            }
            else if (command == ProgID::BrowsePathCommand)
            {
                m_commands.push_back(make<BrowsePathCommand>());
            }
#pragma region Separate
            else if (command == ProgID::AddArchiveCommand)
            {
                if (!hadSeparator)
                {
                    m_commands.push_back(make<Separator>());
                    hadSeparator = true;
                }

                m_commands.push_back(make<ExplorerCommandBase>(ADD_ARCHIVE_COMMAND));
            }
            else if (command == ProgID::AddSeparateArchiveCommand)
            {
                if (!hadSeparator)
                {
                    m_commands.push_back(make<Separator>());
                    hadSeparator = true;
                }

                m_commands.push_back(make<ExplorerCommandBase>(ADD_SEPARATE_ARCHIVE_COMMAND_ARGS));
            }
            else if (command == ProgID::AddSfxCommand)
            {
                if (!hadSeparator)
                {
                    m_commands.push_back(make<Separator>());
                    hadSeparator = true;
                }

                m_commands.push_back(make<ExplorerCommandBase>(ADD_SFX_COMMAND_ARGS));
            }
            else if (command == ProgID::Add7zCommand)
            {
                if (!hadSeparator)
                {
                    m_commands.push_back(make<Separator>());
                    hadSeparator = true;
                }

                m_commands.push_back(make<ExplorerCommandBase>(ADD_7Z_COMMAND_ARGS));
            }
            else if (command == ProgID::AddGzCommand)
            {
                if (!hadSeparator)
                {
                    m_commands.push_back(make<Separator>());
                    hadSeparator = true;
                }

                m_commands.push_back(make<ExplorerCommandBase>(ADD_GZ_COMMAND_ARGS));
            }
            else if (command == ProgID::AddXzCommand)
            {
                if (!hadSeparator)
                {
                    m_commands.push_back(make<Separator>());
                    hadSeparator = true;
                }

                m_commands.push_back(make<ExplorerCommandBase>(ADD_XZ_COMMAND_ARGS));
            }
            else if (command == ProgID::AddZipCommand)
            {
                if (!hadSeparator)
                {
                    m_commands.push_back(make<Separator>());
                    hadSeparator = true;
                }

                m_commands.push_back(make<ExplorerCommandBase>(ADD_ZIP_COMMAND_ARGS));
            }
            else if (command == ProgID::AddZstdCommand)
            {
                if (!hadSeparator)
                {
                    m_commands.push_back(make<Separator>());
                    hadSeparator = true;
                }

                m_commands.push_back(make<ExplorerCommandBase>(ADD_ZSTD_COMMAND_ARGS));
            }
            else if (command == ProgID::Add7zEncryptCommand)
            {
                if (!hadSeparator)
                {
                    m_commands.push_back(make<Separator>());
                    hadSeparator = true;
                }

                m_commands.push_back(make<ExplorerCommandBase>(ADD_7Z_ENCRYPT_COMMAND_ARGS));
            }
            else if (command == ProgID::AddZipMailCommand)
            {
                if (!hadSeparator)
                {
                    m_commands.push_back(make<Separator>());
                    hadSeparator = true;
                }

                m_commands.push_back(make<ExplorerCommandBase>(ADD_ZIP_MAIL_COMMAND_ARGS));
            }
            else if (command == ProgID::SplitCommand)
            {
                if (!hadSeparator)
                {
                    m_commands.push_back(make<Separator>());
                    hadSeparator = true;
                }

                m_commands.push_back(make<ExplorerCommandBase>(SPLIT_COMMAND_ARGS));
            }
            else if (command == ProgID::ConvertCommand)
            {
                if (!hadSeparator)
                {
                    m_commands.push_back(make<Separator>());
                    hadSeparator = true;
                }

                m_commands.push_back(make<ExplorerCommandBase>(CONVERT_COMMAND_ARGS));
            }
#pragma endregion
            else if (command == ProgID::AnalyzeCommand)
            {
                m_commands.push_back(make<Separator>());
                m_commands.push_back(make<AnalyzeCommand>());
            }
            else if (command == ProgID::WipeCommand)
            {
                m_commands.push_back(make<Separator>());
                m_commands.push_back(make<ExplorerCommandBase>(WIPE_COMMAND_ARGS));
            }
        }

        m_commands.shrink_to_fit();
    }
    else
    {
        // Default preset
        m_commands.reserve(15);
        m_commands.push_back(make<ExplorerCommandBase>(EXTRACT_MAIN_COMMAND_ARGS));
        m_commands.push_back(make<ExplorerCommandBase>(EXTRACT_HERE_COMMAND_ARGS));
        m_commands.push_back(make<ExplorerCommandBase>(EXTRACT_SMART_COMMAND_ARGS));
        m_commands.push_back(make<ExplorerCommandBase>(EXTRACT_FOLDER_COMMAND_ARGS));
        m_commands.push_back(make<Separator>());
        m_commands.push_back(make<ExplorerCommandBase>(BROWSE_ARCHIVE_COMMAND_ARGS));
        m_commands.push_back(make<BrowsePathCommand>());
        m_commands.push_back(make<Separator>());
        m_commands.push_back(make<ExplorerCommandBase>(ADD_ARCHIVE_COMMAND));
        m_commands.push_back(make<ExplorerCommandBase>(ADD_7Z_COMMAND_ARGS));
        m_commands.push_back(make<ExplorerCommandBase>(ADD_ZIP_COMMAND_ARGS));
        m_commands.push_back(make<ExplorerCommandBase>(ADD_7Z_ENCRYPT_COMMAND_ARGS));
        m_commands.push_back(make<ExplorerCommandBase>(CONVERT_COMMAND_ARGS));
        m_commands.push_back(make<Separator>());
        m_commands.push_back(make<AnalyzeCommand>());
    }

    m_current = m_commands.cbegin();

    m_initialized = true;
}

#pragma region IExplorerCommand
HRESULT __stdcall ExplorerCommandRoot::GetTitle(IShellItemArray*, LPWSTR* ppszName) noexcept
{
    return SHStrDupW(L"PeaZip", ppszName);
}

HRESULT __stdcall ExplorerCommandRoot::GetIcon(IShellItemArray*, LPWSTR* ppszIcon) noexcept
{
    return SHStrDupW(g_main_app_icon_path.c_str(), ppszIcon);
}

HRESULT __stdcall ExplorerCommandRoot::GetToolTip(IShellItemArray*, LPWSTR* ppszInfotip) noexcept
{
    *ppszInfotip = nullptr;
    return E_NOTIMPL;
}

HRESULT __stdcall ExplorerCommandRoot::GetCanonicalName(GUID* pguidCommandName) noexcept
{
    *pguidCommandName = __uuidof(this);
    return S_OK;
}

HRESULT __stdcall ExplorerCommandRoot::GetState(IShellItemArray*, BOOL, EXPCMDSTATE* pCmdState) noexcept
{
    *pCmdState = ECS_ENABLED;
    return S_OK;
}

HRESULT __stdcall ExplorerCommandRoot::Invoke(IShellItemArray* psiItemArray, IBindCtx* pbc) noexcept
{
    UNREFERENCED_PARAMETER(psiItemArray);
    UNREFERENCED_PARAMETER(pbc);
    return E_NOTIMPL;
}

HRESULT __stdcall ExplorerCommandRoot::GetFlags(EXPCMDFLAGS* pFlags) noexcept
{
    *pFlags = ECF_HASSUBCOMMANDS;
    return S_OK;
}

HRESULT __stdcall ExplorerCommandRoot::EnumSubCommands(IEnumExplorerCommand** ppEnum) noexcept
{
    m_current = m_commands.cbegin();
    AddRef();
    return QueryInterface(IID_PPV_ARGS(ppEnum));
}
#pragma endregion

#pragma region IEnumExplorerCommand
HRESULT __stdcall ExplorerCommandRoot::Next(
    _In_ ULONG celt,
    _Out_writes_to_(celt, *pceltFetched) IExplorerCommand** pUICommand,
    _Out_opt_ ULONG* pceltFetched) noexcept
{
    ULONG fetched = 0;
    for (ULONG i = 0; (i < celt) && (m_current != m_commands.cend()); i++)
    {
        m_current->copy_to(&pUICommand[i]);
        m_current++;
        fetched++;
    }

    wil::assign_to_opt_param(pceltFetched, fetched);
    return (fetched == celt) ? S_OK : S_FALSE;
}

HRESULT __stdcall ExplorerCommandRoot::Skip(ULONG) noexcept { return E_NOTIMPL; }

HRESULT __stdcall ExplorerCommandRoot::Reset(void) noexcept
{
    m_current = m_commands.cbegin();
    return S_OK;
}

HRESULT __stdcall ExplorerCommandRoot::Clone(IEnumExplorerCommand** ppenum) noexcept
{
    *ppenum = nullptr;
    return E_NOTIMPL;
}
#pragma endregion
