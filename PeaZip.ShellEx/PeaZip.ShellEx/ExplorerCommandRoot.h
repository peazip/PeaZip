#pragma once
#include "pch.h"

import std;

using SubCommandList = std::vector<winrt::com_ptr<IExplorerCommand>>;

struct __declspec(uuid("01F6DED0-42C8-4CFA-9F2D-96998BA8BB17")) ExplorerCommandRoot : winrt::implements<ExplorerCommandRoot, IExplorerCommand, IEnumExplorerCommand>
{
    ExplorerCommandRoot();
    HRESULT __stdcall GetTitle(IShellItemArray* psiItemArray, LPWSTR* ppszName) noexcept override;
    HRESULT __stdcall GetIcon(IShellItemArray* psiItemArray, LPWSTR* ppszIcon) noexcept override;
    HRESULT __stdcall GetToolTip(IShellItemArray* psiItemArray, LPWSTR* ppszInfotip) noexcept override;
    HRESULT __stdcall GetCanonicalName(GUID* pguidCommandName) noexcept override;
    HRESULT __stdcall GetState(IShellItemArray* psiItemArray, BOOL fOkToBeSlow, EXPCMDSTATE* pCmdState) noexcept override;
    HRESULT __stdcall Invoke(IShellItemArray* psiItemArray, IBindCtx* pbc) noexcept override;
    HRESULT __stdcall GetFlags(EXPCMDFLAGS* pFlags) noexcept override;
    HRESULT __stdcall EnumSubCommands(IEnumExplorerCommand** ppEnum) noexcept override;
    HRESULT __stdcall Next(ULONG celt, IExplorerCommand** pUICommand, ULONG* pceltFetched) noexcept override;
    HRESULT __stdcall Skip(ULONG celt) noexcept override;
    HRESULT __stdcall Reset(void) noexcept override;
    HRESULT __stdcall Clone(IEnumExplorerCommand** ppenum) noexcept override;

private:
    bool m_initialized;
    SubCommandList m_commands;
    SubCommandList::const_iterator m_current;
};
