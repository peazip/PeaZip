#pragma once
#include "pch.h"

import std;

struct ExplorerCommandBase : winrt::implements<ExplorerCommandBase, IExplorerCommand>
{
    ExplorerCommandBase() = delete;
    ExplorerCommandBase(_In_z_ const std::wstring_view msgId,
                        _In_z_ const std::wstring_view icon,
                        _In_opt_z_ const std::wstring&& option = {});
    virtual ~ExplorerCommandBase();

    HRESULT __stdcall GetTitle(_In_opt_ IShellItemArray* psiItemArray, _Outptr_result_nullonfailure_ LPWSTR* ppszName) noexcept override;
    HRESULT __stdcall GetIcon(_In_opt_ IShellItemArray* psiItemArray, _Outptr_result_nullonfailure_ LPWSTR* ppszIcon) noexcept override;
    HRESULT __stdcall GetToolTip(_In_opt_ IShellItemArray* psiItemArray, _Outptr_result_nullonfailure_ LPWSTR* ppszInfotip) noexcept override;
    virtual HRESULT __stdcall GetCanonicalName(_Out_ GUID* pguidCommandName) noexcept override;
    HRESULT __stdcall GetState(_In_opt_ IShellItemArray* psiItemArray, _In_ BOOL fOkToBeSlow, _Out_ EXPCMDSTATE* pCmdState) noexcept override;
    HRESULT __stdcall Invoke(_In_opt_ IShellItemArray* items, _In_opt_ IBindCtx* pbc) noexcept override;
    HRESULT __stdcall GetFlags(_Out_ EXPCMDFLAGS* pFlags) noexcept override;
    HRESULT __stdcall EnumSubCommands(_COM_Outptr_ IEnumExplorerCommand** ppEnum) noexcept override;

protected:
    std::wstring title;
    std::wstring_view iconPath;
    std::wstring option;

    virtual const wchar_t* Application() const noexcept;

private:
    static const winrt::Microsoft::Windows::ApplicationModel::Resources::ResourceLoader& resource_loader() noexcept;

    void LaunchAppWithArgs(const std::wstring&& commandLine) const noexcept;
    winrt::fire_and_forget InvokeAsync(_In_opt_ IShellItemArray* selection) noexcept;
};
