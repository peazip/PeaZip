#pragma once
#include "ExplorerCommandBase.h"

struct __declspec(uuid("01F6DED1-42C8-4CFA-9F2D-96998BA8BB17")) BrowsePathCommand : ExplorerCommandBase
{
    BrowsePathCommand();
    ~BrowsePathCommand();
    HRESULT __stdcall GetCanonicalName(GUID* pguidCommandName) noexcept override;
};
