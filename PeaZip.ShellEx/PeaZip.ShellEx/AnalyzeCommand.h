#pragma once
#include "ExplorerCommandBase.h"

struct AnalyzeCommand : ExplorerCommandBase
{
    AnalyzeCommand();
    ~AnalyzeCommand();

private:
    virtual const wchar_t* Application() const noexcept override final;
};
