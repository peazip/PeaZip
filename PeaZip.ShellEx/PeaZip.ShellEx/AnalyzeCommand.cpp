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
 * PURPOSE:   Implementation of the analyze command for PeaZip context menu
 *
 * LICENSE:   LGPL-3
 *
 * DEVELOPER: Makoto Sakaguchi (ycco34vx@gmail.com)
 */

#include "pch.h"
#include "AnalyzeCommand.h"

import std;
import util.icon;

using namespace winrt;
using namespace Windows::ApplicationModel;

AnalyzeCommand::AnalyzeCommand() : ExplorerCommandBase(L"Analyze", g_analyze_icon_path, L"PEAUTILS 12 ")
{}

AnalyzeCommand::~AnalyzeCommand()
{}

const wchar_t* AnalyzeCommand::Application() const noexcept
{
    static const std::wstring app = L"pea.exe";
    return app.c_str();
}

