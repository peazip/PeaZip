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
  * PURPOSE:   Implementation of the "Browse path with PeaZip" command for PeaZip context menu
  *
  * LICENSE:   LGPL-3
  *
  * DEVELOPER: Makoto Sakaguchi (ycco34vx@gmail.com)
  */

#include "pch.h"
#include "BrowsePathCommand.h"

import util.icon;

BrowsePathCommand::BrowsePathCommand() :
    ExplorerCommandBase(L"BrowsePath", g_browse_path_icon_path, L"-ext2browsepath ")
{}

BrowsePathCommand::~BrowsePathCommand()
{}

HRESULT __stdcall BrowsePathCommand::GetCanonicalName(GUID* pguidCommandName) noexcept
{
    *pguidCommandName = __uuidof(this);
    return S_OK;
}
