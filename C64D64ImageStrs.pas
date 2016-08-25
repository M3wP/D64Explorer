//------------------------------------------------------------------------------
//C64D64ImageStrs
//===============
//Resource string definitions and helpers for C64D64Image.
//
//Copyright (C) 2016, Daniel England.
//All Rights Reserved.  Released under the GPL.
//
//This program is free software: you can redistribute it and/or modify it under
//the terms of the GNU General Public License as published by the Free Software
//Foundation, either version 3 of the License, or (at your option) any later
//version.
//
//This program is distributed in the hope that it will be useful, but WITHOUT
//ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
//FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
//details.
//
//You should have received a copy of the GNU General Public License along with
//this program.  If not, see <http://www.gnu.org/licenses/>.
//
//------------------------------------------------------------------------------
unit C64D64ImageStrs;

{$IFDEF FPC}
    {$MODE OBJFPC}
{$ENDIF}
{$H+}

interface


resourcestring
    STR_LBL_D64DOSINVALID = 'Invalid';
    STR_LBL_D64VERSION = 'Version';
    STR_LBL_D64YES = 'Yes';
    STR_LBL_D64NO = 'No';
    STR_LBL_D64TRUE = 'True';
    STR_LBL_D64FALSE = 'False';
    STR_LBL_D64GEOSVREND = 'VLIR End';
    STR_LBL_D64GEOSVRNON = 'VLIR Blank';

    STR_CAP_D64EXCEPTION = 'An Error Occurred.';

    STR_FMT_D64FORMATUNK = 'File: "%s" is an unknown format.';
    STR_FMT_D64EXTNSNUNK = 'File: "%s" has an invalid file extension.';
    STR_FMT_D64FILENOTEX = 'File: "%s" does not exist.';
    STR_FMT_D64SECOUTRNG = 'Track #%d does not contain %d sectors.';
    STR_FMT_D64TRKOUTRNG = 'Current image does not contain %d tracks.';
    STR_FMT_D64FILENOTOP = 'File: "%s" unable to be opened.';
    STR_ERR_D64DISKISFUL = 'The current disk image is full.';


function  ChooseString(const ACondition: Boolean): string; overload;
function  ChooseString(const ACondition: Boolean; const ATrueStr,
        AFalseStr: string): string; overload;


implementation

function  ChooseString(const ACondition: Boolean): string;
    begin
    if  ACondition then
        Result:= STR_LBL_D64TRUE
    else
        Result:= STR_LBL_D64FALSE;
    end;

function  ChooseString(const ACondition: Boolean;
            const ATrueStr, AFalseStr: string): string;
    begin
    if  ACondition then
        Result:= ATrueStr
    else
        Result:= AFalseStr;
    end;


end.

