unit D64ExplorerStrs;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

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

