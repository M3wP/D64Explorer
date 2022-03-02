unit D64ExplorerTypes;

{$mode DELPHI}
{$H+}

interface

uses
	Classes, SysUtils, C64D64Image;

type
	TD64File = class(TObject)
	protected
    	FD64Image: TD64Image;
        FFileName: string;
        FDirty: Boolean;

	public
        property  FileName: string read FFilename write FFileName;
        property  D64Image: TD64Image read FD64Image write FD64Image;
        property  Dirty: Boolean read FDirty write FDirty;
	end;


implementation

end.

