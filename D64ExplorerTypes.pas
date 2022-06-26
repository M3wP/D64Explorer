unit D64ExplorerTypes;

{$mode DELPHI}
{$H+}

interface

uses
	Classes, SysUtils, C64D64Image;

type
{ 	TD64File }

  	TD64File = class(TObject)
	protected
    	FD64Image: TD64Image;
        FFileName: string;
        FDirty: Boolean;
        FOnDirtyChange: TNotifyEvent;

        property  OnDirtyChange: TNotifyEvent read FOnDirtyChange write FOnDirtyChange;

    private
		procedure SetDirty(const AValue: Boolean);

	public
        property  FileName: string read FFilename write FFileName;
        property  D64Image: TD64Image read FD64Image write FD64Image;
        property  Dirty: Boolean read FDirty write SetDirty;
	end;


implementation

{ TD64File }

procedure TD64File.SetDirty(const AValue: Boolean);
	begin
    if  AValue <> FDirty then
        begin
    	FDirty:= AValue;

        if Assigned(FOnDirtyChange) then
        	FOnDirtyChange(Self);
        end;
    end;

end.

