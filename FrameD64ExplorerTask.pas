unit FrameD64ExplorerTask;

{$mode delphi}{$H+}

interface

uses
    Generics.Collections, Classes, SysUtils, Forms, Controls, IniFiles,
	D64ExplorerTypes;

type

    { TD64ExplorerTaskFrame }

    TD64ExplorerTaskFrame = class(TFrame)
    private
    	FPrepared: Boolean;

	protected
		FD64File: TD64File;

    public
    	class function GetDescription: string; virtual; abstract;

    	procedure Prepare(const AD64File: TD64File); virtual;
	    procedure Unprepare; virtual;

	    procedure Initialise; virtual; abstract;

        procedure LoadData(const AIniFile: TIniFile); virtual;
        procedure SaveData(const AIniFile: TIniFile); virtual;
        procedure UpdateDisplay; virtual;

        function  AcceptFile(const AFile: string): Boolean; virtual;

		property  D64File: TD64File read FD64File;
		property  Prepared: Boolean read FPrepared;
    end;

    TD64ExplorerTaskFrameClass = class of TD64ExplorerTaskFrame;

	TTaskFramesList = TObjectList<TD64ExplorerTaskFrame>;

implementation

{$R *.lfm}

{ TD64ExplorerTaskFrame }

procedure TD64ExplorerTaskFrame.Prepare(const AD64File: TD64File);
	begin
    FPrepared:= True;
	end;

procedure TD64ExplorerTaskFrame.Unprepare;
	begin
    FPrepared:= False;
	end;

procedure TD64ExplorerTaskFrame.LoadData(const AIniFile: TIniFile);
	begin

	end;

procedure TD64ExplorerTaskFrame.SaveData(const AIniFile: TIniFile);
	begin

	end;

procedure TD64ExplorerTaskFrame.UpdateDisplay;
	begin
    Initialise;
	end;

function TD64ExplorerTaskFrame.AcceptFile(const AFile: string): Boolean;
	begin
    Result:= False;
	end;

end.

