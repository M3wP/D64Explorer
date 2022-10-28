unit FrameD64ExplorerTask;

{$mode delphi}{$H+}

interface

uses
    Generics.Collections, Classes, SysUtils, Forms, Controls, IniFiles;

type

    { TD64ExplorerTaskFrame }

    TD64ExplorerTaskFrame = class(TFrame)
    private

    public
    	class function GetDescription: string; virtual; abstract;

    	procedure Prepare; virtual; abstract;
	    procedure Unprepare; virtual; abstract;

	    procedure Initialise; virtual; abstract;

        procedure LoadData(const AIniFile: TIniFile); virtual;
        procedure SaveData(const AIniFile: TIniFile); virtual;
        procedure UpdateDisplay; virtual;

        function  AcceptFile(const AFile: string): Boolean; virtual;
    end;

    TD64ExplorerTaskFrameClass = class of TD64ExplorerTaskFrame;

	TTaskFramesList = TList<TD64ExplorerTaskFrame>;

implementation

{$R *.lfm}

{ TD64ExplorerTaskFrame }

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

