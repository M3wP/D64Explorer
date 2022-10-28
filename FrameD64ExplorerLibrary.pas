unit FrameD64ExplorerLibrary;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
    IniFiles, FrameD64ExplorerTask, VirtualTrees;

type

    { TD64ExplorerLibraryFrame }

    TD64ExplorerLibraryFrame = class(TD64ExplorerTaskFrame)
        Label1: TLabel;
        Panel1: TPanel;
        Panel2: TPanel;
        VirtualStringTree1: TVirtualStringTree;
        procedure VirtualStringTree1DblClick(Sender: TObject);
        procedure VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
            Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
            var Ghosted: Boolean; var ImageIndex: Integer);
        procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree;
            Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
            var CellText: String);
    private
        FRecentUsed: TStringList;

        procedure RebuildRecentUsed;

    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;

        class function GetDescription: string; override;

        procedure Prepare; override;
        procedure Unprepare; override;
        procedure Initialise; override;

        procedure LoadData(const AIniFile: TIniFile); override;
        procedure SaveData(const AIniFile: TIniFile); override;

        function  AcceptFile(const AFile: string): Boolean; override;

        procedure AddRecentUsed(const AFileName: string);
    end;

implementation

{$R *.lfm}

uses
    DModD64ExplorerMain;

{ TD64ExplorerLibraryFrame }

procedure TD64ExplorerLibraryFrame.VirtualStringTree1GetText(
    	Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    	TextType: TVSTTextType; var CellText: String);
	begin
    CellText:= FRecentUsed[Node^.Index];
	end;

procedure TD64ExplorerLibraryFrame.VirtualStringTree1GetImageIndex(
    	Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
    	Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
	begin
    ImageIndex:= 0;
	end;

procedure TD64ExplorerLibraryFrame.VirtualStringTree1DblClick(Sender: TObject);
    var
    n: PVirtualNode;

    begin
    n:= VirtualStringTree1.GetFirst;

    while Assigned(n) do
    	begin
    	if  VirtualStringTree1.Selected[n] then
            begin
            D64ExplorerMainDMod.OpenDiskImageFile(FRecentUsed[n^.Index]);
            Exit;
            end;

        n:= VirtualStringTree1.GetNext(n);
        end;
    end;

procedure TD64ExplorerLibraryFrame.RebuildRecentUsed;
	begin
	D64ExplorerMainDMod.RequestSaveDataCallback(Self);

    VirtualStringTree1.RootNodeCount:= FRecentUsed.Count;
    VirtualStringTree1.Invalidate;
	end;

constructor TD64ExplorerLibraryFrame.Create(AOwner: TComponent);
	begin
    inherited Create(AOwner);

    FRecentUsed:= TStringList.Create;
    FRecentUsed.Duplicates:= dupIgnore;
    end;

destructor TD64ExplorerLibraryFrame.Destroy;
	begin
    FRecentUsed.Free;

    inherited Destroy;
	end;

class function TD64ExplorerLibraryFrame.GetDescription: string;
	begin
    Result:= 'Disk Librarian';
	end;

procedure TD64ExplorerLibraryFrame.Prepare;
	begin
    D64ExplorerMainDMod.EnableFileDrop(True);
	end;

procedure TD64ExplorerLibraryFrame.Unprepare;
	begin
    D64ExplorerMainDMod.EnableFileDrop(False);
	end;

procedure TD64ExplorerLibraryFrame.Initialise;
	begin
    RebuildRecentUsed;
	end;

procedure TD64ExplorerLibraryFrame.LoadData(const AIniFile: TIniFile);
    var
    i: Integer;
    c: Integer;
    s: string;

    begin
    inherited LoadData(AIniFile);

    c:= AIniFile.ReadInteger('RecentUsed', 'count', 0);

    for i:= 0 to c - 1 do
        begin
        s:= AIniFile.ReadString('RecentUsed', IntToStr(i), '');
        if  s = EmptyStr then
        	Continue;
    	FRecentUsed.Add(s);
        end;
    end;

procedure TD64ExplorerLibraryFrame.SaveData(const AIniFile: TIniFile);
    var
    i: Integer;
    c: Integer;
    s: string;

	begin
    AIniFile.WriteInteger('RecentUsed', 'count', FRecentUsed.Count);

    for i:= 0 to FRecentUsed.Count - 1 do
        AIniFile.WriteString('RecentUsed', IntToStr(i), FRecentUsed[i]);

    inherited SaveData(AIniFile);
    end;

function TD64ExplorerLibraryFrame.AcceptFile(const AFile: string): Boolean;
	begin
    D64ExplorerMainDMod.OpenDiskImageFile(AFile);

    Result:= False;
	end;

procedure TD64ExplorerLibraryFrame.AddRecentUsed(const AFileName: string);
    var
    i: Integer;

    begin
    i:= FRecentUsed.IndexOf(AFileName);

    if  i > -1 then
        FRecentUsed.Delete(i);

    FRecentUsed.Insert(0, AFileName);

    RebuildRecentUsed;
    end;

end.

