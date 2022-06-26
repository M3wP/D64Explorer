unit FrameD64ExplorerLibrary;

{$mode Delphi}
{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
    IniFiles, D64ExplorerTypes, FrameD64ExplorerTask, VirtualTrees;

type

    { TD64ExplorerLibraryFrame }

    TD64ExplorerLibraryFrame = class(TD64ExplorerTaskFrame)
		Label1: TLabel;
		PaintBox1: TPaintBox;
        Panel1: TPanel;
        Panel2: TPanel;
		Panel3: TPanel;
        VirtualStringTree1: TVirtualStringTree;
		procedure PaintBox1Paint(Sender: TObject);
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

        procedure Prepare(const AD64File: TD64File); override;
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
    D64ExplorerConsts, DModD64ExplorerMain;

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

procedure TD64ExplorerLibraryFrame.PaintBox1Paint(Sender: TObject);
	begin
    PaintBox1.Canvas.GradientFill(PaintBox1.ClientRect,
    		ARR_D64_CLR_IDX[dciHdrGrad0], ARR_D64_CLR_IDX[dciHdrGrad1],
            gdHorizontal);

    PaintBox1.Canvas.Brush.Color:= ARR_D64_CLR_IDX[dciHdrGrad1];
    PaintBox1.Canvas.FillRect(Rect(0, 0, 8, PaintBox1.ClientRect.Bottom));

    Label1.Font.Color:= ARR_D64_CLR_IDX[dciHdrText0];
    Label1.Font.Style:= [fsBold];
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

procedure TD64ExplorerLibraryFrame.Prepare(const AD64File: TD64File);
	begin
    D64ExplorerMainDMod.EnableFileDrop(True);

    inherited;
	end;

procedure TD64ExplorerLibraryFrame.Unprepare;
	begin
	if  not Prepared then
		Exit;

    D64ExplorerMainDMod.EnableFileDrop(False);

	inherited;
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

