unit FrameD64ExplorerManage;

{$mode Delphi}
{$H+}

interface

uses
    Classes, SysUtils, Graphics, Forms, Controls, ExtCtrls, Menus, ActnList,
    Buttons, IniFiles, VirtualTrees, C64D64Image, D64ExplorerTypes,
	FrameD64ExplorerTask;

type
    TEntryExtra = packed record
   		isDir: Boolean;
        fileSize: Cardinal;
    end;

    TEntryExtraArr = array of TEntryExtra;

    { TD64ExplorerManageFrame }

    TD64ExplorerManageFrame = class(TD64ExplorerTaskFrame)
        ActManageMvDn: TAction;
        ActManageMvUp: TAction;
		ActManageExport: TAction;
		ActManagePaste: TAction;
		ActManageCopy: TAction;
        ActManageAllPrg: TAction;
        ActViewToggleTree: TAction;
        ActViewToggleDirs: TAction;
        ActManageUpCase: TAction;
        ActManageScratch: TAction;
        ActManageImport: TAction;
        ActViewScratched: TAction;
        ActViewToggleChars: TAction;
        ActionList1: TActionList;
        Bevel1: TBevel;
        Bevel2: TBevel;
		Bevel3: TBevel;
        Bevel4: TBevel;
        Bevel5: TBevel;
        Bevel6: TBevel;
        Bevel7: TBevel;
        ImageList1: TImageList;
        MenuItem1: TMenuItem;
        MenuItem10: TMenuItem;
        MenuItem11: TMenuItem;
        MenuItem2: TMenuItem;
        MenuItem3: TMenuItem;
        MenuItem4: TMenuItem;
        MenuItem5: TMenuItem;
        MenuItem6: TMenuItem;
        MenuItem7: TMenuItem;
        MenuItem8: TMenuItem;
        MenuItem9: TMenuItem;
        MnuItmOne: TMenuItem;
        MnuItmTwo: TMenuItem;
        Panel1: TPanel;
        Panel2: TPanel;
        Panel3: TPanel;
        PopupMenu1: TPopupMenu;
        SpeedButton1: TSpeedButton;
		SpeedButton10: TSpeedButton;
        SpeedButton11: TSpeedButton;
        SpeedButton12: TSpeedButton;
        SpeedButton13: TSpeedButton;
        SpeedButton2: TSpeedButton;
        SpeedButton3: TSpeedButton;
        SpeedButton4: TSpeedButton;
        SpeedButton5: TSpeedButton;
        SpeedButton6: TSpeedButton;
        SpeedButton7: TSpeedButton;
        SpeedButton8: TSpeedButton;
		SpeedButton9: TSpeedButton;
        Splitter1: TSplitter;
        VirtualDrawTree1: TVirtualDrawTree;
        VirtualStringTree1: TVirtualStringTree;
		procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean
			);
        procedure ActManageAllPrgExecute(Sender: TObject);
		procedure ActManageCopyExecute(Sender: TObject);
		procedure ActManageCopyUpdate(Sender: TObject);
        procedure ActManageExportExecute(Sender: TObject);
        procedure ActManageExportUpdate(Sender: TObject);
        procedure ActManageImportExecute(Sender: TObject);
        procedure ActManageMvDnExecute(Sender: TObject);
        procedure ActManageMvDnUpdate(Sender: TObject);
        procedure ActManageMvUpExecute(Sender: TObject);
        procedure ActManageMvUpUpdate(Sender: TObject);
		procedure ActManagePasteExecute(Sender: TObject);
		procedure ActManagePasteUpdate(Sender: TObject);
        procedure ActManageScratchExecute(Sender: TObject);
        procedure ActManageScratchUpdate(Sender: TObject);
		procedure ActTestExecute(Sender: TObject);
		procedure ActTestUpdate(Sender: TObject);
        procedure ActViewScratchedExecute(Sender: TObject);
        procedure ActViewToggleCharsExecute(Sender: TObject);
        procedure ActViewToggleDirsExecute(Sender: TObject);
        procedure ActManageUpCaseExecute(Sender: TObject);
        procedure ActManageUpCaseUpdate(Sender: TObject);
        procedure ActViewToggleTreeExecute(Sender: TObject);
        procedure SpeedButton3Paint(Sender: TObject);
		procedure VirtualDrawTree1AdvancedHeaderDraw(Sender: TVTHeader;
			var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
		procedure VirtualDrawTree1BeforeCellPaint(Sender: TBaseVirtualTree;
			TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
			CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
		procedure VirtualDrawTree1BeforePaint(Sender: TBaseVirtualTree;
			TargetCanvas: TCanvas);
        procedure VirtualDrawTree1DrawNode(Sender: TBaseVirtualTree;
            const PaintInfo: TVTPaintInfo);
		procedure VirtualDrawTree1HeaderDrawQueryElements(Sender: TVTHeader;
			var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
        procedure VirtualStringTree1GetCellIsEmpty(Sender: TBaseVirtualTree;
            Node: PVirtualNode; Column: TColumnIndex; var IsEmpty: Boolean);
        procedure VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
            Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
            var Ghosted: Boolean; var ImageIndex: Integer);
        procedure VirtualStringTree1PaintText(Sender: TBaseVirtualTree;
            const TargetCanvas: TCanvas; Node: PVirtualNode;
            Column: TColumnIndex; TextType: TVSTTextType);
    private
        FEntries: TD64DirEntries;
        FExtras: TEntryExtraArr;
        FDirs: TD64DirPartitions;
        FAltSet: Boolean;
        FUpCase: Boolean;
        FShowDirs: Boolean;
        FScratched: Boolean;
        FTreeView: Boolean;
        FAllPRG: Boolean;
        FMnuItms,
        FMnuLocs: array of TMenuItem;

    protected
        procedure DoInitialiseFiles;
		procedure SaveOrReplaceFile(const dft: TD64FileType;
			const dfn: AnsiString; const AFile: TStream; const AName: string);
        procedure UpdateNodeVisibility;

    public
        class function GetDescription: string; override;

        procedure Prepare(const AD64File: TD64File); override;
        procedure Unprepare; override;

      	procedure Initialise; override;

        procedure LoadData(const AIniFile: TIniFile); override;
        procedure SaveData(const AIniFile: TIniFile); override;

        function  AcceptFile(const AFile: string): Boolean; override;

        property  AltSet: Boolean read FAltSet write FAltSet;
    end;

implementation

{$R *.lfm}

uses
    dbugintf, LCLType, Clipbrd, Dialogs,
    D64ExplorerConsts, D64ExplorerUtils, DModD64ExplorerMain;

var
	FClipFormat: TClipboardFormat;

{ TD64ExplorerManageFrame }

procedure TD64ExplorerManageFrame.VirtualDrawTree1DrawNode(
    	Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo);
	var
    i: Integer;
	c: TColor;
    n: Cardinal;
    ft: TD64FileType;
    fs: TD64FileStates;
    s: AnsiString;

	begin
	if  Sender.Selected[PaintInfo.Node] then
    	c:= clHighlightText
	else
    	c:= ARR_D64_CLR_IDX[dciListText0];

    c:= TColor(ColorToRGB(c));

    n:= PaintInfo.Node^.Index;

	PaintInfo.Canvas.Font.Color:= c;
//	c:= PaintInfo.Canvas.Font.Color;

	if  PaintInfo.Column = 0 then
    	begin
        if  FExtras[n].isDir then
            i:= 6
        else
          	i:= FEntries[n].FileType and $07;

        D64DecodeFileType(FEntries[n].FileType, ft, fs);

	    ImageList1.Draw(PaintInfo.Canvas, 0, 0, i);

	    PaintInfo.Canvas.TextOut(52 + 16 * 3 + 8, 24, 'Ent T/S/#: $' +
        		IntToHex(FEntries[n].Track, 2) + ' $' +
                IntToHex(FEntries[n].Sector, 2) + ' $' +
                IntToHex(FEntries[n].EntryNum, 2));

   		DrawC64Text(PaintInfo.Canvas, 52, 8, FEntries[n].FileName, c, True, True,
        		FAltSet);

        if  dfsReadOnly in fs then
            s:= '<'
        else
          	s:= ' ';

        if  dfsReplacing in fs then
            s:= s + '!'
        else
            s:= s + ' ';

        if  not (dfsClosed in fs) then
            s:= s + '*'
        else
            s:= s + ' ';

	   	DrawC64Text(PaintInfo.Canvas, 52, 28, s, c, True, False, False);
	    end
   	else if PaintInfo.Column = 1 then
    	begin
    	PaintInfo.Canvas.TextOut(PaintInfo.CellRect.Left, 8, 'Blocks: ' +
        		IntToStr(FEntries[n].FileSize));
	    PaintInfo.Canvas.TextOut(PaintInfo.CellRect.Left, 24, 'Bytes: ' +
        		IntToStr(FExtras[n].fileSize));
    	end
   	else if PaintInfo.Column = 2 then
    	begin
    	PaintInfo.Canvas.TextOut(PaintInfo.CellRect.Left, 8, 'Track: ' +
        		IntToStr(FEntries[n].DataTrack));
	    PaintInfo.Canvas.TextOut(PaintInfo.CellRect.Left, 24, 'Sector: ' +
        		IntToStr(FEntries[n].DataSector));
        end;
	end;

procedure TD64ExplorerManageFrame.VirtualDrawTree1HeaderDrawQueryElements(
		Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
		var Elements: THeaderPaintElements);
	begin
    Elements:= [hpeBackground, hpeText];
	end;

procedure TD64ExplorerManageFrame.VirtualStringTree1GetCellIsEmpty(
    	Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    	var IsEmpty: Boolean);
	begin
   	IsEmpty:= False;
	end;

procedure TD64ExplorerManageFrame.VirtualStringTree1GetImageIndex(
    	Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
    	Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
	begin
    if  VirtualStringTree1.AbsoluteIndex(Node) = 0 then
        ImageIndex:= 0
    else
        ImageIndex:= 1;
	end;

procedure TD64ExplorerManageFrame.VirtualStringTree1PaintText(
    	Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
    	Column: TColumnIndex; TextType: TVSTTextType);
    var
    s: AnsiString;
    c: TColor;
    x: Integer;
    i: Integer;

    begin
    x:= VirtualStringTree1.AbsoluteIndex(Node);

    if  x = 0 then
        s:= '"' + FDirs[x].PartDiskName + '" ' + FDirs[x].PartDiskID
    else
        s:= FDirs[x].PartFileName;

    if  vsSelected in Node^.States then
        c:= clHighlightText
    else
        c:= clWindowText;

    c:= TColor(ColorToRGB(c));

    i:= FDirs[x].Depth * 16 + 48 + 6;

    DrawC64Text(TargetCanvas, i, 10, s, c, False, True, FAltSet);
	end;

procedure TD64ExplorerManageFrame.ActViewToggleCharsExecute(Sender: TObject);
	begin
    FAltSet:= not FAltSet;

    ActViewToggleChars.Checked:= FAltSet;
    SpeedButton1.Down:= FAltSet;

    ActManageUpCase.Enabled:= not FAltSet;
    ActManageUpCase.Checked:= FUpCase;
//  SpeedButton5.Down:= FUpCase;

    VirtualDrawTree1.Repaint;
    VirtualStringTree1.Repaint;
	end;

procedure TD64ExplorerManageFrame.ActViewToggleDirsExecute(Sender: TObject);
	begin
    FShowDirs:= not FShowDirs;

    ActViewToggleDirs.Checked:= FShowDirs;
    SpeedButton6.Down:= FShowDirs;

    UpdateNodeVisibility;
	end;

procedure TD64ExplorerManageFrame.ActManageUpCaseExecute(Sender: TObject);
	begin
    FUpCase:= not FUpCase;

    ActManageUpCase.Checked:= FUpCase;
//  SpeedButton5.Down:= FUpCase;
	end;

procedure TD64ExplorerManageFrame.ActManageUpCaseUpdate(Sender: TObject);
	begin
    ActManageUpCase.Enabled:= not FAltSet;
	end;

procedure TD64ExplorerManageFrame.ActViewToggleTreeExecute(Sender: TObject);
	begin
    FTreeView:= not FTreeView;

    VirtualStringTree1.Visible:= FTreeView;
    Splitter1.Visible:= FTreeView;
    Splitter1.Left:= VirtualStringTree1.Width + 16;

    if  not FTreeView then
        VirtualDrawTree1.BorderSpacing.Left:= 8
    else
        VirtualDrawTree1.BorderSpacing.Left:= 0;

    ActViewToggleTree.Checked:= FTreeView;
    SpeedButton7.Down:= FTreeView;
	end;

procedure TD64ExplorerManageFrame.SpeedButton3Paint(Sender: TObject);
    var
    sb: TSpeedButton;

    begin
    sb:= Sender as TSpeedButton;

    if  sb.MouseInClient then
        begin
        sb.Canvas.GradientFill(sb.ClientRect,
        		ARR_D64_CLR_IDX[dciItmHotGrad0], ARR_D64_CLR_IDX[dciItmHotGrad1],
                gdVertical);

        if  sb.Down then
        	sb.Canvas.Pen.Color:= ARR_D64_CLR_IDX[dciItmActvGrad1]
        else
        	sb.Canvas.Pen.Color:= ARR_D64_CLR_IDX[dciItmHotGrad1];

        sb.Canvas.Pen.Style:= psSolid;
        sb.Canvas.Brush.Style:= bsClear;
        sb.Canvas.Rectangle(sb.ClientRect);
        end
    else if  sb.Down then
    	begin
        //if  sb.Enabled then
        //	sb.Canvas.Brush.Color:= clMenuHighlight
        //else
        //    sb.Canvas.Brush.Color:= clBtnShadow;
        //
        //sb.Canvas.Brush.Style:= bsSolid;
        //
        //sb.Canvas.FillRect(sb.ClientRect);

        sb.Canvas.GradientFill(sb.ClientRect,
        		ARR_D64_CLR_IDX[dciItmActvGrad0], ARR_D64_CLR_IDX[dciItmActvGrad1],
                gdVertical);

        sb.Canvas.Pen.Color:= ARR_D64_CLR_IDX[dciItmActvGrad1];
        sb.Canvas.Pen.Style:= psSolid;
        sb.Canvas.Brush.Style:= bsClear;
        sb.Canvas.Rectangle(sb.ClientRect);
        end;

    if sb.Down then
    	sb.Images.Draw(sb.Canvas, 3, 3, sb.ImageIndex, sb.Enabled)
    else if  sb.MouseInClient then
    	sb.Images.Draw(sb.Canvas, 2, 2, sb.ImageIndex, sb.Enabled);
    end;

procedure TD64ExplorerManageFrame.VirtualDrawTree1AdvancedHeaderDraw(
		Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
		const Elements: THeaderPaintElements);
    var
	ts: TTextStyle;

	begin
    Sender.Font.Color:= ARR_D64_CLR_IDX[dciHdrText0];
    Sender.Font.Style:= [fsBold];

    PaintInfo.TargetCanvas.Pen.Style:= psSolid;

	PaintInfo.TargetCanvas.GradientFill(Rect(0, 0,
			VirtualDrawTree1.ClientRect.Right,
			PaintInfo.PaintRectangle.Bottom),
			ARR_D64_CLR_IDX[dciHdrGrad0], ARR_D64_CLR_IDX[dciHdrGrad1],
            gdHorizontal);

	FillChar(ts, SizeOf(TTextStyle), 0);
	ts.Alignment:= taLeftJustify;
	ts.Layout:= tlCenter;
	ts.SingleLine:= True;
	ts.Clipping:= True;
	ts.ExpandTabs:= True;
	ts.Opaque:= False;
	ts.Wordbreak:= False;

//	PaintInfo.TargetCanvas.Font.Quality:= fqAntialiased;

    PaintInfo.TargetCanvas.TextRect(PaintInfo.PaintRectangle,
			Sender.Columns[0].Left + 8, 0, Sender.Columns[0].CaptionText, ts);
    PaintInfo.TargetCanvas.TextRect(PaintInfo.PaintRectangle,
			Sender.Columns[1].Left + 2, 0, Sender.Columns[1].CaptionText, ts);
    PaintInfo.TargetCanvas.TextRect(PaintInfo.PaintRectangle,
			Sender.Columns[2].Left + 2, 0, Sender.Columns[2].CaptionText, ts);

	PaintInfo.TargetCanvas.Pen.Color:= clBtnShadow;
	PaintInfo.TargetCanvas.Pen.Style:= psDot;

	PaintInfo.TargetCanvas.Line(Point(Sender.Columns[0].GetRect.Right - 1, 0),
			Point(Sender.Columns[0].GetRect.Right - 1, Sender.Columns[0].GetRect.Bottom));
	PaintInfo.TargetCanvas.Line(Point(Sender.Columns[1].GetRect.Right  - 1, 0),
			Point(Sender.Columns[1].GetRect.Right - 1, Sender.Columns[1].GetRect.Bottom));
	PaintInfo.TargetCanvas.Line(Point(Sender.Columns[2].GetRect.Right - 1, 0),
			Point(Sender.Columns[2].GetRect.Right - 1, Sender.Columns[2].GetRect.Bottom));
	end;

procedure TD64ExplorerManageFrame.VirtualDrawTree1BeforeCellPaint(
	Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
	Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
	var ContentRect: TRect);
begin

end;

procedure TD64ExplorerManageFrame.VirtualDrawTree1BeforePaint(
	Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
begin

end;


procedure TD64ExplorerManageFrame.ActViewScratchedExecute(Sender: TObject);
	begin
    FScratched:= not FScratched;

    ActViewScratched.Checked:= FScratched;
    SpeedButton2.Down:= FScratched;

    UpdateNodeVisibility;
	end;

procedure TD64ExplorerManageFrame.ActManageScratchExecute(Sender: TObject);
    var
    n: PVirtualNode;

    begin
    if  MessageDlg('Confirm delete', 'You are about to delete the selected files.  '+
    		'Are you sure you wish to continue?', mtWarning, [mbYes, mbNo], 0,
            mbNo) = mrNo then
        Exit;

    n:= VirtualDrawTree1.GetFirst;
    while Assigned(n) do
        begin
        if  vsSelected in n^.States then
            FD64File.D64Image.ScratchFileEntry(FEntries[n^.Index].Track,
            		FEntries[n^.Index].Sector, FEntries[n^.Index].EntryNum);

        n:= VirtualDrawTree1.GetNext(n);
        end;

    //D64ExplorerMainDMod.Dirty:= True;
    //D64ExplorerMainDMod.UpdateTasks;

	DoInitialiseFiles;
	UpdateNodeVisibility;

	FD64File.Dirty:= True;
	Update;
    end;

procedure TD64ExplorerManageFrame.ActManageImportExecute(Sender: TObject);
	begin
    if  D64ExplorerMainDMod.OpenDialog2.Execute then
        D64ExplorerMainDMod.AddDropFiles(D64ExplorerMainDMod.OpenDialog2.Files);
    end;

procedure TD64ExplorerManageFrame.ActManageMvDnExecute(Sender: TObject);
	var
	x: TEntryExtra;
	e: TD64DirEntry;

	i: Integer;
	n,
	p: PVirtualNode;

	begin
	i:= -1;
	n:= VirtualDrawTree1.GetFirst;
//	p:= n;
	while  Assigned(n) do
		begin
		if  VirtualDrawTree1.Selected[n] then
			begin
			i:= n^.Index;
            p:= VirtualDrawTree1.GetNext(n);
			Break;
			end;

		n:= VirtualDrawTree1.GetNext(n);
//		p:= n;
		end;

	Move(FEntries[i + 1].EntryData[2], e.EntryData[2], SizeOf(TD64EntryData) - 2);
	e.Track:= FEntries[i + 1].Track;
	e.Sector:= FEntries[i + 1].Sector;
//	e.EntryNum:= FEntries[i + 1].EntryNum;

	x.fileSize:= FExtras[i + 1].fileSize;
	x.isDir:= FExtras[i + 1].isDir;

	Move(FEntries[i].EntryData[2], FEntries[i + 1].EntryData[2], SizeOf(TD64EntryData) - 2);
	FEntries[i + 1].Track:= FEntries[i].Track;
	FEntries[i + 1].Sector:= FEntries[i].Sector;
//	FEntries[i + 1].EntryNum:= FEntries[i].EntryNum;

	FExtras[i + 1].fileSize:= FExtras[i].fileSize;
	FExtras[i + 1].isDir:= FExtras[i].isDir;

	Move(e.EntryData[2], FEntries[i].EntryData[2], SizeOf(TD64EntryData) - 2);
	FEntries[i].Track:= e.Track;
	FEntries[i].Sector:= e.Sector;
//	FEntries[i].EntryNum:= e.EntryNum;

	FExtras[i].fileSize:= x.fileSize;
	FExtras[i].isDir:= x.isDir;

	//e.EntryNum:= FEntries[i - 1].EntryNum;
	//FEntries[i + 1].EntryNum:= FEntries[i].EntryNum;
	//FEntries[i].EntryNum:= e.EntryNum;

	FD64File.D64Image.ReplaceFileEntry(FEntries[i + 1].Track,
			FEntries[i + 1].Sector, FEntries[i + 1].EntryNum,
			FEntries[i + 1].EntryData);

	FD64File.D64Image.ReplaceFileEntry(FEntries[i].Track,
			FEntries[i].Sector, FEntries[i].EntryNum,
			FEntries[i].EntryData);

	DoInitialiseFiles;

	VirtualDrawTree1.Selected[p]:= True;
	end;

procedure TD64ExplorerManageFrame.ActManageMvDnUpdate(Sender: TObject);
	var
	i: Integer;
	n: PVirtualNode;

	begin
	i:= -1;
	n:= VirtualDrawTree1.GetFirst;
	while  Assigned(n) do
		begin
		if  VirtualDrawTree1.Selected[n] then
			begin
			i:= n^.Index;
			Break;
			end;

		n:= VirtualDrawTree1.GetNext(n);
		end;

	ActManageMvDn.Enabled:= (VirtualDrawTree1.SelectedCount = 1) and
			(i < (VirtualDrawTree1.RootNodeCount - 1)) and FShowDirs and FScratched;
	end;

procedure TD64ExplorerManageFrame.ActManageMvUpExecute(Sender: TObject);
	var
	x: TEntryExtra;
	e: TD64DirEntry;

	i: Integer;
	n,
	p: PVirtualNode;

	begin
	i:= -1;
	p:= nil;
	n:= VirtualDrawTree1.GetFirst;
	while  Assigned(n) do
		begin
		if  VirtualDrawTree1.Selected[n] then
			begin
			i:= n^.Index;
			Break;
			end;

		p:= n;
		n:= VirtualDrawTree1.GetNext(n);
		end;

	Move(FEntries[i - 1].EntryData[2], e.EntryData[2], SizeOf(TD64EntryData) - 2);
	e.Track:= FEntries[i - 1].Track;
	e.Sector:= FEntries[i - 1].Sector;
//	e.EntryNum:= FEntries[i - 1].EntryNum;

	x.fileSize:= FExtras[i - 1].fileSize;
	x.isDir:= FExtras[i - 1].isDir;

	Move(FEntries[i].EntryData[2], FEntries[i - 1].EntryData[2], SizeOf(TD64EntryData) - 2);
	FEntries[i - 1].Track:= FEntries[i].Track;
	FEntries[i - 1].Sector:= FEntries[i].Sector;
//	FEntries[i - 1].EntryNum:= FEntries[i].EntryNum;

	FExtras[i - 1].fileSize:= FExtras[i].fileSize;
	FExtras[i - 1].isDir:= FExtras[i].isDir;

	Move(e.EntryData[2], FEntries[i].EntryData[2], SizeOf(TD64EntryData) - 2);
	FEntries[i].Track:= e.Track;
	FEntries[i].Sector:= e.Sector;
//	FEntries[i].EntryNum:= e.EntryNum;

	FExtras[i].fileSize:= x.fileSize;
	FExtras[i].isDir:= x.isDir;

	//e.EntryNum:= FEntries[i - 1].EntryNum;
	//FEntries[i - 1].EntryNum:= FEntries[i].EntryNum;
	//FEntries[i].EntryNum:= e.EntryNum;

	FD64File.D64Image.ReplaceFileEntry(FEntries[i - 1].Track,
			FEntries[i - 1].Sector, FEntries[i - 1].EntryNum,
			FEntries[i - 1].EntryData);

	FD64File.D64Image.ReplaceFileEntry(FEntries[i].Track,
			FEntries[i].Sector, FEntries[i].EntryNum,
			FEntries[i].EntryData);

	DoInitialiseFiles;

	VirtualDrawTree1.Selected[p]:= True;
	end;

procedure TD64ExplorerManageFrame.ActManageMvUpUpdate(Sender: TObject);
	var
	i: Integer;
	n: PVirtualNode;

	begin
	i:= -1;
	n:= VirtualDrawTree1.GetFirst;
	while  Assigned(n) do
		begin
		if  VirtualDrawTree1.Selected[n] then
			begin
			i:= n^.Index;
			Break;
			end;

		n:= VirtualDrawTree1.GetNext(n);
		end;

	ActManageMvUp.Enabled:= (VirtualDrawTree1.SelectedCount = 1) and (i > 0)
    		and FShowDirs and FScratched;
	end;

procedure TD64ExplorerManageFrame.ActManagePasteExecute(Sender: TObject);
    var
	f,
    b: TMemoryStream;
	fn: AnsiString;
	ft: Byte;
	nm: array[0..15] of AnsiChar;
    i,
    c: Word;
    z: Cardinal;

	begin
	try
    	f:= TMemoryStream.Create;
        try
			if  Clipboard.GetFormat(FClipFormat, f) then
				begin
				f.Position:= 0;
                c:= f.ReadWord;

                b:= TMemoryStream.Create;
                try
                    for i:= 0 to c - 1 do
                	    begin
					    ft:= f.ReadByte;
					    f.Read(nm[0], 16);
					    fn:= nm;

                        z:= f.ReadDWord;

                        b.Clear;

                        b.CopyFrom(f, z);
                        b.Position:= 0;

					    SaveOrReplaceFile(ft, fn, b, PetsciiToAsciiString(fn));
                	    end;

                	finally
                    b.Free;
                    end;

    			DoInitialiseFiles;
   				UpdateNodeVisibility;
                end;

			finally
			f.Free;
			end;
		except
       	MessageDlg('Error detected', 'Unable to write file.',
	       		mtError, [mbOk], 0);
		end;
	end;

procedure TD64ExplorerManageFrame.ActManagePasteUpdate(Sender: TObject);
	begin
	try
		if  not Prepared then
			Exit;

		if  D64ExplorerMainDMod.CurrD64File = -1 then
			Exit;

    	ActManagePaste.Enabled:= Clipboard.HasFormat(FClipFormat);

		except
		end;
	end;

procedure TD64ExplorerManageFrame.ActManageAllPrgExecute(Sender: TObject);
	begin
    FAllPRG:= not FAllPRG;

    ActManageAllPrg.Checked:= FAllPRG;
    SpeedButton8.Down:= FAllPRG;
	end;

procedure TD64ExplorerManageFrame.ActionList1Update(AAction: TBasicAction;
		var Handled: Boolean);
	begin
    if  not Prepared then
		Handled:= True;
	end;

procedure TD64ExplorerManageFrame.ActManageCopyExecute(Sender: TObject);
	var
    z: Cardinal;
    f,
	d: TMemoryStream;
    n: PVirtualNode;
    i: Integer;
	nm: array[0..15] of AnsiChar;

	begin
	f:= TMemoryStream.Create;
    try
        f.WriteWord(VirtualDrawTree1.SelectedCount);

//	    i:= -1;
        n:= VirtualDrawTree1.GetFirst;
        while Assigned(n) do
            begin
            if  vsSelected in n^.States then
                begin
                i:= n^.Index;

    		    d:= TMemoryStream.Create;
    		    try
    			    f.WriteByte(FEntries[i].FileType);
    			    nm:= Copy(FEntries[i].FileName + StringOfChar(#$A0, 16), 1, 16);
    			    f.WriteBuffer(nm[0], 16);

    			    FD64File.D64Image.GetDataChain(FEntries[i].DataTrack,
    					    FEntries[i].DataSector, d, z);

                    f.WriteDWord(z);

    			    d.Position:= 0;
    			    f.CopyFrom(d, z);

    			    finally
    			    d.Free;
    			    end;
                end;

            n:= VirtualDrawTree1.GetNext(n);
            end;

        f.Position:= 0;
	    Clipboard.AddFormat(FClipFormat, f);

    	finally
	    f.Free;
        end;
    end;

procedure TD64ExplorerManageFrame.ActManageCopyUpdate(Sender: TObject);
	var
    ft: TD64FileType;
    fs: TD64FileStates;
	i: Integer;
	n: PVirtualNode;

	begin
	try
		if  not Prepared then
			Exit;

	    ActManageCopy.Enabled:= False;

		if  Length(FEntries) = 0 then
			Exit;

		if  VirtualDrawTree1.SelectedCount = 0 then
			Exit;

	    n:= VirtualDrawTree1.GetFirst;
	    while Assigned(n) do
	        begin
	        if  vsSelected in n^.States then
	            begin
	            i:= n^.Index;

		        D64DecodeFileType(FEntries[i].FileType, ft, fs);

	    	    if  not ((ft in [VAL_TYP_D64FTYPE_SEQ..VAL_TYP_D64FTYPE_USR])
	        	and (fs = [dfsClosed])) then
	        		Exit;
                end;

	        n:= VirtualDrawTree1.GetNext(n);
	        end;

	    ActManageCopy.Enabled:= True;

    	except
		end;
	end;

procedure TD64ExplorerManageFrame.ActManageExportExecute(Sender: TObject);
    var
    f: TMemoryStream;
    sz: Cardinal;
    i: Integer;
	n: PVirtualNode;

    begin
	i:= -1;
    n:= VirtualDrawTree1.GetFirst;
    while Assigned(n) do
        begin
        if  vsSelected in n^.States then
            begin
            i:= n^.Index;
			Break;
            end;

        n:= VirtualDrawTree1.GetNext(n);
        end;

    if  i = -1 then
        Exit;

    D64ExplorerMainDMod.SaveDialog1.Title:= 'Export File...';
    D64ExplorerMainDMod.SaveDialog1.Filter:= 'All Files (*.*)|*.*';
    D64ExplorerMainDMod.SaveDialog1.DefaultExt:= '';
    if  D64ExplorerMainDMod.SaveDialog1.Execute then
        begin
        f:= TMemoryStream.Create;
        try
		    FD64File.D64Image.GetDataChain(
        		    FEntries[i].EntryData[$03],
                    FEntries[i].EntryData[$04], f, sz);

            f.Position:= 0;
            f.SetSize(sz);
            f.SaveToFile(D64ExplorerMainDMod.SaveDialog1.FileName);

        	finally
            f.Free;
            end;
        end;
    end;

procedure TD64ExplorerManageFrame.ActManageExportUpdate(Sender: TObject);
	var
    i: Integer;
    n: PVirtualNode;

    begin
	if  not Prepared then
		Exit;

    ActManageExport.Enabled:= False;

	if  Length(FEntries) = 0 then
		Exit;

    if  (VirtualDrawTree1.SelectedCount = 0)
    or  (VirtualDrawTree1.SelectedCount > 1) then
        Exit;

    i:= -1;
    n:= VirtualDrawTree1.GetFirst;
    while Assigned(n) do
        begin
        if  vsSelected in n^.States then
            begin
            i:= n^.Index;
		    Break;
            end;

        n:= VirtualDrawTree1.GetNext(n);
        end;

    if  i = -1 then
        Exit;

	if  FD64File.D64Image.GEOSDisk
    and ((FEntries[i].FileType and $07) in [1..3])
    and (FEntries[i].EntryData[$18] <> 0) then
        Exit;

    if  (FEntries[i].EntryData[$03] <> 0)
    and ((FEntries[i].FileType and $7) > 0)
    and ((FEntries[i].FileType and $7) <> 5) then
    	ActManageExport.Enabled:= True;
	end;

procedure TD64ExplorerManageFrame.ActManageScratchUpdate(Sender: TObject);
	begin
	if  not Prepared then
		Exit;

	try
    	ActManageScratch.Enabled:= VirtualDrawTree1.SelectedCount > 0;

		except
		end;
	end;

procedure TD64ExplorerManageFrame.ActTestExecute(Sender: TObject);
	begin
    Color:= clRed;
	end;

procedure TD64ExplorerManageFrame.ActTestUpdate(Sender: TObject);
	begin
    ActManageExport.Enabled:= Clipboard.HasFormat(FClipFormat);
	end;

procedure TD64ExplorerManageFrame.DoInitialiseFiles;
    var
    i: Integer;
    z,
    p: Cardinal;
    f: TMemoryStream;
    ft: TD64FileType;
    fs: TD64FileStates;

    procedure BuildDirTree;
        var
        i: Integer;
        p: array of PVirtualNode;
        n: PVirtualNode;

        begin
        n:= nil;

		VirtualStringTree1.RootNodeCount:= 0;

        SetLength(p, Length(FDirs));
        p[0]:= nil;

        for i:= 0 to Length(FDirs) - 1 do
            begin
            if  FDirs[i].Parent < 0 then
                n:= nil
            else
            	n:= p[FDirs[i].Parent];

            n:= VirtualStringTree1.AddChild(n);
            p[i]:= n;
            end;
        end;

    begin
	VirtualDrawTree1.RootNodeCount:= 0;

    if  FD64File.D64Image.DiskType = ddt1581 then
        FD64File.D64Image.GetPartitionFiles(FEntries)
    else
        FD64File.D64Image.GetFileEntries(FEntries);

    f:= TMemoryStream.Create;
    try
        SetLength(FExtras, Length(FEntries));
        for i:= 0 to Length(FEntries) - 1 do
            begin
            f.Clear;

            FExtras[i].isDir:= FD64File.D64Image.IsDirectoryPartition(
        		    FEntries[i], p);

            D64DecodeFileType(FEntries[i].FileType, ft, fs);

            if  (ft in [VAL_TYP_D64FTYPE_SEQ..VAL_TYP_D64FTYPE_USR])
            and (fs = [dfsClosed]) then
            	FD64File.D64Image.GetDataChain(FEntries[i].DataTrack,
            			FEntries[i].DataSector, f, z)
            else
                z:= 0;

            FExtras[i].fileSize:= z;
            end;

    	finally
        f.Free;
        end;

    FD64File.D64Image.GetDirPartitions(FDirs);

    VirtualDrawTree1.RootNodeCount:= Length(FEntries);

    BuildDirTree;
    VirtualStringTree1.FullExpand;
	end;


procedure TD64ExplorerManageFrame.SaveOrReplaceFile(const dft: TD64FileType;
		const dfn: AnsiString; const AFile: TStream; const AName: string);
	var
	i: Integer;
	den: TD64DirEntry;
	blk: Word;
	ssc: TD64SectorNum;
	stk: TD64TrackNum;

	begin
    for i:= 0 to Length(FEntries) - 1 do
        if  ((FEntries[i].FileType and $0F) = (dft and $0F))
        and (CompareStr(dfn, FEntries[i].FileName) = 0) then
            begin
            if  MessageDlg('Replace file', 'The file "' + ExtractFileName(
				AName) +
        		    '" already exists.  Should it be replaced?', mtConfirmation,
                    [mbYes, mbNo], 0, mbNo) = mrNo then
         	    Exit;

            FD64File.D64Image.ScratchFileEntry(FEntries[i].Track,
        		    FEntries[i].Sector, FEntries[i].EntryNum);

            Break;
            end;

   	FD64File.D64Image.AllocateDiskSectors(AFile, stk, ssc, blk);

	FillChar(den.EntryData[0], SizeOf(TD64EntryData), $00);
    den.SetFileName(dfn);
    den.SetFileType(dft);
	den.SetDataTS(stk, ssc);
    den.SetFileSize(blk);

    FD64File.D64Image.AllocateFileEntry(den.EntryData);
    FD64File.Dirty:= True;
	end;

procedure TD64ExplorerManageFrame.UpdateNodeVisibility;
    var
    n: PVirtualNode;

    begin
    n:= VirtualDrawTree1.GetFirst;
    while Assigned(n) do
		begin
        if  (not FScratched)
        and ((FEntries[n^.Index].FileType and $07) = 0) then
            Exclude(n^.States, vsVisible)
        else if (not FShowDirs)
        and FExtras[n^.Index].isDir then
        	Exclude(n^.States, vsVisible)
        else
            Include(n^.States, vsVisible);

        n:= VirtualDrawTree1.GetNext(n);
        end;

   	VirtualDrawTree1.Invalidate;
    end;

class function TD64ExplorerManageFrame.GetDescription: string;
	begin
    Result:= 'File Manager';
	end;

procedure TD64ExplorerManageFrame.Prepare(const AD64File: TD64File);
    var
    i: Integer;

    begin
    FD64File:= AD64File;

	Panel3.Visible:= True;
	D64ExplorerMainDMod.BindToCoolbar(Panel3, False);
	D64ExplorerMainDMod.EnableFileDrop(True);
 //
 // 	SetLength(FMnuItms, PopupMenu1.Items[0].Count + PopupMenu1.Items[1].Count);
 //
 //   for i:= 0 to PopupMenu1.Items[0].Count - 1 do
 //     	FMnuItms[i]:= PopupMenu1.Items[0].Items[i];
 //
 //   for i:= 0 to PopupMenu1.Items[1].Count - 1 do
 //     	FMnuItms[PopupMenu1.Items[0].Count + i]:= PopupMenu1.Items[1].Items[i];
//
//	D64ExplorerMainDMod.MoveMenuItems(FMnuItms, FMnuLocs);

	inherited;
    end;

procedure TD64ExplorerManageFrame.Unprepare;
	begin
	if  not Prepared then
		Exit;

	D64ExplorerMainDMod.UnbindToCoolbar(Panel3, Panel1);
	Panel3.Parent:= Panel1;
	Panel3.Visible:= False;

    D64ExplorerMainDMod.EnableFileDrop(False);

//	D64ExplorerMainDMod.RetrieveMenuItems(FMnuItms, FMnuLocs, PopupMenu1);


	inherited;
	end;

procedure TD64ExplorerManageFrame.Initialise;
	begin
    ActManageUpCase.Checked:= FUpCase;
    ActManageAllPrg.Checked:= FAllPRG;

    ActViewToggleChars.Checked:= FAltSet;

	ActViewToggleTree.Checked:= FTreeView;
    ActViewScratched.Checked:= FScratched;
    ActViewToggleDirs.Checked:= FShowDirs;

    DoInitialiseFiles;

    UpdateNodeVisibility;
	end;

procedure TD64ExplorerManageFrame.LoadData(const AIniFile: TIniFile);
	begin
    inherited LoadData(AIniFile);

    FAltSet:= AIniFile.ReadBool('Manager', 'AltSet', False);
    FUpCase:= AIniFile.ReadBool('Manager', 'UpCase', True);
    FShowDirs:= AIniFile.ReadBool('Manager', 'ShowDirs', False);
    FScratched:= AIniFile.ReadBool('Manager', 'Scratched', False);
    FTreeView:= AIniFile.ReadBool('Manager', 'TreeView', True);
    FAllPRG:= AIniFile.ReadBool('Manager', 'AllPRG', False);

    VirtualStringTree1.Width:= AIniFile.ReadInteger('Manager', 'TreeWidth', 240);
    end;

procedure TD64ExplorerManageFrame.SaveData(const AIniFile: TIniFile);
	begin
    AIniFile.WriteBool('Manager', 'AltSet', FAltSet);
    AIniFile.WriteBool('Manager', 'UpCase', FUpCase);
    AIniFile.WriteBool('Manager', 'ShowDirs', FShowDirs);
    AIniFile.WriteBool('Manager', 'Scratched', FScratched);
    AIniFile.WriteBool('Manager', 'TreeView', FTreeView);
    AIniFile.WriteBool('Manager', 'AllPRG', FAllPRG);

    AIniFile.WriteInteger('Manager', 'TreeWidth', VirtualStringTree1.Width);

    inherited SaveData(AIniFile);
	end;

function TD64ExplorerManageFrame.AcceptFile(const AFile: string): Boolean;
    var
    fst: TFileStream;
    ifn: string;
    ift: string;

    pfn: AnsiString;
    dfn: AnsiString;
    dft: TD64FileType;

	begin
    Result:= True;

    if  not Assigned(FD64File.D64Image) then
        begin
        Result:= False;
        Exit;
        end;

    if  DirectoryExists(AFile) then
    	MessageDlg('Unsupported function', 'Directories cannot be processed at '+
        		'this time.  Please select individual files.', mtInformation,
                [mbOk], 0)
    else if  not FileExists(AFile) then
    	begin
    	MessageDlg('Error detected', 'Could not process the file.  Terminating.',
        		mtError, [mbOk], 0);
       	Result:= False;
        Exit;
        end
   	else
    	begin
        if  (not FAltSet)
        and FUpCase then
            begin
            ifn:= UpperCase(ExtractFileName(AFile));
            ift:= UpperCase(ExtractFileExt(AFile));
            end
       	else
        	begin
            ifn:= ExtractFileName(AFile);
            ift:= ExtractFileExt(AFile);
            end;

        if  CompareText(ift, '.prg') = 0 then
            begin
            ifn:= ChangeFileExt(ifn, '');
            dft:= D64EncodeFileType(VAL_TYP_D64FTYPE_PRG, [dfsClosed]);
            end
        else if CompareText(ift, '.seq') = 0 then
            begin
            ifn:= ChangeFileExt(ifn, '');
            dft:= D64EncodeFileType(VAL_TYP_D64FTYPE_SEQ, [dfsClosed]);
            end
        else if FAllPRG then
            dft:= D64EncodeFileType(VAL_TYP_D64FTYPE_PRG, [dfsClosed])
        else
            dft:= D64EncodeFileType(VAL_TYP_D64FTYPE_SEQ, [dfsClosed]);

        pfn:= AsciiToPetsciiString(Copy(ifn, 1, 16), FAltSet);

        if  Length(pfn) < 16 then
       		dfn:= pfn + Copy(LIT_LBL_D64BLNKFLN, 1, 16 - Length(pfn));

		try
			fst:= TFileStream.Create(AFile, fmOpenRead);
			try
				SaveOrReplaceFile(dft, dfn, fst, AFile);

				finally
				fst.Free;
				end;

			except
        	MessageDlg('Error detected', 'Unable to write file.  Terminating.',
            		mtError, [mbOk], 0);
            Result:= False;
            end;
        end;
    end;

initialization
    FClipFormat:= RegisterClipboardFormat('D64EXPLORER.FILES.STREAM');


end.

