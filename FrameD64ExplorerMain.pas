//------------------------------------------------------------------------------
//FrameD64ExplorerMain
//====================
//Frame for the main display in the D64 Explorer application.  Created and
//managed by the Main Data Module, displays information about the currently
//opened disk image and its files.
//
//Please note:
//------------
//Presently, only FPC/Lazarus is supported.  Delphi support is incomplete.
//Needs a class helper for TMemoryStream to include ReadByte (which I'm a fan
//of) or ReadByte changed to ReadBuffer.  Additional Lazarus controls are also
//used.
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
unit FrameD64ExplorerMain;

{$mode Delphi}
{$H+}

interface

uses
    Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, StdCtrls,
    ExtCtrls, ComCtrls, D64ExplorerTypes, C64D64Image, Types,
	FrameD64ExplorerTask;

type

{ TD64ExplorerMainFrame }

    TD64ExplorerMainFrame = class(TD64ExplorerTaskFrame)
        Button1: TButton;
        Button2: TButton;
        Button3: TButton;
        CmbDirectory: TComboBox;
        CmbGEOSVLIRRec: TComboBox;
        DividerBevel1: TDividerBevel;
        DividerBevel2: TDividerBevel;
        ImgGEOSIcon: TImage;
        Label1: TLabel;
        Label10: TLabel;
        Label11: TLabel;
        Label12: TLabel;
        Label13: TLabel;
        Label14: TLabel;
        Label15: TLabel;
        Label16: TLabel;
        Label17: TLabel;
        Label18: TLabel;
        Label19: TLabel;
        Label2: TLabel;
        Label20: TLabel;
        Label21: TLabel;
        Label22: TLabel;
        Label23: TLabel;
        Label24: TLabel;
        Label25: TLabel;
        Label26: TLabel;
        Label27: TLabel;
        Label28: TLabel;
        Label29: TLabel;
        Label3: TLabel;
        Label30: TLabel;
        Label31: TLabel;
        Label32: TLabel;
        Label33: TLabel;
        Label34: TLabel;
        Label35: TLabel;
        Label36: TLabel;
        Label37: TLabel;
        Label38: TLabel;
        Label39: TLabel;
        Label4: TLabel;
		Label40: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        Label7: TLabel;
        Label8: TLabel;
        Label9: TLabel;
        LblDiskID: TLabel;
        LblDiskName: TLabel;
        LblDOSType: TLabel;
        LblDOSVersion: TLabel;
        LblFileActual: TLabel;
        LblFileClosed: TLabel;
        LblFileData: TLabel;
        LblFileLocked: TLabel;
        LblFileSize: TLabel;
        LblFileType: TLabel;
        LblGEOS4080: TLabel;
        LblGEOSAppData: TLabel;
        LblGEOSAuthor: TLabel;
        LblGEOSC64Type: TLabel;
        LblGEOSClass: TLabel;
        LblGEOSDate: TLabel;
        LblGEOSDesc: TLabel;
        LblGEOSEnd: TLabel;
        LblGEOSFile: TLabel;
        LblGEOSIDBytes: TLabel;
        LblGEOSInfoLoc: TLabel;
        LblGEOSInit: TLabel;
        LblGEOSLoad: TLabel;
        LblGEOSParent: TLabel;
        LblGEOSStruct: TLabel;
        LblGEOSStructInf: TLabel;
        LblGEOSType: TLabel;
        LblGEOSTypeInf: TLabel;
        LblGEOSVer: TLabel;
        LblGEOSVLIRActual: TLabel;
        LblNumTracks: TLabel;
        LblReplacing: TLabel;
        LstBxFileData: TListBox;
        LstBxFiles: TListBox;
        LstBxGEOSData: TListBox;
        LstBxGEOSVLIRRec: TListBox;
		PaintBox1: TPaintBox;
        Panel1: TPanel;
        Panel2: TPanel;
		Panel3: TPanel;
        PgCtrlFile: TPageControl;
        PnlDetails: TPanel;
        PnlDirectory: TPanel;
        Splitter1: TSplitter;
        TbShtC64File: TTabSheet;
        TbShtC64REL: TTabSheet;
        TbShtGEOSInfo: TTabSheet;
        TbShtGEOSVLIR: TTabSheet;
        procedure Button1Click(Sender: TObject);
        procedure Button2Click(Sender: TObject);
        procedure Button3Click(Sender: TObject);
        procedure CmbDirectoryChange(Sender: TObject);
        procedure CmbDirectoryDrawItem(Control: TWinControl; Index: Integer;
            ARect: TRect; State: TOwnerDrawState);
        procedure CmbGEOSVLIRRecChange(Sender: TObject);
        procedure LstBxFilesSelectionChange(Sender: TObject; User: boolean);
		procedure PaintBox1Paint(Sender: TObject);
		procedure PnlDetailsPaint(Sender: TObject);
    private
        FChanging: Boolean;
        FEntries: TD64DirEntries;
        FDirectories: TD64DirPartitions;

        FVLIRRecs: array of Word;

        procedure DoInitialiseFiles;

        procedure ClearGEOSInfo;

        procedure DoPrepareFileInfo(const AEntry: Integer;
                const AStream: TStream; out AGEOSFile: Boolean);
        procedure DoPrepareFileData(const AEntry: Integer;
                const AStream: TMemoryStream);
        procedure DoPrepareFileREL(const AEntry: Integer;
                const AStream: TMemoryStream);
        procedure DoPrepareGEOSInfo(const AEntry: Integer;
                const AStream: TMemoryStream);
        procedure DoPrepareGEOSVLIR(const AEntry: Integer;
                const AStream: TMemoryStream);
        procedure DoPrepareGEOSVLIRRec(const ARecord: Integer);

        procedure InitialiseFileView(const AEntry: Integer);

    public
        class function GetDescription: string; override;

        procedure Prepare(const AD64File: TD64File); override;
        procedure Unprepare; override;
        procedure Initialise; override;
    end;

implementation

{$R *.lfm}

uses
    DateUtils, Graphics, Clipbrd,
    D64ExplorerConsts, D64ExplorerStrs, D64ExplorerUtils,
    DModD64ExplorerMain;


{ TD64ExplorerMainFrame }

procedure TD64ExplorerMainFrame.LstBxFilesSelectionChange(Sender: TObject;
        User: boolean);
    var
    i: Integer;

    begin
    if not FChanging then
        for i:= 0 to LstBxFiles.Items.Count - 1 do
            if  LstBxFiles.Selected[i] then
                begin
                InitialiseFileView(i);
                Break;
                end;
    end;

procedure TD64ExplorerMainFrame.PaintBox1Paint(Sender: TObject);
	begin
    PaintBox1.Canvas.GradientFill(PaintBox1.ClientRect,
    		ARR_D64_CLR_IDX[dciHdrGrad0], ARR_D64_CLR_IDX[dciHdrGrad1],
            gdHorizontal);

    PaintBox1.Canvas.Brush.Color:= ARR_D64_CLR_IDX[dciHdrGrad1];
    PaintBox1.Canvas.FillRect(Rect(0, 0, 8, PaintBox1.ClientRect.Bottom));

    Label40.Font.Color:= ARR_D64_CLR_IDX[dciHdrText0];
    Label40.Font.Style:= [fsBold];
    end;

procedure TD64ExplorerMainFrame.PnlDetailsPaint(Sender: TObject);
    begin
    //PnlDetails.Canvas.GradientFill(PnlDetails.ClientRect,
    //		ARR_D64_CLR_IDX[dciOptsGrad0], ARR_D64_CLR_IDX[dciOptsGrad1],
    //        gdHorizontal);
	end;

procedure TD64ExplorerMainFrame.CmbGEOSVLIRRecChange(Sender: TObject);
    begin
    if  not FChanging then
        DoPrepareGEOSVLIRRec(CmbGEOSVLIRRec.ItemIndex);
    end;

procedure TD64ExplorerMainFrame.Button1Click(Sender: TObject);
    begin
    Clipboard.AsText:= LstBxFileData.Items.Text;
    end;

procedure TD64ExplorerMainFrame.Button2Click(Sender: TObject);
    begin
    Clipboard.AsText:= LstBxGEOSData.Items.Text;
    end;

procedure TD64ExplorerMainFrame.Button3Click(Sender: TObject);
    begin
    Clipboard.AsText:= LstBxGEOSVLIRRec.Items.Text;
    end;

procedure TD64ExplorerMainFrame.CmbDirectoryChange(Sender: TObject);
    begin
    if  not FChanging then
        begin
        FD64File.D64Image.SetCurrentPartition(
                FDirectories[CmbDirectory.ItemIndex].Info);
        DoInitialiseFiles;
        end;
    end;

procedure TD64ExplorerMainFrame.CmbDirectoryDrawItem(Control: TWinControl;
        Index: Integer; ARect: TRect; State: TOwnerDrawState);
    begin
    DirectoryDrawComboItem(CmbDirectory, Index, ARect, State, FDirectories);
    end;

procedure TD64ExplorerMainFrame.DoInitialiseFiles;
    var
    i: Integer;

    begin
    if  FD64File.D64Image.DiskType = ddt1581 then
        FD64File.D64Image.GetPartitionFiles(FEntries)
    else
        FD64File.D64Image.GetFileEntries(FEntries);

    FChanging:= True;
    try
        LstBxFiles.Items.BeginUpdate;
        try
            LstBxFiles.Clear;

            for i:= 0 to High(FEntries) do
                LstBxFiles.Items.Add(PetsciiToAsciiString(FEntries[i].FileName));

            finally
            LstBxFiles.Items.EndUpdate;
            end;
        finally
        FChanging:= False;
        end;

    if  LstBxFiles.Items.Count > 0 then
        begin
        LstBxFiles.Selected[0]:= True;
//dengland This doesnt seem to work.
//      LstBxFilesSelectionChange(Self, False);
        InitialiseFileView(0);
        end;
    end;

procedure TD64ExplorerMainFrame.DoPrepareFileInfo(const AEntry: Integer;
        const AStream: TStream; out AGEOSFile: Boolean);
    begin
    LblFileType.Caption:= D64FileTypeToStr(FEntries[AEntry].FileType, False);
    LblReplacing.Caption:= ChooseString(
            (FEntries[AEntry].FileType and $20) <> 0);
    LblFileLocked.Caption:=
            ChooseString((FEntries[AEntry].FileType and $40) <> 0);
    LblFileClosed.Caption:=
            ChooseString((FEntries[AEntry].FileType and $80) <> 0);
    LblFileData.Caption:=
            Format('$%2.2x $%2.2x', [FEntries[AEntry].EntryData[$03],
            FEntries[AEntry].EntryData[$04]]);

    AGEOSFile:= FD64File.D64Image.GEOSDisk and
            ((FEntries[AEntry].FileType and $07) in [1..3]) and
            (FEntries[AEntry].EntryData[$18] <> 0);

    LblGEOSFile.Caption:= ChooseString(AGEOSFile, STR_LBL_D64YES,
            STR_LBL_D64NO);

    LblFileSize.Caption:= Format('$%4.4x (%0:d)',
            [FEntries[AEntry].FileSize]);
    end;

procedure TD64ExplorerMainFrame.DoPrepareFileData(const AEntry: Integer;
        const AStream: TMemoryStream);
    var
    i: Integer;
    nt,
    ns: Byte;
    sz: Cardinal;

    begin
    LstBxFileData.Items.BeginUpdate;
    try
        LstBxFileData.Clear;

//dengland Not going to try reading scratched file data because if it was a CBM
//      partition, nasty things could happen.  Viewing scratched files is a
//      task for another routine.
        if  (FEntries[AEntry].EntryData[$03] <> 0)
        and ((FEntries[AEntry].FileType and $7) > 0) then
            begin
            AStream.Clear;

            if  (FEntries[AEntry].FileType and $7) <> 5 then
                FD64File.D64Image.GetDataChain(
                        FEntries[AEntry].EntryData[$03],
                        FEntries[AEntry].EntryData[$04], AStream, sz)
            else
                begin
                i:= 0;
                nt:= FEntries[AEntry].EntryData[$03];
                ns:= FEntries[AEntry].EntryData[$04];

                while i < FEntries[AEntry].FileSize do
                    begin
                    FD64File.D64Image.GetRawSector(nt, ns, AStream);

                    Inc(i);
                    Inc(ns);
                    if  ns = FD64File.D64Image.GetSectorsForTrack(nt) then
                        begin
                        Inc(nt);
                        ns:= 0;
                        end;
                    end;

                sz:= FEntries[AEntry].FileSize * VAL_SIZ_D64SECTRSIZE;
                end;

            LblFileActual.Caption:= Format('$%6.6x (%0:d)', [sz]);

            AStream.Position:= 0;

            LstBxFileData.Items.Add('Offset   Data');
            HexDumpStreamToLstBx(AStream, LstBxFileData);
            end
        else
            LblFileActual.Caption:= EmptyStr;

        finally
        LstBxFileData.Items.EndUpdate;
        end;
    end;

procedure TD64ExplorerMainFrame.DoPrepareFileREL(const AEntry: Integer;
        const AStream: TMemoryStream);
    begin

    end;

procedure TD64ExplorerMainFrame.DoPrepareGEOSInfo(const AEntry: Integer;
        const AStream: TMemoryStream);
    var
    d: TDateTime;
    b: Byte;
    s: string;
    i: Integer;

    procedure DoDecodeGEOSIcon;
        var
        b: Byte;
        i,
        k,
        x,
        y: Integer;

        begin
        for y:= 0 to 20 do
            begin
            for i:= 0 to 2 do
                begin
                x:= i * 8;
                b:= AStream.ReadByte;
                k:= 128;
                while k >= 1 do
                    begin
                    if  (b and k) <> 0 then
                        ImgGEOSIcon.Picture.Bitmap.Canvas.Pixels[x, y]:= clBlack;

                    Inc(x);
                    if k = 1 then
                        k:= 0
                    else
                        k:= k shr 1;
                    end;
                end;
            end;
        end;

    function DoReadWord: Word;
        var
        b: Byte;

        begin
        b:= AStream.ReadByte;
        Result:= b;
        b:= AStream.ReadByte;
        Result:= Result or (b shl 8);
        end;

    function DoReadStringData(const ALen: Byte): string;
        var
        b: Byte;
        i: Integer;

        begin
        Result:= EmptyStr;
        for i:= 0 to ALen - 1 do
            begin
            b:= AStream.ReadByte;
            if  b in [$20..$7E] then
                begin
                Result:= Result + string(AnsiChar(b));
//dengland      These strings are going to labels which will use '&' characters
//              as shortcut references unless we prevent it with '&&';
                if  b = $26 then
                    Result:= Result + string(AnsiChar(b));
                end
            else
                Result:= Result + ' ';
            end;
        end;

    begin
    ClearGEOSInfo;

    LblGEOSStruct.Caption:= D64GEOSStructToStr(FEntries[AEntry].EntryData[$17]);
    LblGEOSType.Caption:= D64GEOSFileTypeToStr(FEntries[AEntry].EntryData[$18]);
    LblGEOSInfoLoc.Caption:= Format('$%2.2x $%2.2x', [
            FEntries[AEntry].EntryData[$15],
            FEntries[AEntry].EntryData[$16]]);

    d:= EncodeDateTime(1900 + FEntries[AEntry].EntryData[$19],
            FEntries[AEntry].EntryData[$1A], FEntries[AEntry].EntryData[$1B],
            FEntries[AEntry].EntryData[$1C], FEntries[AEntry].EntryData[$1D],
            0, 0);
    LblGEOSDate.Caption:= DateTimeToStr(d);

    if  FEntries[AEntry].EntryData[$15] > 0 then
        begin
        AStream.Clear;
        FD64File.D64Image.GetRawSector(
                FEntries[AEntry].EntryData[$15],
                FEntries[AEntry].EntryData[$16], AStream);

        AStream.Position:= 2;
        s:= EmptyStr;
        for i:= 0 to 2 do
            begin
            b:= AStream.ReadByte;
            s:= s + Format('$%2.2x ', [b]);
            end;
        LblGEOSIDBytes.Caption:= s;

        DoDecodeGEOSIcon;

        LblGEOSC64Type.Caption:= D64FileTypeToStr(AStream.ReadByte, False);
        LblGEOSTypeInf.Caption:= D64GEOSFileTypeToStr(AStream.ReadByte);
        LblGEOSStructInf.Caption:= D64GEOSStructToStr(AStream.ReadByte);

        LblGEOSLoad.Caption:= Format('$%4.4x', [DoReadWord]);
        LblGEOSEnd.Caption:= Format('$%4.4x', [DoReadWord]);
        LblGEOSInit.Caption:= Format('$%4.4x', [DoReadWord]);

        LblGEOSClass.Caption:= DoReadStringData($13);

        LblGEOS4080.Caption:= Format('$%2.2x', [AStream.ReadByte]);

        LblGEOSAuthor.Caption:= DoReadStringData($14);
        LblGEOSParent.Caption:= DoReadStringData($14);

        s:= EmptyStr;
        for i:= 0 to $16 do
            begin
            b:= AStream.ReadByte;
            s:= s + Format('$%2.2x ', [b]);
            end;
        LblGEOSAppData.Caption:= s;

        LblGEOSDesc.Caption:= DoReadStringData($60);

        AStream.Position:= $4D;
        HexDumpStreamToLstBx(AStream, LstBxGEOSData);
        end;
    end;

procedure TD64ExplorerMainFrame.DoPrepareGEOSVLIR(const AEntry: Integer;
        const AStream: TMemoryStream);
    var
    c: Boolean;
    t,
    s: Byte;
    i: Integer;

    begin
    c:= FChanging;
    FChanging:= True;
    try
        AStream.Clear;
        FD64File.D64Image.GetRawSector(
                FEntries[AEntry].EntryData[$03],
                FEntries[AEntry].EntryData[$04], AStream);

        CmbGEOSVLIRRec.Items.BeginUpdate;
        try
            CmbGEOSVLIRRec.Items.Clear;

            SetLength(FVLIRRecs, 127);
            for i:= 0 to 126 do
                FVLIRRecs[i]:= $0000;

            AStream.Position:= $02;
            i:= 0;
            while AStream.Position < AStream.Size do
                begin
                t:= AStream.ReadByte;
                s:= AStream.ReadByte;

                FVLIRRecs[i]:= t or (s shl 8);

                if  t = $00 then
                    CmbGEOSVLIRRec.Items.Add('%3.3d:  %s ($%2.2x $%2.2x)', [i,
                            ChooseString(s = 0, STR_LBL_D64GEOSVREND,
                            STR_LBL_D64GEOSVRNON), t, s])
                else
                    CmbGEOSVLIRRec.Items.Add('%3.3d:  $%2.2x $%2.2x', [i, t, s]);

                Inc(i);
                end;

            finally
            CmbGEOSVLIRRec.Items.EndUpdate;
            end;

        finally
        FChanging:= c;
        end;

    CmbGEOSVLIRRec.ItemIndex:= 0;
    DoPrepareGEOSVLIRRec(0);
    end;

procedure TD64ExplorerMainFrame.DoPrepareGEOSVLIRRec(const ARecord: Integer);
    var
    t,
    s: Byte;
    m: TMemoryStream;
    sz: Cardinal;

    begin
    t:= FVLIRRecs[ARecord] and $00FF;
    s:= (FVLIRRecs[ARecord] and $FF00) shr 8;

    LstBxGEOSVLIRRec.Clear;
    LblGEOSVLIRActual.Caption:= EmptyStr;

    if  t = 0 then
        Exit;

    m:= TMemoryStream.Create;
    try
        FD64File.D64Image.GetDataChain(t, s, m, sz);

        m.Position:= 0;
        HexDumpStreamToLstBx(m, LstBxGEOSVLIRRec);

        LblGEOSVLIRActual.Caption:= Format('$%6.6x (%0:d)', [sz]);

        finally
        m.Free;
        end;
    end;

procedure TD64ExplorerMainFrame.ClearGEOSInfo;
    begin
    LblGEOSStruct.Caption:= EmptyStr;
    LblGEOSType.Caption:= EmptyStr;
    LblGEOSInfoLoc.Caption:= EmptyStr;
    LblGEOSDate.Caption:= EmptyStr;
    LblGEOSIDBytes.Caption:= EmptyStr;

    if  not Assigned(ImgGEOSIcon.Picture.Bitmap) then
//dengland  Strangely, this isn't being called...
        ImgGEOSIcon.Picture.Bitmap:= TBitmap.Create;

    ImgGEOSIcon.Picture.Bitmap.Width:= 24;
    ImgGEOSIcon.Picture.Bitmap.Height:= 21;
    ImgGEOSIcon.Picture.Bitmap.Canvas.Brush.Color:= clNone;
    ImgGEOSIcon.Picture.Bitmap.Canvas.Pen.Color:= clBlack;
    ImgGEOSIcon.Picture.Bitmap.Canvas.FillRect(0, 0, 24, 21);

    LblGEOSC64Type.Caption:= EmptyStr;
    LblGEOSTypeInf.Caption:= EmptyStr;
    LblGEOSStructInf.Caption:= EmptyStr;
    LblGEOSLoad.Caption:= EmptyStr;
    LblGEOSEnd.Caption:= EmptyStr;
    LblGEOSInit.Caption:= EmptyStr;
    LblGEOSClass.Caption:= EmptyStr;
    LblGEOS4080.Caption:= EmptyStr;
    LblGEOSAuthor.Caption:= EmptyStr;
    LblGEOSParent.Caption:= EmptyStr;
    LblGEOSAppData.Caption:= EmptyStr;
    LblGEOSDesc.Caption:= EmptyStr;

    LstBxGEOSData.Items.Clear;
    end;

procedure TD64ExplorerMainFrame.InitialiseFileView(const AEntry: Integer);
    var
    pc: TCursor;
    m: TMemoryStream;
    g: Boolean;

    begin
    pc:= Screen.Cursor;
    Screen.Cursor:= crHourGlass;
    m:= TMemoryStream.Create;
    try
        DoPrepareFileInfo(AEntry, m, g);
        DoPrepareFileData(AEntry, m);

        if  g then
            begin
            TbShtC64REL.TabVisible:= False;
            TbShtGEOSInfo.TabVisible:= True;

            DoPrepareGEOSInfo(AEntry, m);

            if  FEntries[AEntry].EntryData[$17] = 1 then
                begin
                TbShtGEOSVLIR.TabVisible:= True;
                DoPrepareGEOSVLIR(AEntry, m);
                end
            else
                TbShtGEOSVLIR.TabVisible:= False;
            end
        else
            begin
            TbShtGEOSInfo.TabVisible:= False;
            TbShtGEOSVLIR.TabVisible:= False;

            if  (FEntries[AEntry].FileType and $07) = 4 then
                begin
                TbShtC64REL.TabVisible:= True;

                DoPrepareFileREL(AEntry, m);
                end
            else
                TbShtC64REL.TabVisible:= False;
            end;


        PgCtrlFile.ActivePage:= TbShtC64File;

        finally
        m.Free;
        Screen.Cursor:= pc;
        end;
    end;

class function TD64ExplorerMainFrame.GetDescription: string;
	begin
    Result:= 'File Inspector';
	end;

procedure TD64ExplorerMainFrame.Prepare(const AD64File: TD64File);
	begin
	FD64File:= AD64File;
    D64ExplorerMainDMod.BindToCoolbar(PnlDetails);

	inherited;
	end;

procedure TD64ExplorerMainFrame.Unprepare;
	begin
	if  not Prepared then
		Exit;

    D64ExplorerMainDMod.UnbindToCoolbar(PnlDetails, Self);
    PnlDetails.Parent:= Self;
    PnlDetails.Visible:= False;

	inherited;
	end;

procedure TD64ExplorerMainFrame.Initialise;
    var
    i: Integer;

    begin
    LblDOSVersion.Caption:= EmptyStr;
    LblDOSType.Caption:= EmptyStr;
    LblDiskName.Caption:= EmptyStr;
    LblDiskID.Caption:= EmptyStr;
    LblNumTracks.Caption:= EmptyStr;
    LblGEOSVer.Caption:= EmptyStr;

    if  FD64File.D64Image.ValidVersion then
        LblDOSVersion.Caption:= Format('$%2.2x',
        		[FD64File.D64Image.DOSVersion])
    else
        LblDOSVersion.Caption:= STR_LBL_D64DOSINVALID;

    LblDOSType.Caption:= FD64File.D64Image.DOSType;
    LblDiskName.Caption:= PetsciiToAsciiString(FD64File.D64Image.DiskName);
    LblDiskID.Caption:= PetsciiToAsciiString(FD64File.D64Image.DiskID);
    LblNumTracks.Caption:= IntToStr(FD64File.D64Image.TrackCount);

    if  FD64File.D64Image.GEOSDisk then
        LblGEOSVer.Caption:= STR_LBL_D64YES + ' (' +
                STR_LBL_D64VERSION + ' ' +
                IntToStr(FD64File.D64Image.GEOSVerMajor) + '.' +
                IntToStr(FD64File.D64Image.GEOSVerMinor) + ')'
    else
        LblGEOSVer.Caption:= STR_LBL_D64NO;

    PnlDetails.Visible:= True;

    if FD64File.D64Image.DiskType = ddt1581 then
        begin
        FD64File.D64Image.GetDirPartitions(FDirectories);

        PnlDirectory.Visible:= True;

        FChanging:= True;
        try
            CmbDirectory.Items.BeginUpdate;
            try
                CmbDirectory.Clear;

                for i:= 0 to High(FDirectories) do
                    CmbDirectory.Items.Add('/' +
							PetsciiToAsciiString(FDirectories[i].PartFileName));

                finally
                CmbDirectory.Items.EndUpdate;
                end;

            CmbDirectory.ItemIndex:= 0;

            finally
            FChanging:= False;
            end;
        end
    else
        begin
        PnlDirectory.Visible:= False;
        SetLength(FDirectories, 0);
        end;

    DoInitialiseFiles;
    end;

end.

