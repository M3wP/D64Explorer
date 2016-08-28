//------------------------------------------------------------------------------
//FormD64DirectoryView
//====================
//Provides a directory list of the files on the currently opened disk image and
//file entry details for a selected file.
//
//Should be reworked to take advantage of the fact that GetFileEntries now
//returns the entry data as well.
//
//Please note:
//------------
//Presently, only FPC/Lazarus is supported.  Delphi support is incomplete.
//Needs a class helper for TMemoryStream to include ReadByte (which I'm a fan
//of) or ReadByte changed to ReadBuffer.
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
unit FormD64DirectoryView;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ExtCtrls, C64D64Image, Types;

type

{ TD64DirectoryViewForm }

    TD64DirectoryViewForm = class(TForm)
        CmbDirectory: TComboBox;
        Label1: TLabel;
        Label32: TLabel;
        Label9: TLabel;
        LblFileType: TLabel;
        LblFileLoc: TLabel;
        LblFileName: TLabel;
        LblSideSect: TLabel;
        LblRELRecSz: TLabel;
        LblData: TLabel;
        LblFileSz: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        Label7: TLabel;
        Label8: TLabel;
        LblDirLoc: TLabel;
        LstBxDirectory: TListBox;
        Panel1: TPanel;
        PnlDirectories: TPanel;
        procedure CmbDirectoryChange(Sender: TObject);
        procedure CmbDirectoryDrawItem(Control: TWinControl; Index: Integer;
            ARect: TRect; State: TOwnerDrawState);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
        procedure LstBxDirectorySelectionChange(Sender: TObject; User: boolean);
    private
        FChanging: Boolean;
        FEntryTrkSectrs: array of Word;
        FDirectories: TD64DirPartitions;

        procedure DoIntitialiseFiles;

        procedure ClearDisplay;
        procedure ClearView;
        procedure InitialiseDisplay;
        procedure InitialiseView(const ATrack, ASector, AEntry: Byte);
    public
        { public declarations }
    end;

var
    D64DirectoryViewForm: TD64DirectoryViewForm;

implementation

{$R *.lfm}

uses
    LCLType, DModD64ExplorerMain;


{ TD64DirectoryViewForm }

procedure TD64DirectoryViewForm.FormShow(Sender: TObject);
    begin
    ClearDisplay;
    InitialiseDisplay;

    D64ExplorerMainDMod.ActViewDirView.Checked:= True;
    end;

procedure TD64DirectoryViewForm.LstBxDirectorySelectionChange(Sender: TObject;
        User: boolean);
    var
    i,
    j: Integer;
    t,
    s,
    e: Byte;
    p: Word;

    begin
    if  not FChanging then
        begin
        ClearView;

        j:= -1;
        for i:= 1 to LstBxDirectory.Items.Count - 1 do
            if  LstBxDirectory.Selected[i] then
                begin
                j:= i;
                Break;
                end;

        if j > -1 then
            begin
            e:= (j - 1) mod 8;
            p:= FEntryTrkSectrs[j - 1];
            t:= p and $FF;
            s:= (p and $FF00) shr 8;

            InitialiseView(t, s, e);
            end;
        end;
    end;

procedure TD64DirectoryViewForm.DoIntitialiseFiles;
    var
    e: TD64DirEntries;
    ft: string;
    i: Integer;
    info: TD64DirPartitionInfo;

    begin
    LstBxDirectory.Items.BeginUpdate;
    try
        LstBxDirectory.Clear;

        if  D64ExplorerMainDMod.D64Image.DiskType = ddt1581 then
            begin
            D64ExplorerMainDMod.D64Image.SetCurrentPartition(
                    FDirectories[CmbDirectory.ItemIndex].Info, info);
            try
                LstBxDirectory.Items.Add(Format('0 %-18s %s %s', [
                        '"' + FDirectories[CmbDirectory.ItemIndex].PartDiskName+'"',
                        FDirectories[CmbDirectory.ItemIndex].PartDiskID,
                        FDirectories[CmbDirectory.ItemIndex].PartDOSType]));

                D64ExplorerMainDMod.D64Image.GetPartitionFiles(e);

                finally
                D64ExplorerMainDMod.D64Image.SetCurrentPartition(info);
                end;
            end
        else
            begin;
            LstBxDirectory.Items.Add(Format('0 %-18s %s %s', [
                    '"' + D64ExplorerMainDMod.D64Image.DiskName + '"',
                    D64ExplorerMainDMod.D64Image.DiskID,
                    D64ExplorerMainDMod.D64Image.DOSType]));

            D64ExplorerMainDMod.D64Image.GetFileEntries(e);
            end;

        SetLength(FEntryTrkSectrs, Length(e));

        for i:= 0 to High(e) do
            begin
            ft:= D64FileTypeToStr(e[i].FileType);

            LstBxDirectory.Items.Add(Format('%-5d %-18s %s',
                    [e[i].FileSize, '"' + e[i].FileName + '"', ft]));
            FEntryTrkSectrs[i]:= e[i].Track + e[i].Sector shl 8;
            end;
        finally
        LstBxDirectory.Items.EndUpdate;
        end;
    end;

procedure TD64DirectoryViewForm.ClearDisplay;
    begin
    LstBxDirectory.Clear;
    ClearView;
    end;

procedure TD64DirectoryViewForm.ClearView;
    begin
    LblDirLoc.Caption:= EmptyStr;
    LblFileType.Caption:= EmptyStr;
    LblFileLoc.Caption:= EmptyStr;
    LblFileName.Caption:= EmptyStr;
    LblSideSect.Caption:= EmptyStr;
    LblRELRecSz.Caption:= EmptyStr;
    LblData.Caption:= EmptyStr;
    LblFileSz.Caption:= EmptyStr;
    end;

procedure TD64DirectoryViewForm.InitialiseDisplay;
    var
    i: Integer;

    begin
    FChanging:= True;
    try
        if  D64ExplorerMainDMod.D64Image.DiskType = ddt1581 then
            begin
            D64ExplorerMainDMod.D64Image.GetDirPartitions(FDirectories);

            CmbDirectory.Items.BeginUpdate;
            try
                CmbDirectory.Clear;

                for i:= 0 to High(FDirectories) do
                    CmbDirectory.Items.Add('/' + FDirectories[i].PartFileName);

                finally
                CmbDirectory.Items.EndUpdate;
                end;

            CmbDirectory.ItemIndex:= 0;
            PnlDirectories.Visible:= True;
            end
        else
            PnlDirectories.Visible:= False;

        DoIntitialiseFiles;

        finally
        FChanging:= False;
        end;
    end;

procedure TD64DirectoryViewForm.InitialiseView(const ATrack, ASector,
        AEntry: Byte);
    var
    m: TMemoryStream;
    b1,
    b2: Byte;
    i: Integer;
    s: string;

    begin
    m:= TMemoryStream.Create;
    try
        D64ExplorerMainDMod.D64Image.GetRawSector(ATrack, ASector, m);

        m.Position:= $20 * AEntry;

        b1:= m.ReadByte;
        b2:= m.ReadByte;
        LblDirLoc.Caption:= Format('$%2.2x $%2.2x', [b1, b2]);

        b1:= m.ReadByte;
        LblFileType.Caption:= Format('$%2.2x', [b1]);

        b1:= m.ReadByte;
        b2:= m.ReadByte;
        LblFileLoc.Caption:= Format('$%2.2x $%2.2x', [b1, b2]);

        s:= '';
        for i:= 0 to 15 do
            begin
            b1:= m.ReadByte;
            s:= s + Format('$%2.2x ', [b1]);
            end;
        LblFileName.Caption:= s;

        b1:= m.ReadByte;
        b2:= m.ReadByte;
        LblSideSect.Caption:= Format('$%2.2x $%2.2x', [b1, b2]);

        b1:= m.ReadByte;
        LblRELRecSz.Caption:= Format('$%2.2x', [b1]);

        s:= '';
        for i:= 0 to 5 do
            begin
            b1:= m.ReadByte;
            s:= s + Format('$%2.2x ', [b1]);
            end;
        LblData.Caption:= s;

        b1:= m.ReadByte;
        b2:= m.ReadByte;
        LblFileSz.Caption:= Format('$%2.2x $%2.2x', [b1, b2]);

        finally
        m.Free;
        end;
    end;

procedure TD64DirectoryViewForm.FormClose(Sender: TObject;
        var CloseAction: TCloseAction);
    begin
    D64ExplorerMainDMod.ActViewDirView.Checked:= False;
    end;

procedure TD64DirectoryViewForm.CmbDirectoryDrawItem(Control: TWinControl;
        Index: Integer; ARect: TRect; State: TOwnerDrawState);
    var
    s: string;
    i: Integer;

    begin
    //odSelected, odGrayed, odDisabled, odChecked,
    //odFocused, odDefault, odHotLight, odInactive, odNoAccel,
    //odNoFocusRect, odReserved1, odReserved2, odComboBoxEdit,
    //odPainted  // item already painted

    if  not (odPainted in State) then
        begin
        if  odSelected in State then
            begin
            CmbDirectory.Canvas.Brush.Color:= clHighlight;
            CmbDirectory.Canvas.Pen.Color:= clWindow;
            end
        else
            begin
            CmbDirectory.Canvas.Brush.Color:= clWindow;
            CmbDirectory.Canvas.Pen.Color:= clWindowText;
            end;

        CmbDirectory.Canvas.Brush.Style:= bsSolid;
        CmbDirectory.Canvas.Pen.Style:= psSolid;

        CmbDirectory.Canvas.FillRect(ARect);

        s:= '/' + TrimRight(FDirectories[Index].PartFileName);
        if  odComboBoxEdit in State then
            begin
            i:= FDirectories[Index].Parent;
            while i > 0 do
                begin
                s:= '/' + TrimRight(FDirectories[i].PartFileName) + s;
                i:= FDirectories[i].Parent;
                end;
            end
        else
            begin
            if  FDirectories[Index].HasChildren then
                s:= '+' + s
            else
                s:= '-' + s;

            s:= '-' + s;

            for i:= FDirectories[Index].Depth downto 0 do
                if  i = 1 then
                    s:= '|' + s
                else if i > 0 then
                    s:= '  |' + s;
            end;

        CmbDirectory.Canvas.TextOut(ARect.Left, ARect.Top, s);
        end;
    end;

procedure TD64DirectoryViewForm.CmbDirectoryChange(Sender: TObject);
    begin
    if  not FChanging then
        DoIntitialiseFiles;
    end;

end.

