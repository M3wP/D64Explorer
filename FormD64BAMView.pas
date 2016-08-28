//------------------------------------------------------------------------------
//FormD64BAMView
//==============
//Displays the BAM (block allocation map) for the currently opened disk image.
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
unit FormD64BAMView;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ExtCtrls, C64D64Image, Types;

type

    { TD64BAMViewForm }

    TD64BAMViewForm = class(TForm)
        CmbDirectory: TComboBox;
        Label32: TLabel;
        Label9: TLabel;
        LstBxBAM: TListBox;
        PnlDirectories: TPanel;
        procedure CmbDirectoryChange(Sender: TObject);
        procedure CmbDirectoryDrawItem(Control: TWinControl; Index: Integer;
            ARect: TRect; State: TOwnerDrawState);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
    private
        FChanging: Boolean;
        FDirectories: TD64DirPartitions;

        procedure ClearDisplay;
        procedure DoInitialiseDisplay;
        procedure InitialiseDisplay;
    public
        { public declarations }
    end;

var
    D64BAMViewForm: TD64BAMViewForm;

implementation

{$R *.lfm}

uses
    LCLType, DModD64ExplorerMain;


{ TD64BAMViewForm }

procedure TD64BAMViewForm.FormShow(Sender: TObject);
    begin
    ClearDisplay;
    InitialiseDisplay;

    D64ExplorerMainDMod.ActViewBAMView.Checked:= True;
    end;

procedure TD64BAMViewForm.ClearDisplay;
    begin
    LstBxBAM.Clear;
    end;

procedure TD64BAMViewForm.DoInitialiseDisplay;
    var
    b: TD64DiskBAM;
    m: Integer;
    info: TD64DirPartitionInfo;

    procedure DoOutputTracksBAM(const AStart, AEnd: Integer);
        var
        t: Byte;
        s: Byte;
        d: Byte;
        o: string;
        i: Integer;

        begin
        LstBxBAM.Items.Add('              Tracks');
        o:= '              ';
        for t:= AStart to AEnd do
            o:= o + Format('%2.2d ', [t + 1]);
        LstBxBAM.Items.Add(o);
        LstBxBAM.Items.Add(EmptyStr);

        o:= 'Sec. Free     ';
        for t:= AStart to AEnd do
            o:= o + Format('%2.2d ', [b[t].FreeSectors]);
        LstBxBAM.Items.Add(o);
        LstBxBAM.Items.Add(EmptyStr);

        d:= 1;
        i:= 0;
        for s:= 0 to D64ExplorerMainDMod.D64Image.MaxSectors - 1 do
            begin
            if  s = 0 then
                o:= 'Sector 00     '
            else
                o:= Format('       %2.2d     ', [Ord(s)]);

            for t:= AStart to AEnd do
                if  s < D64ExplorerMainDMod.D64Image.GetSectorsForTrack(t + 1) then
                    if  (b[t].Bitmap[i] and d) = 0 then
                        o:= o + ' X '
                    else
                        o:= o + ' . '
                else
                    o:= o + '   ';

            LstBxBAM.Items.Add(o);

            if  d = 128 then
                begin
                d:= 1;
                Inc(i);
                end
            else
                d:= d shl 1;
            end;
        end;

    begin
//              Tracks
//              01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 .. 30 31 32 33 34 35
//
//Sec. Free
//
//Sector 00      X  .
//       nn      X  .

    if  D64ExplorerMainDMod.D64Image.DiskType = ddt1581 then
        begin
        D64ExplorerMainDMod.D64Image.SetCurrentPartition(
                FDirectories[CmbDirectory.ItemIndex].Info, info);
        try
            D64ExplorerMainDMod.D64Image.GetPartitionBAM(b);

            finally
            D64ExplorerMainDMod.D64Image.SetCurrentPartition(info);
            end;
        end
    else
        D64ExplorerMainDMod.D64Image.GetDiskBAM(b);

    LstBxBAM.Items.BeginUpdate;
    try
        LstBxBAM.Clear;

        if  (not D64ExplorerMainDMod.D64Image.SingleSide)
        and (Length(b) > 40) then
            m:= Length(b) div 2
        else
            m:= Length(b);

        DoOutputTracksBAM(0, m - 1);

        if  (not D64ExplorerMainDMod.D64Image.SingleSide)
        and (Length(b) > 40) then
            begin
            LstBxBAM.Items.Add(EmptyStr);
            LstBxBAM.Items.Add(EmptyStr);

            DoOutputTracksBAM(m, High(b));
            end;

        finally
        LstBxBAM.Items.EndUpdate;
        end;
    end;

procedure TD64BAMViewForm.InitialiseDisplay;
    var
    i: Integer;

    begin
    if  D64ExplorerMainDMod.D64Image.DiskType = ddt1581 then
        begin
        D64ExplorerMainDMod.D64Image.GetDirPartitions(FDirectories);

        FChanging:= True;
        CmbDirectory.Items.BeginUpdate;
        try
            CmbDirectory.Clear;

            for i:= 0 to High(FDirectories) do
                CmbDirectory.Items.Add('/' + FDirectories[i].PartFileName);

            finally
            CmbDirectory.Items.EndUpdate;
            FChanging:= False;
            end;

        CmbDirectory.ItemIndex:= 0;
        PnlDirectories.Visible:= True;
        end
    else
        PnlDirectories.Visible:= False;

    DoInitialiseDisplay;
    end;

procedure TD64BAMViewForm.FormClose(Sender: TObject;
        var CloseAction: TCloseAction);
    begin
    D64ExplorerMainDMod.ActViewBAMView.Checked:= False;
    end;

procedure TD64BAMViewForm.CmbDirectoryChange(Sender: TObject);
    begin
    if  not FChanging then
        DoInitialiseDisplay;
    end;

procedure TD64BAMViewForm.CmbDirectoryDrawItem(Control: TWinControl;
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


end.


