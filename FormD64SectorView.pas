//------------------------------------------------------------------------------
//FormD64SectorView
//=================
//Displays a hex dump of a selected sector from the currently opened disk image.
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
unit FormD64SectorView;

{$mode Delphi}
{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, C64D64Image, D64ExplorerTypes;

type

{ TD64SectorViewForm }

	TD64SectorViewForm = class(TForm)
        CmbTrack: TComboBox;
        CmbSector: TComboBox;
        Label1: TLabel;
        Label2: TLabel;
        LstBxHex: TListBox;
        procedure CmbSectorChange(Sender: TObject);
        procedure CmbTrackChange(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
	private
        FChanging: Boolean;

		FD64File: TD64File;

        procedure ClearDisplay;
        procedure InitialiseTracks;
        procedure InitialiseSectors;

        procedure DisplayTrackSector(const ATrack: TD64TrackNum;
        		const ASector: TD64SectorNum);

  	public
    	property  D64File: TD64File read FD64File write FD64File;
  	end;

var
  D64SectorViewForm: TD64SectorViewForm;

implementation

{$R *.lfm}

uses
  	D64ExplorerUtils, DModD64ExplorerMain;

{ TD64SectorViewForm }

procedure TD64SectorViewForm.FormClose(Sender: TObject;
		var CloseAction: TCloseAction);
	begin
    D64ExplorerMainDMod.ActViewSecView.Checked:= False;
	end;

procedure TD64SectorViewForm.CmbTrackChange(Sender: TObject);
    begin
    if  not FChanging then
        begin
        ClearDisplay;
        InitialiseSectors;
        CmbSector.ItemIndex:= 0;
        CmbSectorChange(Self);
        end;
    end;

procedure TD64SectorViewForm.CmbSectorChange(Sender: TObject);
	begin
    if  not FChanging then
    	DisplayTrackSector(TD64TrackNum(CmbTrack.ItemIndex + 1),
        		TD64SectorNum(CmbSector.ItemIndex));
	end;

procedure TD64SectorViewForm.FormShow(Sender: TObject);
	begin
    ClearDisplay;
    InitialiseTracks;
    if  FD64File.D64Image.DiskType = ddt1581 then
        CmbTrack.ItemIndex:= 39
    else
        CmbTrack.ItemIndex:= 17;
    CmbTrackChange(Self);
    CmbSector.ItemIndex:= 0;
    CmbSectorChange(Self);

    D64ExplorerMainDMod.ActViewSecView.Checked:= True;
	end;

procedure TD64SectorViewForm.ClearDisplay;
	begin
    LstBxHex.Clear;
	end;

procedure TD64SectorViewForm.InitialiseTracks;
    var
    t: TD64TrackNum;

    begin
    FChanging:= True;
    CmbTrack.Items.BeginUpdate;
    CmbSector.Items.BeginUpdate;
    try
        CmbTrack.Clear;
        CmbSector.Clear;

        for t:= 1 to FD64File.D64Image.TrackCount do
        	CmbTrack.Items.Add(IntToStr(t));

    	finally
        CmbSector.Items.EndUpdate;
        CmbTrack.Items.EndUpdate;
        FChanging:= False;
        end;
    end;

procedure TD64SectorViewForm.InitialiseSectors;
    var
    s: Byte;

    begin
    FChanging:= True;
    CmbSector.Items.BeginUpdate;
    try
        CmbSector.Clear;

        for s:= 0 to FD64File.D64Image.GetSectorsForTrack(
                TD64TrackNum(CmbTrack.ItemIndex + 1)) - 1 do
        	CmbSector.Items.Add(IntToStr(s));

    	finally
        CmbSector.Items.EndUpdate;
        FChanging:= False;
        end;
	end;

procedure TD64SectorViewForm.DisplayTrackSector(const ATrack: TD64TrackNum;
		const ASector: TD64SectorNum);
    var
    m: TMemoryStream;
    b: Byte;
    i,
    j: Integer;
    sh,
    sa: string;

    begin
    m:= TMemoryStream.Create;
    try
       	ClearDisplay;

        FD64File.D64Image.GetRawSector(ATrack, ASector, m);
        m.Position:= 0;

        LstBxHex.Items.BeginUpdate;
        try
            HexDumpStreamToLstBx(m, LstBxHex, 1);

        	finally
            LstBxHex.Items.EndUpdate;
            end;

    	finally
        m.Free;
        end;
    end;

end.

