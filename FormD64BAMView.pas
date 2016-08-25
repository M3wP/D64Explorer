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
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

    { TD64BAMViewForm }

    TD64BAMViewForm = class(TForm)
        LstBxBAM: TListBox;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
    private
        procedure ClearDisplay;
        procedure InitialiseDisplay;
    public
        { public declarations }
    end;

var
    D64BAMViewForm: TD64BAMViewForm;

implementation

{$R *.lfm}

uses
    C64D64Image, DModD64ExplorerMain;


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

procedure TD64BAMViewForm.InitialiseDisplay;
    var
    b: TD64DiskBAM;
    m: Integer;

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

    D64ExplorerMainDMod.D64Image.GetDiskBAM(b);

    LstBxBAM.Items.BeginUpdate;
    try
        if  not D64ExplorerMainDMod.D64Image.SingleSide then
            m:= Length(b) div 2
        else
            m:= Length(b);

        DoOutputTracksBAM(0, m - 1);

        if  not D64ExplorerMainDMod.D64Image.SingleSide then
            begin
            LstBxBAM.Items.Add(EmptyStr);
            LstBxBAM.Items.Add(EmptyStr);

            DoOutputTracksBAM(m, High(b));
            end;

        finally
        LstBxBAM.Items.EndUpdate;
        end;
    end;

procedure TD64BAMViewForm.FormClose(Sender: TObject;
        var CloseAction: TCloseAction);
    begin
    D64ExplorerMainDMod.ActViewBAMView.Checked:= False;
    end;


end.


