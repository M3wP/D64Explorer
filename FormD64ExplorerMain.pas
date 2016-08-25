//------------------------------------------------------------------------------
//FormD64ExplorerMain
//===================
//Application Main Form.  The application functions are managed and performed by
//the Main Data Module.
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
unit FormD64ExplorerMain;

{$mode objfpc}{$H+}

interface

uses
 	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
    ActnList, ExtCtrls, StdCtrls;

type

{ TD64ExplorerMainForm }

   TD64ExplorerMainForm = class(TForm)
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        LblNumTracks: TLabel;
        LblGEOSVer: TLabel;
        LblDiskID: TLabel;
        LblDiskName: TLabel;
        LblDOSType: TLabel;
        LblDOSVersion: TLabel;
        PnlDetails: TPanel;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  	private

    public
    	{ public declarations }
  	end;

var
  	D64ExplorerMainForm: TD64ExplorerMainForm;

implementation

{$R *.lfm}

uses
    DModD64ExplorerMain;

{ TD64ExplorerMainForm }


procedure TD64ExplorerMainForm.FormClose(Sender: TObject;
        var CloseAction: TCloseAction);
    begin
    D64ExplorerMainDMod.ApplicationClose;
    end;

end.

