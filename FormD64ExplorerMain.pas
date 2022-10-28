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

{$mode delphi}{$H+}

interface

uses
 	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
    ActnList, ExtCtrls, StdCtrls, Buttons, ComCtrls;

type

{ TD64ExplorerMainForm }

   TD64ExplorerMainForm = class(TForm)
        Bevel1: TBevel;
        Bevel2: TBevel;
        Bevel3: TBevel;
        CoolBar1: TCoolBar;
        CoolBar2: TCoolBar;
        Label1: TLabel;
        PaintBox1: TPaintBox;
        Panel1: TPanel;
        Panel2: TPanel;
        Panel3: TPanel;
        Panel4: TPanel;
        SpeedButton1: TSpeedButton;
        SpeedButton2: TSpeedButton;
        SpeedButton3: TSpeedButton;
        SpeedButton4: TSpeedButton;
        SpeedButton5: TSpeedButton;
        SpeedButton6: TSpeedButton;
        SpeedButton7: TSpeedButton;
        SpeedButton8: TSpeedButton;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
        procedure FormDropFiles(Sender: TObject;
            const FileNames: array of String);
        procedure PaintBox1Paint(Sender: TObject);
        procedure SpeedButton1Paint(Sender: TObject);
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

procedure TD64ExplorerMainForm.FormCloseQuery(Sender: TObject;
    	var CanClose: Boolean);
	begin
    D64ExplorerMainDMod.CloseQuery(CanClose);
	end;

procedure TD64ExplorerMainForm.FormDropFiles(Sender: TObject;
    	const FileNames: array of String);
	begin
    D64ExplorerMainDMod.AddDropFiles(FileNames);
	end;

procedure TD64ExplorerMainForm.PaintBox1Paint(Sender: TObject);
	begin
    PaintBox1.Canvas.GradientFill(PaintBox1.ClientRect, clBtnShadow,
    		clBtnFace, gdHorizontal);
	end;

procedure TD64ExplorerMainForm.SpeedButton1Paint(Sender: TObject);
    var
    sb: TSpeedButton;

    begin
    sb:= Sender as TSpeedButton;

    if  sb.Down then
    	begin
        if  sb.Enabled then
        	sb.Canvas.Brush.Color:= clMenuHighlight
        else
            sb.Canvas.Brush.Color:= clBtnShadow;

        sb.Canvas.Brush.Style:= bsSolid;

        sb.Canvas.FillRect(sb.ClientRect);

        sb.Images.Draw(sb.Canvas, 3, 3, sb.ImageIndex, sb.Enabled);
        end;
    end;


end.

