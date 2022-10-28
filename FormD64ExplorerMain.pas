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
//ANY WARRANTY; without even  the implied warranty of MERCHANTABILITY or FITNESS
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
    ActnList, ExtCtrls, StdCtrls, Buttons, ComCtrls, ButtonPanel;

type

{ TD64ExplorerMainForm }

   TD64ExplorerMainForm = class(TForm)
        Bevel1: TBevel;
        Bevel2: TBevel;
		CoolBar1: TCoolBar;
        CoolBar2: TCoolBar;
        CoolBar3: TCoolBar;
        Label1: TLabel;
        PaintBox1: TPaintBox;
        Panel1: TPanel;
        Panel2: TPanel;
        Panel3: TPanel;
        Panel4: TPanel;
		Panel5: TPanel;
		Panel6: TPanel;
        Panel7: TPanel;
        SpeedButton1: TSpeedButton;
        SpeedButton10: TSpeedButton;
        SpeedButton11: TSpeedButton;
        SpeedButton2: TSpeedButton;
        SpeedButton3: TSpeedButton;
        SpeedButton4: TSpeedButton;
        SpeedButton5: TSpeedButton;
        SpeedButton6: TSpeedButton;
        SpeedButton7: TSpeedButton;
        SpeedButton8: TSpeedButton;
        SpeedButton9: TSpeedButton;
        procedure CoolBar3Resize(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
		procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormDropFiles(Sender: TObject;
            const FileNames: array of String);
		procedure FormResize(Sender: TObject);
        procedure PaintBox1Paint(Sender: TObject);
        procedure Panel1Paint(Sender: TObject);
		procedure Panel5Paint(Sender: TObject);
        procedure SpeedButton1Paint(Sender: TObject);
        procedure SpeedButton3Paint(Sender: TObject);
  	private
    	FBarBmp,
        FTskBmp: TBitmap;
    public
    	{ public declarations }
  	end;

var
  	D64ExplorerMainForm: TD64ExplorerMainForm;

implementation

{$R *.lfm}

uses
    GraphType, D64ExplorerConsts, DModD64ExplorerMain;

{ TD64ExplorerMainForm }


procedure TD64ExplorerMainForm.FormClose(Sender: TObject;
        var CloseAction: TCloseAction);
    begin
    D64ExplorerMainDMod.ApplicationClose;
    end;

procedure TD64ExplorerMainForm.CoolBar3Resize(Sender: TObject);
	begin
    if  not Application.Terminated then
		D64ExplorerMainDMod.RezizeFilesBar;
	end;

procedure TD64ExplorerMainForm.FormCloseQuery(Sender: TObject;
    	var CanClose: Boolean);
	begin
    D64ExplorerMainDMod.CloseQuery(CanClose);
	end;

procedure TD64ExplorerMainForm.FormCreate(Sender: TObject);
	begin
    FBarBmp:= TBitmap.Create;
    FTskBmp:= TBitmap.Create;
	end;

procedure TD64ExplorerMainForm.FormDestroy(Sender: TObject);
	begin
    FBarBmp.Free;
    FTskBmp.Free;
	end;

procedure TD64ExplorerMainForm.FormDropFiles(Sender: TObject;
    	const FileNames: array of String);
	begin
    D64ExplorerMainDMod.AddDropFiles(FileNames);
	end;

procedure TD64ExplorerMainForm.FormResize(Sender: TObject);
    var
    r: TRect;

    begin
    Panel5.Height:= ClientHeight - (CoolBar1.GrabWidth + 52 * 3 + 4);

	FBarBmp.Width:= ClientWidth;
	FBarBmp.Height:= 16;
    FBarBmp.Canvas.GradientFill(Coolbar3.ClientRect,
    		ARR_D64_CLR_IDX[dciOptsGrad0], ARR_D64_CLR_IDX[dciOptsGrad1],
			gdHorizontal);
	CoolBar1.Bitmap:= FBarBmp;

	CoolBar3.Bitmap:= FBarBmp;

    FTskBmp.Height:= ClientHeight;
    FTskBmp.Width:= 16;

    r:= Coolbar2.ClientRect;
//  r.Top:= r.Top + Coolbar2.GrabWidth + 8;

   	FTskBmp.Canvas.GradientFill(r,
//    		ARR_D64_CLR_IDX[dciHdrGrad0], ARR_D64_CLR_IDX[dciHdrGrad1],
    		ARR_D64_CLR_IDX[dciOptsGrad0], ARR_D64_CLR_IDX[dciOptsGrad1],
    	   	gdVertical);

//  r:= Rect(0, 0, r.Right, Coolbar2.GrabWidth + 8);
//  FTskBmp.Canvas.Brush.Style:= bsSolid;
//  FTskBmp.Canvas.Brush.Color:= ARR_D64_CLR_IDX[dciHdrGrad1];
//  FTskBmp.Canvas.FillRect(r);

    Coolbar2.Bitmap:= FTskBmp;

    Panel1.ParentBackground:= False;
    Panel1.ParentBackground:= True;

    Panel5.ParentBackground:= False;
    Panel5.ParentBackground:= True;

    Panel7.Color:= ARR_D64_CLR_IDX[dciOptsGrad1];
	end;

procedure TD64ExplorerMainForm.PaintBox1Paint(Sender: TObject);
	begin
    PaintBox1.Canvas.GradientFill(PaintBox1.ClientRect,
    		ARR_D64_CLR_IDX[dciHdrGrad0], ARR_D64_CLR_IDX[dciHdrGrad1],
			gdHorizontal);

    Label1.Font.Color:= ARR_D64_CLR_IDX[dciHdrText0];
    Label1.Font.Style:= [fsBold];
	end;

procedure TD64ExplorerMainForm.Panel1Paint(Sender: TObject);
	begin
	//Panel1.Canvas.GradientFill(Panel5.ClientRect,
	//		ARR_D64_CLR_IDX[dciHdrGrad0], ARR_D64_CLR_IDX[dciHdrGrad1],
 //       	gdVertical);
	end;

procedure TD64ExplorerMainForm.Panel5Paint(Sender: TObject);
	begin
    //Panel5.Canvas.GradientFill(Panel5.ClientRect,
    //		ARR_D64_CLR_IDX[dciHdrGrad0], ARR_D64_CLR_IDX[dciHdrGrad1],
    //        gdVertical);
	end;

procedure TD64ExplorerMainForm.SpeedButton1Paint(Sender: TObject);
    var
	pt: TPoint;
    sb: TSpeedButton;
	de: TGraphicsDrawEffect;

    procedure DrawFrameRect(const AClr0, AClr1: TColor);
        begin
        sb.Canvas.Pen.Style:= psSolid;
        sb.Canvas.Pen.Width:= 1;
        sb.Canvas.Brush.Style:= bsClear;

        sb.Canvas.MoveTo(0, sb.ClientRect.Bottom - 1);
        sb.Canvas.Pen.Color:= AClr1;

        sb.Canvas.LineTo(0, 0);
        sb.Canvas.LineTo(sb.ClientRect.Right, 0);

        sb.Canvas.Pen.Color:= AClr0;

        sb.Canvas.MoveTo(sb.ClientRect.Right - 2, 1);
        sb.Canvas.LineTo(sb.ClientRect.Right - 2, sb.ClientRect.Bottom - 2);
        sb.Canvas.LineTo(1, sb.ClientRect.Bottom - 2);

        sb.Canvas.MoveTo(0, sb.ClientRect.Bottom - 1);
        sb.Canvas.Pen.Color:= AClr0;

        sb.Canvas.LineTo(sb.ClientRect.Right - 1, sb.ClientRect.Bottom - 1);
        sb.Canvas.LineTo(sb.ClientRect.Right - 1, 0);

        sb.Canvas.Pen.Color:= AClr1;

        sb.Canvas.MoveTo(sb.ClientRect.Right - 2, 1);
        sb.Canvas.LineTo(1, 1);
        sb.Canvas.LineTo(1, sb.ClientRect.Bottom - 2);
        end;

    begin
    sb:= Sender as TSpeedButton;

    if  sb.MouseInClient then
        begin
//      sb.Canvas.Brush.Color:= clMenuHighlight;
//    	sb.Canvas.Brush.Color:= clActiveCaption;
//		sb.Canvas.Brush.Style:= bsSolid;
        sb.Canvas.GradientFill(sb.ClientRect,
        		ARR_D64_CLR_IDX[dciItmHotGrad0], ARR_D64_CLR_IDX[dciItmHotGrad1],
                gdVertical);

        if  sb.Down then
        	begin
//        	sb.Canvas.Pen.Color:= ARR_D64_CLR_IDX[dciItmActvGrad1];
            DrawFrameRect(ARR_D64_CLR_IDX[dciItmActvGrad1], ARR_D64_CLR_IDX[dciItmActvGrad0]);
            pt.x:= 4;
    		pt.y:= 4;
        	end
        else
        	begin
//        	sb.Canvas.Pen.Color:= ARR_D64_CLR_IDX[dciItmHotGrad1];
            DrawFrameRect(ARR_D64_CLR_IDX[dciItmHotGrad0], ARR_D64_CLR_IDX[dciItmHotGrad1]);
	  		pt.x:= 2;
  			pt.y:= 2;
            end;

//      sb.Canvas.Pen.Style:= psSolid;
//      sb.Canvas.Brush.Style:= bsClear;
//      sb.Canvas.Rectangle(sb.ClientRect);
        end
    else if  sb.Down then
    	begin
		sb.Canvas.Brush.Style:= bsSolid;

//      if  sb.Enabled then
//          begin
//          sb.Canvas.Brush.Color:= clMenuBar
//          sb.Canvas.Brush.Color:= clMenuHighlight;
//          sb.Canvas.FillRect(sb.ClientRect);
			sb.Canvas.GradientFill(sb.ClientRect,
            		ARR_D64_CLR_IDX[dciItmActvGrad0], ARR_D64_CLR_IDX[dciItmActvGrad1],
                    gdVertical);

//          sb.Canvas.Pen.Color:= ARR_D64_CLR_IDX[dciItmActvGrad1];
//          sb.Canvas.Pen.Style:= psSolid;
//          sb.Canvas.Brush.Style:= bsClear;
//          sb.Canvas.Rectangle(sb.ClientRect);
			DrawFrameRect(ARR_D64_CLR_IDX[dciItmActvGrad0], ARR_D64_CLR_IDX[dciItmActvGrad1]);

            sb.Canvas.Brush.Color:= clMenuHighlight;
//          end
//      else
//       	begin
//          sb.Canvas.Brush.Color:= clInactiveCaption;
//          sb.Canvas.FillRect(sb.ClientRect);
//          end;

//		sb.Canvas.GradientFill(sb.ClientRect, clMenuHighlight,
//				sb.Canvas.Brush.Color, gdVertical);

		pt.x:= 4;
		pt.y:= 4;
        end
	else
       	begin
//      sb.Canvas.Brush.Color:= clInactiveCaption;
        sb.Canvas.Brush.Color:= clNone;
        sb.Canvas.Brush.Style:= bsClear;

        sb.Canvas.Clear;
//      end;

//		sb.Canvas.Brush.Style:= bsSolid;
//		sb.Canvas.FillRect(sb.ClientRect);

		pt.x:= 2;
		pt.y:= 2;
		end;

	if  not sb.Enabled then
		begin
		de:= gdeDisabled;
		end
	else
//		if sb.MouseInClient
//		and not sb.Down then
//			de:= gdeHighlighted
//		else
			de:= gdeNormal;

	sb.Images.Draw(sb.Canvas, pt.X, pt.Y, sb.ImageIndex, de);
    end;

procedure TD64ExplorerMainForm.SpeedButton3Paint(Sender: TObject);
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


end.

