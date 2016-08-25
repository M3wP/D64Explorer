//------------------------------------------------------------------------------
//DModD64ExplorerMain
//====================
//Main Data Module for the D64 Explorer application.  Controls most aspects of
//the application.
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
unit DModD64ExplorerMain;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, FileUtil, Menus, ActnList, Dialogs, FrameD64ExplorerMain,
    C64D64Image;

type

{ TD64ExplorerMainDMod }

	TD64ExplorerMainDMod = class(TDataModule)
        ActFileExit: TAction;
        ActFileOpen: TAction;
        ActHelpAbout: TAction;
        ActViewDirView: TAction;
        ActViewBAMView: TAction;
        ActViewSecView: TAction;
        ActionList1: TActionList;
        MainMenu1: TMainMenu;
        MenuItem1: TMenuItem;
        MenuItem10: TMenuItem;
        MenuItem2: TMenuItem;
        MenuItem3: TMenuItem;
        MenuItem4: TMenuItem;
        MenuItem5: TMenuItem;
        MenuItem6: TMenuItem;
        MenuItem7: TMenuItem;
        MenuItem8: TMenuItem;
        MenuItem9: TMenuItem;
        OpenDialog1: TOpenDialog;
        procedure ActFileExitExecute(Sender: TObject);
        procedure ActFileOpenExecute(Sender: TObject);
        procedure ActHelpAboutExecute(Sender: TObject);
        procedure ActionList1Update(AAction: TBasicAction;
                var Handled: Boolean);
        procedure ActViewBAMViewExecute(Sender: TObject);
        procedure ActViewDirViewExecute(Sender: TObject);
        procedure ActViewSecViewExecute(Sender: TObject);
        procedure DataModuleCreate(Sender: TObject);
        procedure DataModuleDestroy(Sender: TObject);
  	private
    	FD64Image: TD64Image;
        FMainFrame: TD64ExplorerMainFrame;

    	procedure ClearMainForm;
        procedure InitialiseMainForm;

    public
        procedure ApplicationClose;

        property D64Image: TD64Image read FD64Image;
	end;

var
    D64ExplorerMainDMod: TD64ExplorerMainDMod;

implementation

{$R *.lfm}

uses
	Controls, Forms, D64ExplorerStrs, FormD64ExplorerMain, FormD64SectorView,
    FormD64BAMView, FormD64DirectoryView, FormD64ExplorerAbout;

resourcestring
    STR_CAP_D64EXPLORER = ' - D64 Explorer';


{ TD64ExplorerMainDMod }

procedure TD64ExplorerMainDMod.ActFileOpenExecute(Sender: TObject);
	begin
  	if  OpenDialog1.Execute then
      	begin
        ClearMainForm;

        if  Assigned(FD64Image) then
            FreeAndNil(FD64Image);

        try
//dengland  If I use the file open version of the constructor and the file is
//          share locked, then the runtime fails and the program aborts.  This
//          is despite 3 layers of trying to protect the application from errors.
            FD64Image:= TD64Image.Create;
            FD64Image.LoadFromFile(OpenDialog1.FileName);

            InitialiseMainForm;

            Application.Title:= ExtractFileName(OpenDialog1.FileName) +
                    STR_CAP_D64EXPLORER;
            D64ExplorerMainForm.Caption:= ExtractFileName(OpenDialog1.FileName)+
                    STR_CAP_D64EXPLORER;

            except
            on E: Exception do
                MessageDlg(STR_CAP_D64EXCEPTION, E.Message, mtError, [mbOk], -1);
            end;
        end;
	end;

procedure TD64ExplorerMainDMod.ActHelpAboutExecute(Sender: TObject);
    begin
    if  not Assigned(D64ExplorerAboutForm) then
        Application.CreateForm(TD64ExplorerAboutForm, D64ExplorerAboutForm);

    D64ExplorerAboutForm.ShowModal;
    end;

procedure TD64ExplorerMainDMod.ActionList1Update(AAction: TBasicAction;
        var Handled: Boolean);
    begin
    if  Assigned(FD64Image) then
        begin
        ActViewSecView.Enabled:= True;
        ActViewBAMView.Enabled:= FD64Image.ValidVersion;
        ActViewDirView.Enabled:= FD64Image.ValidVersion;
        end
    else
        begin
        ActViewSecView.Enabled:= False;
        ActViewBAMView.Enabled:= False;
        ActViewDirView.Enabled:= False;
        end;
    end;

procedure TD64ExplorerMainDMod.ActViewBAMViewExecute(Sender: TObject);
    begin
    if  not Assigned(D64BAMViewForm) then
        Application.CreateForm(TD64BAMViewForm, D64BAMViewForm)
    else
    	D64BAMViewForm.Show;
    end;

procedure TD64ExplorerMainDMod.ActViewDirViewExecute(Sender: TObject);
    begin
    if  not Assigned(D64DirectoryViewForm) then
        Application.CreateForm(TD64DirectoryViewForm, D64DirectoryViewForm)
    else
    	D64DirectoryViewForm.Show;
    end;

procedure TD64ExplorerMainDMod.ActViewSecViewExecute(Sender: TObject);
	begin
    if  not Assigned(D64SectorViewForm) then
        Application.CreateForm(TD64SectorViewForm, D64SectorViewForm)
    else
    	D64SectorViewForm.Show;
    end;

procedure TD64ExplorerMainDMod.DataModuleCreate(Sender: TObject);
    begin
    ClearMainForm;
    end;

procedure TD64ExplorerMainDMod.DataModuleDestroy(Sender: TObject);
	begin
    if  Assigned(FD64Image) then
    	FD64Image.Free;
	end;

procedure TD64ExplorerMainDMod.ClearMainForm;
    begin
    if  Assigned(D64SectorViewForm) then
        D64SectorViewForm.Close;

    if  Assigned(D64BAMViewForm) then
        D64BAMViewForm.Close;

    if  Assigned(D64DirectoryViewForm) then
        D64DirectoryViewForm.Close;

    with D64ExplorerMainForm do
        begin
        LblDOSVersion.Caption:= EmptyStr;
        LblDOSType.Caption:= EmptyStr;
        LblDiskName.Caption:= EmptyStr;
        LblDiskID.Caption:= EmptyStr;
        LblNumTracks.Caption:= EmptyStr;
        LblGEOSVer.Caption:= EmptyStr;
        end;

    if  Assigned(FMainFrame) then
        begin
        FMainFrame.Visible:= False;
        FMainFrame.Parent:= nil;
        FMainFrame.Free;
        FMainFrame:= nil;
        end;

    D64ExplorerMainForm.PnlDetails.Visible:= False;
    end;

procedure TD64ExplorerMainDMod.InitialiseMainForm;
    begin
    with D64ExplorerMainForm do
        begin
        if  FD64Image.ValidVersion then
            LblDOSVersion.Caption:= Format('$%2.2x', [FD64Image.DOSVersion])
        else
            LblDOSVersion.Caption:= STR_LBL_D64DOSINVALID;

        LblDOSType.Caption:= FD64Image.DOSType;
        LblDiskName.Caption:= FD64Image.DiskName;
        LblDiskID.Caption:= FD64Image.DiskID;
        LblNumTracks.Caption:= IntToStr(FD64Image.TrackCount);

        if  FD64Image.GEOSDisk then
            LblGEOSVer.Caption:= STR_LBL_D64YES + ' (' +
                    STR_LBL_D64VERSION + ' ' +
                    IntToStr(FD64Image.GEOSVerMajor) + '.' +
                    IntToStr(FD64Image.GEOSVerMinor) + ')'
        else
            LblGEOSVer.Caption:= STR_LBL_D64NO;

        PnlDetails.Visible:= True;
        end;

    FMainFrame:= TD64ExplorerMainFrame.Create(Self);
    FMainFrame.InitialiseDisplay;

    FMainFrame.Parent:= D64ExplorerMainForm;
    FMainFrame.Align:= alClient;
    end;

procedure TD64ExplorerMainDMod.ApplicationClose;
    begin
    if  Assigned(FMainFrame) then
        begin
//dengland Doing this causes nasty things to happen so I'm avoiding it and the
//      control seems to have a bitmap by default, anyway.
//      if  Assigned(FMainFrame.ImgGEOSIcon.Picture.Bitmap) then
//          FMainFrame.ImgGEOSIcon.Picture.Clear;

        FMainFrame.Visible:= False;
        FMainFrame.Parent:= nil;
        FreeAndNil(FMainFrame);
        end;
    end;

procedure TD64ExplorerMainDMod.ActFileExitExecute(Sender: TObject);
	begin
    Application.Terminate;
	end;

end.

