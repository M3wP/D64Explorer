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

{$mode delphi}{$H+}

interface

uses
	Classes, SysUtils, FileUtil, Forms, Menus, ActnList, Dialogs, Controls,
    Graphics, IniFiles, C64D64Image, Generics.Collections, D64ExplorerTypes,
	FrameD64ExplorerTask, ExtCtrls, ComCtrls, Buttons;

type
    TMenuItemArray = array of TMenuItem;

	TD64FileInst = class(TD64File)
	protected
		FActiveTask: TD64ExplorerTaskFrame;
		FPanel: TPanel;
	public
		property  ActiveTask: TD64ExplorerTaskFrame read FActiveTask write FActiveTask;
        property  OnDirtyChange;
	end;

   TD64Files = TObjectList<TD64FileInst>;

{ TD64ExplorerMainDMod }

	TD64ExplorerMainDMod = class(TDataModule)
        ActFileExit: TAction;
        ActFileOpen: TAction;
        ActHelpAbout: TAction;
        ActFileSave: TAction;
        ActFileSaveAs: TAction;
        ActFileClose: TAction;
        ActFileNew: TAction;
        ActTaskBAMView: TAction;
        ActTaskDirView: TAction;
        ActTaskSectView: TAction;
        ActTaskLibrary: TAction;
        ActTaskFInspector: TAction;
        ActTaskFManager: TAction;
        ActViewDirView: TAction;
        ActViewBAMView: TAction;
        ActViewSecView: TAction;
        ActionList1: TActionList;
        ImgLstButtons: TImageList;
        ImgLstDisks: TImageList;
        ImgLstTasks: TImageList;
        ImgLstActionsMed: TImageList;
        ImgLstActionsSmall: TImageList;
        MainMenu1: TMainMenu;
        MenuItem1: TMenuItem;
        MenuItem10: TMenuItem;
        MenuItem11: TMenuItem;
        MenuItem12: TMenuItem;
        MenuItem13: TMenuItem;
        MenuItem14: TMenuItem;
        MenuItem15: TMenuItem;
        MenuItem16: TMenuItem;
        MenuItem17: TMenuItem;
        MenuItem18: TMenuItem;
        MenuItem19: TMenuItem;
        MenuItem2: TMenuItem;
        MenuItem20: TMenuItem;
        MenuItem21: TMenuItem;
        MenuItem22: TMenuItem;
        MenuItem3: TMenuItem;
        MenuItem4: TMenuItem;
        MenuItem5: TMenuItem;
        MenuItem6: TMenuItem;
        MenuItem7: TMenuItem;
        MenuItem8: TMenuItem;
        MenuItem9: TMenuItem;
        OpenDialog1: TOpenDialog;
        OpenDialog2: TOpenDialog;
        SaveDialog1: TSaveDialog;
        procedure ActFileCloseExecute(Sender: TObject);
        procedure ActFileCloseUpdate(Sender: TObject);
        procedure ActFileExitExecute(Sender: TObject);
        procedure ActFileNewExecute(Sender: TObject);
        procedure ActFileOpenExecute(Sender: TObject);
        procedure ActFileSaveAsExecute(Sender: TObject);
        procedure ActFileSaveAsUpdate(Sender: TObject);
        procedure ActFileSaveExecute(Sender: TObject);
        procedure ActFileSaveUpdate(Sender: TObject);
        procedure ActHelpAboutExecute(Sender: TObject);
        procedure ActionList1Update(AAction: TBasicAction;
                var Handled: Boolean);
		procedure ActionTest1Execute(Sender: TObject);
		procedure ActionTest2Execute(Sender: TObject);
        procedure ActMngrToggleSetExecute(Sender: TObject);
        procedure ActTaskBAMViewExecute(Sender: TObject);
        procedure ActTaskBAMViewUpdate(Sender: TObject);
        procedure ActTaskDirViewExecute(Sender: TObject);
        procedure ActTaskDirViewUpdate(Sender: TObject);
        procedure ActTaskFInspectorExecute(Sender: TObject);
        procedure ActTaskFInspectorUpdate(Sender: TObject);
        procedure ActTaskFManagerExecute(Sender: TObject);
        procedure ActTaskFManagerUpdate(Sender: TObject);
        procedure ActTaskLibraryExecute(Sender: TObject);
        procedure ActTaskSectViewExecute(Sender: TObject);
        procedure ActTaskSectViewUpdate(Sender: TObject);
        procedure ActViewBAMViewExecute(Sender: TObject);
        procedure ActViewDirViewExecute(Sender: TObject);
        procedure ActViewSecViewExecute(Sender: TObject);
        procedure DataModuleCreate(Sender: TObject);
        procedure DataModuleDestroy(Sender: TObject);
  	private
        FIniFile: TIniFile;

		FCurrD64File: Integer;
		FD64Files: TD64Files;

        FInjectOpen: string;
        FDropFiles: TStringList;
        FUpdThread: TThread;

		FTaskFrames: TTaskFramesList;
		FFreeFrames: TTaskFramesList;
//      FActiveTask: TD64ExplorerTaskFrame;

		FLastAct: TAction;

    	procedure ClearMainForm;
        procedure InitialiseMainForm;
        procedure SetApplicationTitle;

        procedure SetDirty(const AValue: Boolean);

        procedure SetSaveDialogTypes;

		procedure HandleIdle(Sender: TObject; var Done: Boolean);

    protected
        procedure ProcessOpenFiles;
       	procedure ProcessDropFiles;
		procedure ProcessFreeFrames;

		procedure DoCreateInstBar(const AInst: TD64FileInst);
        procedure DoSetInstBarCaption(const AInst: TD64FileInst);

        procedure DoInstDirtyChange(ASender: TObject);

		procedure DoBarButtonClick(ASender: TObject);
		procedure DoBarButtonPaint(ASender: TObject);
		procedure ToggleBarDown;

    public
        procedure ApplicationClose;
        procedure CloseQuery(var ACanClose: Boolean);
        procedure AddDropFiles(const AFiles: array of string); overload;
        procedure AddDropFiles(const AFiles: TStrings); overload;

        procedure BindToCoolbar(const AControl: TControl;
        		const ABreak: Boolean = True); overload;
        procedure BindToCoolbar(const AControl: TControl;
        		ACoolbar: TCoolbar; const ABreak: Boolean = True); overload;
        procedure UnbindToCoolbar(const AControl: TControl;
				const AParent: TWinControl); overload;
        procedure UnbindToCoolbar(const AControl: TControl; ACoolbar: TCoolbar;
				const AParent: TWinControl); overload;
        procedure UpdateMenuVisibility;
        procedure MoveMenuItems(const AItems: array of TMenuItem;
                var AMenus: TMenuItemArray);
        procedure RetrieveMenuItems(const AItems: array of TMenuItem;
                const AMenus: TMenuItemArray; const ATarget: TPopupMenu);

        procedure EnableFileDrop(const AEnable: Boolean);

        procedure CreateActivateTask(const ATaskClass: TD64ExplorerTaskFrameClass;
				const AInst: TD64FileInst);
        procedure AddRecentUsed(const AFileName: string);
        procedure OpenDiskImageFile(const AFileName: string);

        procedure UpdateTasks;
        procedure RequestSaveDataCallback(const ATask: TD64ExplorerTaskFrame);

		procedure RezizeFilesBar(const AAuto: Boolean = True);

		property  CurrD64File: Integer read FCurrD64File;

	end;

var
    D64ExplorerMainDMod: TD64ExplorerMainDMod;

implementation

{$R *.lfm}

uses
	Types, GraphType,
    D64ExplorerConsts, D64ExplorerUtils, D64ExplorerStrs,
    FormD64ExplorerMain,
    FormD64ExplorerAbout, FormD64ExplorerNewDisk,

    FrameD64ExplorerLibrary, FrameD64ExplorerManage, FrameD64ExplorerMain,

    FormD64SectorView, FormD64BAMView, FormD64DirectoryView;

resourcestring
    STR_CAP_D64EXPLORER = ' - D64 Explorer';


type

    { TUpdateThread }

    TUpdateThread = class(TThread)
    protected
        procedure Execute; override;
        procedure CheckDropFiles;
        procedure CheckOpenFiles;
		procedure CheckFreeFrames;

    end;

{ TUpdateThread }

procedure TUpdateThread.Execute;
	begin
    while not Terminated do
        begin
        Sleep(30);

        if  not Terminated then
        	Synchronize(CheckDropFiles);

        Sleep(30);

        if  not Terminated then
        	Synchronize(CheckOpenFiles);

        Sleep(30);

        if  not Terminated then
        	Synchronize(CheckFreeFrames);
        end;
    end;

procedure TUpdateThread.CheckDropFiles;
	begin
    D64ExplorerMainDMod.ProcessDropFiles;
	end;

procedure TUpdateThread.CheckOpenFiles;
	begin
    D64ExplorerMainDMod.ProcessOpenFiles;
	end;

procedure TUpdateThread.CheckFreeFrames;
	begin
	D64ExplorerMainDMod.ProcessFreeFrames;
	end;

{ TD64ExplorerMainDMod }

procedure TD64ExplorerMainDMod.ActFileOpenExecute(Sender: TObject);
    var
	img: TD64Image;
	fn: string;
	inst: TD64FileInst;

	begin
	//if  FDirty then
 //   	if  MessageDlg('Confirm discard', 'There are unsaved changes.  Are you ' +
 //       		'sure you wish to discard them and open another file?', mtWarning,
 //               [mbYes, mbNo], 0, mbNo) = mrNo then
 //       	Exit;

//!!!FIXME
//	Check that the file isn't already open, if open switch to?

    if  (Length(FInjectOpen) > 0)
    or  OpenDialog1.Execute then
      	begin
        //FDirty:= False;

        img:= TD64Image.Create;
        try
//dengland  If I use the file open version of the constructor and the file is
//          share locked, then the runtime fails and the program aborts.  This
//          is despite 3 layers of trying to protect the application from errors.

            if  Length(FInjectOpen) > 0 then
                fn:= FInjectOpen
            else
            	fn:= OpenDialog1.FileName;

            FInjectOpen:= EmptyStr;

            img.LoadFromFile(fn);

            inst:= TD64FileInst.Create;
			inst.D64Image:= img;
			inst.FileName:= fn;

            inst.OnDirtyChange:= DoInstDirtyChange;

			FD64Files.Add(inst);
			FCurrD64File:= FD64Files.Count - 1;

			DoCreateInstBar(inst);
			inst.FPanel.Controls[0].Tag:= FCurrD64File;

            SetApplicationTitle;

//          InitialiseMainForm;
			ClearMainForm;

//          if  Assigned(FLastAct) then
//           	FLastAct.Execute
//          else
            	ActTaskFManager.Execute;

//          SetApplicationTitle(QuotedStr(FD64FileName));
            AddRecentUsed(fn);

            except
            on E: Exception do
                MessageDlg(STR_CAP_D64EXCEPTION, E.Message, mtError, [mbOk], -1);
            end;
        end;
	end;

procedure TD64ExplorerMainDMod.ActFileSaveAsExecute(Sender: TObject);
	begin
    SetSaveDialogTypes;

    if  SaveDialog1.Execute then
        begin
        FD64Files[FCurrD64File].FileName:= SaveDialog1.FileName;
        ActFileSave.Execute;
        end;
    end;

procedure TD64ExplorerMainDMod.ActFileSaveAsUpdate(Sender: TObject);
	begin
    ActFileSaveAs.Enabled:=  FCurrD64File > -1;//Assigned(FD64Image);
	end;

procedure TD64ExplorerMainDMod.ActFileSaveExecute(Sender: TObject);
	begin
    if  FD64Files[FCurrD64File].FileName = EmptyStr then
        begin
        SetSaveDialogTypes;

        if  SaveDialog1.Execute then
	        FD64Files[FCurrD64File].FileName:= SaveDialog1.FileName
        else
            Exit;
        end;

    FD64Files[FCurrD64File].D64Image.SaveToFile(FD64Files[FCurrD64File].FileName);
    FD64Files[FCurrD64File].Dirty:= False;

    //SetApplicationTitle(QuotedStr(FD64FileName));

    AddRecentUsed(FD64Files[FCurrD64File].FileName);
	end;

procedure TD64ExplorerMainDMod.ActFileSaveUpdate(Sender: TObject);
	begin
	try
    	ActFileSave.Enabled:= (FCurrD64File > -1) and
				FD64Files[FCurrD64File].Dirty;// Assigned(FD64Image) and FDirty;
		except

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
	try
	    if  FCurrD64File > -1 then
	        begin
	        ActViewSecView.Enabled:= True;
	        ActViewBAMView.Enabled:= FD64Files[FCurrD64File].D64Image.ValidVersion;
	        ActViewDirView.Enabled:= FD64Files[FCurrD64File].D64Image.ValidVersion;
	        end
	    else
	        begin
	        ActViewSecView.Enabled:= False;
	        ActViewBAMView.Enabled:= False;
	        ActViewDirView.Enabled:= False;
	        end;

		except
		end;
	end;

procedure TD64ExplorerMainDMod.ActionTest1Execute(Sender: TObject);
    var
	f: TD64ExplorerManageFrame;

	begin
	f:= TD64ExplorerManageFrame.Create(Application);
	f.Parent:= D64ExplorerMainForm.Panel3;
	f.Align:= alClient;
    f.Visible:= True;
	f.BringToFront;

	f.Prepare(nil);
//	f.Initialise;

	FTaskFrames.Add(f);
	end;

procedure TD64ExplorerMainDMod.ActionTest2Execute(Sender: TObject);
	begin
//    while FTaskFrames.Count > 0 do
		begin
		FTaskFrames[FTaskFrames.Count - 1].Unprepare;
		FTaskFrames.Delete(FTaskFrames.Count - 1);
		end;
	end;


procedure TD64ExplorerMainDMod.ActMngrToggleSetExecute(Sender: TObject);
	begin
//
	end;

procedure TD64ExplorerMainDMod.ActTaskBAMViewExecute(Sender: TObject);
	begin
//
	end;

procedure TD64ExplorerMainDMod.ActTaskBAMViewUpdate(Sender: TObject);
	begin
    ActTaskBAMView.Enabled:= FCurrD64File > -1;//(FD64Image);
	end;

procedure TD64ExplorerMainDMod.ActTaskDirViewExecute(Sender: TObject);
	begin
//
	end;

procedure TD64ExplorerMainDMod.ActTaskDirViewUpdate(Sender: TObject);
	begin
    ActTaskDirView.Enabled:= FCurrD64File > -1;//(FD64Image);
	end;

procedure TD64ExplorerMainDMod.ActTaskFInspectorExecute(Sender: TObject);
	begin
    FLastAct:= ActTaskFInspector;

    CreateActivateTask(TD64ExplorerMainFrame, FD64Files[FCurrD64File]);
    D64ExplorerMainForm.SpeedButton7.Down:= True;
	end;

procedure TD64ExplorerMainDMod.ActTaskFInspectorUpdate(Sender: TObject);
	begin
    ActTaskFInspector.Enabled:= FCurrD64File > -1;//(FD64Image);
	end;

procedure TD64ExplorerMainDMod.ActTaskFManagerExecute(Sender: TObject);
	begin
    FLastAct:= ActTaskFManager;

    CreateActivateTask(TD64ExplorerManageFrame, FD64Files[FCurrD64File]);
    D64ExplorerMainForm.SpeedButton2.Down:= True;
	end;

procedure TD64ExplorerMainDMod.ActTaskFManagerUpdate(Sender: TObject);
	begin
    ActTaskFManager.Enabled:= FCurrD64File > -1;//Assigned(FD64Image);
	end;

procedure TD64ExplorerMainDMod.ActTaskLibraryExecute(Sender: TObject);
	begin
    FLastAct:= ActTaskLibrary;

    CreateActivateTask(TD64ExplorerLibraryFrame, nil);
    D64ExplorerMainForm.SpeedButton1.Down:= True;
	end;

procedure TD64ExplorerMainDMod.ActTaskSectViewExecute(Sender: TObject);
	begin
//
	end;

procedure TD64ExplorerMainDMod.ActTaskSectViewUpdate(Sender: TObject);
	begin
    ActTaskSectView.Enabled:= FCurrD64File > -1;//(FD64Image);
	end;

procedure TD64ExplorerMainDMod.ActViewBAMViewExecute(Sender: TObject);
    begin
    if  not Assigned(D64BAMViewForm) then
        Application.CreateForm(TD64BAMViewForm, D64BAMViewForm);

	D64BAMViewForm.D64File:= FD64Files[FCurrD64File];
	D64BAMViewForm.Show;
    end;

procedure TD64ExplorerMainDMod.ActViewDirViewExecute(Sender: TObject);
    begin
    if  not Assigned(D64DirectoryViewForm) then
        Application.CreateForm(TD64DirectoryViewForm, D64DirectoryViewForm);

   	D64DirectoryViewForm.D64File:= FD64Files[FCurrD64File];
   	D64DirectoryViewForm.Show;
    end;

procedure TD64ExplorerMainDMod.ActViewSecViewExecute(Sender: TObject);
	begin
    if  not Assigned(D64SectorViewForm) then
        Application.CreateForm(TD64SectorViewForm, D64SectorViewForm);

	D64SectorViewForm.D64File:= FD64Files[FCurrD64File];
   	D64SectorViewForm.Show;
    end;

procedure TD64ExplorerMainDMod.DataModuleCreate(Sender: TObject);
    begin
	Application.OnIdle:= HandleIdle;

    FCurrD64File:= -1;
	FD64Files:= TD64Files.Create(False);
	FTaskFrames:= TTaskFramesList.Create(False);
	FFreeFrames:= TTaskFramesList.Create(True);

    FDropFiles:= TStringList.Create;

    FIniFile:= TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
    FIniFile.CacheUpdates:= False;

//  FOldIdle:= Application.OnIdle;
//  Application.OnIdle:= DoOnIdle;

    ClearMainForm;
	ActTaskLibrary.Execute;

    FUpdThread:= TUpdateThread.Create(True);
    FUpdThread.FreeOnTerminate:= False;
    FUpdThread.Suspended:= False;
	end;

procedure TD64ExplorerMainDMod.DataModuleDestroy(Sender: TObject);
    begin
    //if  Assigned(FD64Image) then
    //	FD64Image.Free;

	FUpdThread.Terminate;

    while FTaskFrames.Count > 0 do
		begin
//		FFreeFrames.Add(FTaskFrames[FTaskFrames.Count - 1]);
		if  FTaskFrames[FTaskFrames.Count - 1].Prepared then
			FTaskFrames[FTaskFrames.Count - 1].Unprepare;

		FTaskFrames[FTaskFrames.Count - 1].Free;
		FTaskFrames.Delete(FTaskFrames.Count - 1);
		end;

	FUpdThread.WaitFor;
    FUpdThread.Free;

    FTaskFrames.Free;
	FFreeFrames.Free;

    FIniFile.Free;
    end;

procedure TD64ExplorerMainDMod.ClearMainForm;
    var
    i: Integer;

    begin
	D64ExplorerMainForm.Canvas.LockCanvas;
	try
	 //   if  (FCurrD64File > -1)
		//and Assigned(FD64Files[FCurrD64File].ActiveTask) then
	 //       begin
	 //       FD64Files[FCurrD64File].ActiveTask.Unprepare;
	 //       FD64Files[FCurrD64File].ActiveTask:= nil;
	 //       end;

	    for i:= FTaskFrames.Count - 1 downto 0 do
	        if  not (FTaskFrames[i] is TD64ExplorerLibraryFrame) then
	            begin
	            FTaskFrames[i].SaveData(FIniFile);

	            FTaskFrames[i].Visible:= False;

				if  FTaskFrames[i].Prepared then
					FTaskFrames[i].Unprepare;

	            FTaskFrames[i].Parent:= nil;

				FFreeFrames.Add(FTaskFrames[i]);
//	            FTaskFrames[i].Free;
//	            FTaskFrames[i]:= nil;

	            FTaskFrames.Delete(i);
	            end
            else
            	begin
	            FTaskFrames[i].Visible:= False;
                end;

        for i:= 0 to FD64Files.Count - 1 do
			FD64Files[i].ActiveTask:= nil;

	    if  Assigned(D64SectorViewForm) then
	        D64SectorViewForm.Close;

	    if  Assigned(D64BAMViewForm) then
	        D64BAMViewForm.Close;

	    if  Assigned(D64DirectoryViewForm) then
	        D64DirectoryViewForm.Close;

        finally
		D64ExplorerMainForm.Canvas.UnlockCanvas;
		end;

//	D64ExplorerMainForm.Invalidate;
//	Application.ProcessMessages;

 //   if  FD64Files.Count > 0 then
	//	ActTaskFManager.Execute
	//else
	//	ActTaskLibrary.Execute;

//    SetApplicationTitle('');
    end;

procedure TD64ExplorerMainDMod.InitialiseMainForm;
    begin
//  FMainFrame:= TD64ExplorerMainFrame.Create(Self);
//  FMainFrame.InitialiseDisplay;

//  FMainFrame.Parent:= D64ExplorerMainForm;
//  FMainFrame.Align:= alClient;

//  ActTaskFManager.Enabled:= True;
//  ActTaskFInspector.Enabled:= True;

    ActTaskFManager.Execute;
    end;

procedure TD64ExplorerMainDMod.SetApplicationTitle;
	var
  	s: string;

    begin
    if  FD64Files[FCurrD64File].FileName = '' then
        s:= '[untitled]'
   	else
        s:= FD64Files[FCurrD64File].FileName;

    s:= s + STR_CAP_D64EXPLORER;

    if  FD64Files[FCurrD64File].Dirty then
        s:= '* ' + s;

    Application.Title:= s;
    D64ExplorerMainForm.Caption:= s;
    end;

procedure TD64ExplorerMainDMod.SetDirty(const AValue: Boolean);
	begin
//  if  (not FDirty)
//  and AValue then
//      begin
        //FDirty:= AValue;
        //SetApplicationTitle(QuotedStr(FD64Files[FCurrD64File].FileName));
//      end;

//  FDirty:= AValue;
	end;

procedure TD64ExplorerMainDMod.SetSaveDialogTypes;
	begin
    SaveDialog1.Title:= 'Save D64 Image File...';

    if  FD64Files[FCurrD64File].D64Image.DiskType = ddt1541 then
        begin
        SaveDialog1.Filter:= 'D64 Image Files (*.d64)|*.d64';
        SaveDialog1.DefaultExt:= '.d64';
        end
    else if FD64Files[FCurrD64File].D64Image.DiskType = ddt1571 then
        begin
        SaveDialog1.Filter:= 'D71 Image Files (*.d71)|*.d71';
        SaveDialog1.DefaultExt:= '.d71';
        end
    else
	    begin
    	SaveDialog1.Filter:= 'D81 Image Files (*.d81)|*.d81';
	    SaveDialog1.DefaultExt:= '.d81';
    	end
    end;

procedure TD64ExplorerMainDMod.HandleIdle(Sender: TObject; var Done: Boolean);
	begin
	ProcessFreeFrames;
	Done:= True;
	end;

procedure TD64ExplorerMainDMod.ProcessOpenFiles;
	begin
    if  Length(FInjectOpen) > 0 then
	    ActFileOpen.Execute;
	end;

procedure TD64ExplorerMainDMod.ProcessDropFiles;
    var
    i: Integer;
    f: Boolean;

    begin
    if  FDropFiles.Count = 0 then
        Exit;

    f:= False;

    if  (FDropFiles.Count > 0)
    and (Assigned(FD64Files[FCurrD64File].ActiveTask)) then
        begin
        i:= 0;
        f:= True;
        while (i < FDropFiles.Count) and
				FD64Files[FCurrD64File].ActiveTask.AcceptFile(FDropFiles[i]) do
			Inc(i);
        end;

    FDropFiles.Clear;

    if  f then
        UpdateTasks;
	end;

procedure TD64ExplorerMainDMod.ProcessFreeFrames;
	var
	i: Integer;

	begin
//	FFreeFrames.Clear;

	for i:= FFreeFrames.Count - 1 downto 0 do
		try
			if  FFreeFrames[i].Prepared then
				FFreeFrames[i].Unprepare;

//			FFreeFrames[i].Free;
   	    	FFreeFrames.Delete(i);
			except
			end;
	end;

procedure TD64ExplorerMainDMod.DoCreateInstBar(const AInst: TD64FileInst);
    var
    btn: TSpeedButton;

	begin
	Ainst.FPanel:=  TPanel.Create(Self);
	Ainst.FPanel.Width:= 128;
    AInst.FPanel.Constraints.MinHeight:= 24;
	AInst.FPanel.AutoSize:= True;
	Ainst.FPanel.Height:= 24;
	Ainst.FPanel.Caption:= '';
	AInst.FPanel.BevelOuter:= bvNone;

	btn:= TSpeedButton.Create(Self);
    btn.Align:= alLeft;
	btn.Parent:= AInst.FPanel;
    btn.Width:= 124;
	btn.Height:= 24;

    DoSetInstBarCaption(AInst);

	btn.Hint:= AInst.FileName;
	btn.ShowHint:= True;
	btn.Color:= clMenuBar;
	btn.GroupIndex:= -1;
    btn.AllowAllUp:= True;

//  btn.Images:= ImgLstDisks;
    btn.ImageIndex:= 0;

	btn.OnClick:= DoBarButtonClick;
	btn.OnPaint:= DoBarButtonPaint;

	RezizeFilesBar(False);
	BindToCoolbar(Ainst.FPanel, D64ExplorerMainForm.CoolBar3, False);
	RezizeFilesBar;

	ToggleBarDown;
	end;

procedure TD64ExplorerMainDMod.DoSetInstBarCaption(const AInst: TD64FileInst);
    var
    s: string;

    begin
    if AInst.FileName = '' then
	    s:= 'untitled'
    else
	    s:= ExtractFileName(AInst.FileName);
    if  AInst.Dirty then
	    s:= '* ' + s;

    TSpeedButton(AInst.FPanel.Controls[0]).Caption:= s;
    end;

procedure TD64ExplorerMainDMod.DoInstDirtyChange(ASender: TObject);
	begin
    DoSetInstBarCaption(TD64FileInst(ASender));
	end;

procedure TD64ExplorerMainDMod.DoBarButtonClick(ASender: TObject);
	begin
	FCurrD64File:= TSpeedButton(ASender).Tag;

	ToggleBarDown;

    SetApplicationTitle;

	ClearMainForm;

    if  Assigned(FLastAct) then
        FLastAct.Execute
    else
    	ActTaskFManager.Execute;
	end;

procedure TD64ExplorerMainDMod.DoBarButtonPaint(ASender: TObject);
    var
	pt: TPoint;
    sb: TSpeedButton;
//	de: TGraphicsDrawEffect;
	sz: TSize;
//	r: TRect;
	cl1,
	cl2,
    cl3: TColor;

    begin
    sb:= ASender as TSpeedButton;

//  sb.Canvas.Font.Style:= [fsBold];
    sb.Canvas.Font.Color:= ARR_D64_CLR_IDX[dciListText0];

    if  sb.Down then
    	begin
       	cl1:= ARR_D64_CLR_IDX[dciItmActvGrad0];
		cl2:= ARR_D64_CLR_IDX[dciItmActvGrad1];
        cl3:= cl2;
//	    sb.Canvas.Font.Color:= ARR_D64_CLR_IDX[dciItmText1];

		//pt.x:= 4;
		//pt.y:= 4;

        end
	else
		begin
		if  sb.MouseInClient then
			begin
       		cl1:= ARR_D64_CLR_IDX[dciItmHotGrad0];
			cl2:= ARR_D64_CLR_IDX[dciItmHotGrad1];
            cl3:= cl2;
//			sb.Canvas.Font.Color:= ARR_D64_CLR_IDX[dciItmText1];
			end
		else
			begin
       		cl1:= ARR_D64_CLR_IDX[dciItmGrad0];
			cl2:= ARR_D64_CLR_IDX[dciItmGrad1];
            cl3:= cl1;
//			sb.Canvas.Font.Color:= ARR_D64_CLR_IDX[dciItmText0];
			end;

		//pt.x:= 2;
		//pt.y:= 2;
		end;

	//if  not sb.Enabled then
	//	de:= gdeDisabled
	//else
	//	de:= gdeNormal;

//	sb.Canvas.Pen.Color:= clBtnShadow;
//	sb.Canvas.Pen.Style:= psSolid;

	sb.Canvas.Brush.Style:= bsSolid;

	sb.Canvas.GradientFill(sb.ClientRect, cl1, cl2, gdHorizontal);

    sb.Canvas.Pen.Color:= cl3;
    sb.Canvas.Pen.Style:= psSolid;
    sb.Canvas.Brush.Style:= bsClear;
    sb.Canvas.Rectangle(sb.ClientRect);

//	sb.Canvas.FillRect(sb.ClientRect);
//	sb.Canvas.RoundRect(sb.ClientRect, 4, 4);

///	sb.Canvas.Font.Style:= sb.Canvas.Font.Style + [fsBold];
	sz:= sb.Canvas.TextExtent(sb.Caption);

//	r:= sb.ClientRect;
	pt.X:= ((sb.ClientRect.Right - sb.ClientRect.Left) - sz.cx) div 2;
//	r.Right:= r.Left + sz.cx;
	pt.Y:= ((sb.ClientRect.Bottom - sb.ClientRect.Top) - sz.cy) div 2;
//	r.Bottom:= r.Top + sz.cy;

    sb.Canvas.TextRect(sb.ClientRect, pt.X, pt.Y, sb.Caption);
	ImgLstDisks.Draw(sb.Canvas, pt.X - 28, 0, sb.ImageIndex, gdeNormal);
	end;

procedure TD64ExplorerMainDMod.ToggleBarDown;
    var
	i: Integer;

	begin
	for i:= 0 to FD64Files.Count - 1 do
		TSpeedButton(FD64Files[i].FPanel.Controls[0]).Down:= FCurrD64File = i;
	end;

procedure TD64ExplorerMainDMod.ApplicationClose;
	var
	i: Integer;

    begin
    FUpdThread.Terminate;

    for i:= FTaskFrames.Count - 1 downto 0 do
        begin
        FTaskFrames[i].SaveData(FIniFile);

        FTaskFrames[i].Visible:= False;
        FTaskFrames[i].Parent:= nil;
        FTaskFrames.Delete(i);
        end;
    end;

procedure TD64ExplorerMainDMod.CloseQuery(var ACanClose: Boolean);
    var
    d: Boolean;
    i: Integer;

    begin
    ACanClose:= False;

    d:= False;
    for i:= 0 to FD64Files.Count - 1 do
        d:= d or FD64Files[i].Dirty;

	if  d then
   	if  MessageDlg('Confirm discard', 'There are unsaved changes.  Are you ' +
				'sure you wish to discard them and exit the application?', mtWarning,
                [mbYes, mbNo], 0, mbNo) = mrNo then
       	Exit;

    ACanClose:= True;
    end;

procedure TD64ExplorerMainDMod.AddDropFiles(const AFiles: array of string);
	begin
    FDropFiles.AddStrings(AFiles);
	end;

procedure TD64ExplorerMainDMod.AddDropFiles(const AFiles: TStrings);
	begin
    FDropFiles.AddStrings(AFiles);
	end;

procedure TD64ExplorerMainDMod.BindToCoolbar(const AControl: TControl;
        const ABreak: Boolean);
	begin
	BindToCoolbar(AControl, D64ExplorerMainForm.CoolBar1, ABreak);
	end;

procedure TD64ExplorerMainDMod.BindToCoolbar(const AControl: TControl;
		ACoolbar: TCoolbar; const ABreak: Boolean);
    var
    i: Integer;

    begin
    AControl.Parent:= ACoolbar;

    if  not ABreak then
        for i:= 0 to ACoolBar.Bands.Count - 1 do
            if  ACoolBar.Bands[i].Control = AControl then
        		begin
                ACoolBar.Bands[i].Break:= False;
        		end;

    AControl.Visible:= True;
    if  AControl is TPanel then
        begin
        TPanel(AControl).ParentColor:= False;
        TPanel(AControl).ParentBackground:= False;

        TPanel(AControl).ParentColor:= True;
        TPanel(AControl).ParentBackground:= True;
		end;
    end;

procedure TD64ExplorerMainDMod.UnbindToCoolbar(const AControl: TControl;
		const AParent: TWinControl);
	begin
	UnbindToCoolbar(AControl, D64ExplorerMainForm.CoolBar1, AParent);
	end;

procedure TD64ExplorerMainDMod.UnbindToCoolbar(const AControl: TControl;
		ACoolbar: TCoolbar; const AParent: TWinControl);
	begin
    AControl.Parent:= AParent;
    AControl.Visible:= False;
	end;

procedure TD64ExplorerMainDMod.UpdateMenuVisibility;
    var
    i: Integer;

    begin
    for i:= 0 to MainMenu1.Items.Count - 1 do
        MainMenu1.Items[i].Visible:= MainMenu1.Items[i].Count > 0;
	end;

procedure TD64ExplorerMainDMod.MoveMenuItems(const AItems: array of TMenuItem;
    	var AMenus: TMenuItemArray);
    var
    i,
    j: Integer;
    m: TMenuItem;
//  s1,
//  s2: string;

    begin
    SetLength(AMenus, Length(AItems));

    for i:= 0 to Length(AItems) - 1 do
        begin
        m:= nil;
        for j:= 0 to MainMenu1.Items.Count - 1 do
            begin
//          s1:= MainMenu1.Items[j].Caption;
//          s2:= AItems[i].Parent.Caption;

           	if  CompareText(MainMenu1.Items[j].Caption, AItems[i].Parent.Caption) = 0 then
                begin
                m:= MainMenu1.Items[j];
                Break;
                end;
            end;

        if  Assigned(m) then
            begin
	        AItems[i].Parent.Remove(AItems[i]);
    	    m.Add(AItems[i]);
            end;

        AMenus[i]:= m;
        end;

    UpdateMenuVisibility;
    end;

procedure TD64ExplorerMainDMod.RetrieveMenuItems(
    	const AItems: array of TMenuItem; const AMenus: TMenuItemArray;
    	const ATarget: TPopupMenu);
    var
    i,
    j: Integer;
    m: TMenuItem;

    begin
    Assert(Length(AItems) = Length(AMenus), 'Assertion failed!');

    for i:= 0 to Length(AItems) - 1 do
        begin
        m:= nil;
        for j:= 0 to ATarget.Items.Count - 1 do
        	if  CompareText(AMenus[i].Caption, ATarget.Items[j].Caption) = 0 then
                begin
                m:= ATarget.Items[j];
                Break;
                end;

        AMenus[i].Remove(AItems[i]);
        if  Assigned(m) then
            m.Add(AItems[i]);
        end;

    UpdateMenuVisibility;
    end;

procedure TD64ExplorerMainDMod.EnableFileDrop(const AEnable: Boolean);
	begin
    D64ExplorerMainForm.AllowDropFiles:= AEnable;
	end;

procedure TD64ExplorerMainDMod.CreateActivateTask(
    	const ATaskClass: TD64ExplorerTaskFrameClass; const AInst: TD64FileInst);
    var
  i: Integer;
    f: TD64ExplorerTaskFrame;
    c: Boolean;

    begin
    f:= nil;
    c:= False;

	ClearMainForm;

    for i:= 0 to FTaskFrames.Count - 1 do
        if  (FTaskFrames[i] is ATaskClass) then
// 		and (FTaskFrames[i].D64File = AInst) then
            begin
            f:= FTaskFrames[i];
			if  f.Prepared then
			   	f.Unprepare;

            Break;
            end;

    if  not Assigned(f) then
        begin
    	f:= ATaskClass.Create(Application);

        f.ParentDoubleBuffered:= False;
        f.DoubleBuffered:= True;
        f.ParentColor:= False;

        f.LoadData(FIniFile);

        c:= True;
        end;

    //if  (FCurrD64File > -1)
 	//for i:= 0 to FTaskFrames.Count - 1 do
		//if  (FTaskFrames[i] <> f)
		//and FTaskFrames[i].Prepared then
	 //       begin
	 //       FTaskFrames[i].Unprepare;
	 //       FTaskFrames[i].Visible:= False;
	 //       end;

//  D64ExplorerMainForm.Update;

//  D64ExplorerMainForm.CoolBar1.BeginUpdate;
//	D64ExplorerMainForm.Canvas.Lock;
    try
		if  not f.Prepared then
	    	f.Prepare(AInst);

        D64ExplorerMainForm.CoolBar1.Invalidate;
        D64ExplorerMainForm.CoolBar1.Repaint;

        if  c then
            begin
            FTaskFrames.Add(f);
            end;

        f.Parent:= D64ExplorerMainForm.Panel3;
        f.Align:= alClient;
        f.Visible:= True;
        f.BringToFront;
//  	f.Invalidate;

	   	f.Initialise;

	    D64ExplorerMainForm.Label1.Caption:= f.GetDescription;

    	finally
//     	D64ExplorerMainForm.Canvas.Unlock;
//      D64ExplorerMainForm.CoolBar1.EndUpdate;
    	end;

	if  Assigned(AInst) then
	   	AInst.ActiveTask:= f;

//  D64ExplorerMainForm.Repaint;
//	D64ExplorerMainForm.Invalidate;
//	Application.ProcessMessages;
	end;

procedure TD64ExplorerMainDMod.AddRecentUsed(const AFileName: string);
	begin
	if  (FTaskFrames.Count > 0)
	and (FTaskFrames[0] is TD64ExplorerLibraryFrame) then
		TD64ExplorerLibraryFrame(FTaskFrames[0]).AddRecentUsed(AFileName);
	end;

procedure TD64ExplorerMainDMod.OpenDiskImageFile(const AFileName: string);
	begin
    FInjectOpen:= AFileName;
	end;

procedure TD64ExplorerMainDMod.UpdateTasks;
    var
    i: Integer;

    begin
    for i:= 0 to FTaskFrames.Count - 1 do
        FTaskFrames[i].UpdateDisplay;
	end;

procedure TD64ExplorerMainDMod.RequestSaveDataCallback(
    	const ATask: TD64ExplorerTaskFrame);
	begin
    ATask.SaveData(FIniFile);
	end;

procedure TD64ExplorerMainDMod.RezizeFilesBar(const AAuto: Boolean);
    var
	i: Integer;
	w,
	l: Integer;

	begin
    if  FD64Files.Count > 0 then
        begin
    	w:= Trunc(D64ExplorerMainForm.CoolBar3.Width / FD64Files.Count) -
				(D64ExplorerMainForm.CoolBar3.GrabWidth + 20);

		for i:= 0 to FD64Files.Count - 1 do
			FD64Files[i].FPanel.Controls[0].Width:= w;

        l:= 0;
 		for i:= 0 to D64ExplorerMainForm.CoolBar3.Bands.Count - 1 do
			begin
			D64ExplorerMainForm.CoolBar3.Bands[i].Width:= w;
//  	    D64ExplorerMainForm.CoolBar3.Bands[i].Left:= w;
        	Inc(l, w);
			end;
    	end;

	if  AAuto then
		D64ExplorerMainForm.CoolBar3.AutosizeBands;
	end;

procedure TD64ExplorerMainDMod.ActFileExitExecute(Sender: TObject);
    var
    cc: Boolean;

    begin
    CloseQuery(cc);
    if  not cc then
        Exit;

    //FDirty:= False;
    Application.Terminate;
	end;

procedure TD64ExplorerMainDMod.ActFileNewExecute(Sender: TObject);
    var
    ss: Boolean;
    ty: TD64DiskType;
    nm,
    id: AnsiString;
	img: TD64Image;
	fn: string;
    inst: TD64FileInst;

    begin
	//if  FDirty then
 //   	if  MessageDlg('Confirm discard', 'There are unsaved changes.  Are you ' +
 //       		'sure you wish to discard them and create a new file?', mtWarning,
 //               [mbYes, mbNo], 0, mbNo) = mrNo then
 //       	Exit;

	if  not Assigned(D64ExplorerNewDiskForm) then
        Application.CreateForm(TD64ExplorerNewDiskForm, D64ExplorerNewDiskForm);

    D64ExplorerNewDiskForm.Initialise;

    if  D64ExplorerNewDiskForm.ShowModal = mrOk then
        begin
        fn:= '';
        //FDirty:= False;

        //if  Assigned(FD64Image) then
            //FreeAndNil(FD64Image);

        img:= TD64Image.Create;

        if  D64ExplorerNewDiskForm.ComboBox1.ItemIndex in [1, 2] then
            begin
            ss:= D64ExplorerNewDiskForm.ComboBox1.ItemIndex = 1;
            ty:= ddt1571;
            end
        else if D64ExplorerNewDiskForm.ComboBox1.ItemIndex = 0 then
            begin
            ss:= True;
            ty:= ddt1541;
            end
        else
        	begin
            ss:= False;
            ty:= ddt1581;
            end;

        nm:= AsciiToPetsciiString(D64ExplorerNewDiskForm.Edit1.Text);
        id:= AsciiToPetsciiString(D64ExplorerNewDiskForm.Edit2.Text);

        img.FormatImage(nm, id, ty, ss,
        		D64ExplorerNewDiskForm.CheckBox1.Checked);
        //FDirty:= True;

        inst:= TD64FileInst.Create;
		inst.D64Image:= img;
		inst.FileName:= fn;
		inst.Dirty:= True;

        inst.OnDirtyChange:= DoInstDirtyChange;

		FD64Files.Add(inst);
		FCurrD64File:= FD64Files.Count - 1;

		DoCreateInstBar(inst);
		inst.FPanel.Controls[0].Tag:= FCurrD64File;

        SetApplicationTitle;

//      InitialiseMainForm;
		ClearMainForm;

        if  Assigned(FLastAct) then
        	FLastAct.Execute
        else
        	ActTaskFManager.Execute;

        //SetApplicationTitle('Untitled');
		end;
	end;

procedure TD64ExplorerMainDMod.ActFileCloseExecute(Sender: TObject);
//	var
//	i: Integer;

	begin
	if  (FCurrD64File > -1)
	and FD64Files[FCurrD64File].Dirty then
    	if  MessageDlg('Confirm discard', 'There are unsaved changes.  Are you ' +
        		'sure you wish to discard them and close this file?', mtWarning,
                [mbYes, mbNo], 0, mbNo) = mrNo then
        	Exit;

//    ClearMainForm;
//	for i:= FTaskFrames.Count - 1 downto 0 do
//		if  FTaskFrames[i].D64File = FD64Files[FCurrD64File] then
//			begin
//            if  FTaskFrames[i].Prepared then
//				FTaskFrames[i].Unprepare;
//
//			//FTaskFrames[i].Free;
//
//			FTaskFrames[i].Visible:= False;
////          FTaskFrames[i].Parent:= nil;
////			FFreeFrames.Add(FTaskFrames[i]);
//            FTaskFrames.Delete(i);
////			Application.ProcessMessages;
//			end;

	ClearMainForm;

	FD64Files[FCurrD64File].D64Image.Free;

	FD64Files[FCurrD64File].FPanel.Controls[0].Free;
    FD64Files[FCurrD64File].FPanel.Free;
    FD64Files[FCurrD64File].Free;

	FD64Files.Delete(FCurrD64File);
	RezizeFilesBar;

	if  FD64Files.Count > 0 then
		FCurrD64File:= 0
	else
		FCurrD64File:= -1;

	ToggleBarDown;

	try
		if  FD64Files.Count > 0 then
			ActTaskFManager.Execute
		else
			ActTaskLibrary.Execute;

		except
		end;

    //if  Assigned(FD64Image) then
        //FreeAndNil(FD64Image);

    //FDirty:= False;
    end;

procedure TD64ExplorerMainDMod.ActFileCloseUpdate(Sender: TObject);
	begin
	ActFileClose.Enabled:= FCurrD64File > -1;//Assigned(FD64Image);
	end;

end.

