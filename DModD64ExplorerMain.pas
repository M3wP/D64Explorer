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
    IniFiles, C64D64Image, FrameD64ExplorerTask;

type
    TMenuItemArray = array of TMenuItem;

{ TD64ExplorerMainDMod }

	TD64ExplorerMainDMod = class(TDataModule)
        ActFileExit: TAction;
        ActFileOpen: TAction;
        ActHelpAbout: TAction;
        ActFileSave: TAction;
        ActFileSaveAs: TAction;
        ActFileClose: TAction;
        ActFileNew: TAction;
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
        procedure ActMngrToggleSetExecute(Sender: TObject);
        procedure ActTaskFInspectorExecute(Sender: TObject);
        procedure ActTaskFInspectorUpdate(Sender: TObject);
        procedure ActTaskFManagerExecute(Sender: TObject);
        procedure ActTaskFManagerUpdate(Sender: TObject);
        procedure ActTaskLibraryExecute(Sender: TObject);
        procedure ActViewBAMViewExecute(Sender: TObject);
        procedure ActViewDirViewExecute(Sender: TObject);
        procedure ActViewSecViewExecute(Sender: TObject);
        procedure DataModuleCreate(Sender: TObject);
        procedure DataModuleDestroy(Sender: TObject);
  	private
        FIniFile: TIniFile;

    	FD64Image: TD64Image;
        FD64FileName: string;
        FDirty: Boolean;

        FInjectOpen: string;
        FDropFiles: TStringList;
        FUpdThread: TThread;

		FTaskFrames: TTaskFramesList;
        FActiveTask: TD64ExplorerTaskFrame;

    	procedure ClearMainForm;
        procedure InitialiseMainForm;
        procedure SetApplicationTitle(const ATitle: string);

        procedure SetDirty(const AValue: Boolean);

        procedure SetSaveDialogTypes;

    protected
        procedure ProcessOpenFiles;
       	procedure ProcessDropFiles;

    public
        procedure ApplicationClose;
        procedure CloseQuery(var ACanClose: Boolean);
        procedure AddDropFiles(const AFiles: array of string); overload;
        procedure AddDropFiles(const AFiles: TStrings); overload;

        procedure BindToCoolbar(const AControl: TControl;
        		const ABreak: Boolean = True);
        procedure UnbindToCoolbar(const AControl: TControl);
        procedure UpdateMenuVisibility;
        procedure MoveMenuItems(const AItems: array of TMenuItem;
                var AMenus: TMenuItemArray);
        procedure RetrieveMenuItems(const AItems: array of TMenuItem;
                const AMenus: TMenuItemArray; const ATarget: TPopupMenu);

        procedure EnableFileDrop(const AEnable: Boolean);

        procedure CreateActivateTask(const ATaskClass: TD64ExplorerTaskFrameClass);
        procedure AddRecentUsed(const AFileName: string);
        procedure OpenDiskImageFile(const AFileName: string);

        procedure UpdateTasks;
        procedure RequestSaveDataCallback(const ATask: TD64ExplorerTaskFrame);

        property  D64Image: TD64Image read FD64Image;
        property  Dirty: Boolean read FDirty write SetDirty;
	end;

var
    D64ExplorerMainDMod: TD64ExplorerMainDMod;

implementation

{$R *.lfm}

uses
	D64ExplorerUtils, D64ExplorerStrs,
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

{ TD64ExplorerMainDMod }

procedure TD64ExplorerMainDMod.ActFileOpenExecute(Sender: TObject);
	begin
	if  FDirty then
    	if  MessageDlg('Confirm discard', 'There are unsaved changes.  Are you ' +
        		'sure you wish to discard them and open another file?', mtWarning,
                [mbYes, mbNo], 0, mbNo) = mrNo then
        	Exit;

    if  (Length(FInjectOpen) > 0)
    or  OpenDialog1.Execute then
      	begin
        FDirty:= False;
		ActFileClose.Execute;

        FD64Image:= TD64Image.Create;
        try
//dengland  If I use the file open version of the constructor and the file is
//          share locked, then the runtime fails and the program aborts.  This
//          is despite 3 layers of trying to protect the application from errors.

            if  Length(FInjectOpen) > 0 then
                FD64FileName:= FInjectOpen
            else
            	FD64FileName:= OpenDialog1.FileName;

            FInjectOpen:= EmptyStr;

            FD64Image.LoadFromFile(FD64FileName);

//          InitialiseMainForm;
			ClearMainForm;
            ActTaskFManager.Execute;

            SetApplicationTitle(QuotedStr(FD64FileName));

            AddRecentUsed(FD64FileName);

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
        FD64FileName:= SaveDialog1.FileName;
        ActFileSave.Execute;
        end;
    end;

procedure TD64ExplorerMainDMod.ActFileSaveAsUpdate(Sender: TObject);
	begin
    ActFileSaveAs.Enabled:= Assigned(FD64Image);
	end;

procedure TD64ExplorerMainDMod.ActFileSaveExecute(Sender: TObject);
	begin
    if  FD64FileName = EmptyStr then
        begin
        SetSaveDialogTypes;

        if  SaveDialog1.Execute then
	        FD64FileName:= SaveDialog1.FileName
        else
            Exit;
        end;

    FD64Image.SaveToFile(FD64FileName);
    FDirty:= False;

    SetApplicationTitle(QuotedStr(FD64FileName));

    AddRecentUsed(FD64FileName);
	end;

procedure TD64ExplorerMainDMod.ActFileSaveUpdate(Sender: TObject);
	begin
    ActFileSave.Enabled:= Assigned(FD64Image) and FDirty;
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

procedure TD64ExplorerMainDMod.ActMngrToggleSetExecute(Sender: TObject);
	begin
//
	end;

procedure TD64ExplorerMainDMod.ActTaskFInspectorExecute(Sender: TObject);
	begin
    CreateActivateTask(TD64ExplorerMainFrame);
    D64ExplorerMainForm.SpeedButton7.Down:= True;
	end;

procedure TD64ExplorerMainDMod.ActTaskFInspectorUpdate(Sender: TObject);
	begin
    ActTaskFInspector.Enabled:= Assigned(FD64Image);
	end;

procedure TD64ExplorerMainDMod.ActTaskFManagerExecute(Sender: TObject);
	begin
    CreateActivateTask(TD64ExplorerManageFrame);
    D64ExplorerMainForm.SpeedButton2.Down:= True;
	end;

procedure TD64ExplorerMainDMod.ActTaskFManagerUpdate(Sender: TObject);
	begin
    ActTaskFManager.Enabled:= Assigned(FD64Image);
	end;

procedure TD64ExplorerMainDMod.ActTaskLibraryExecute(Sender: TObject);
	begin
    CreateActivateTask(TD64ExplorerLibraryFrame);
    D64ExplorerMainForm.SpeedButton1.Down:= True;
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
    FTaskFrames:= TTaskFramesList.Create;

    FDropFiles:= TStringList.Create;

    FIniFile:= TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
    FIniFile.CacheUpdates:= False;

//  FOldIdle:= Application.OnIdle;
//  Application.OnIdle:= DoOnIdle;

    ClearMainForm;

    FUpdThread:= TUpdateThread.Create(True);
    FUpdThread.FreeOnTerminate:= True;
    FUpdThread.Suspended:= False;
    end;

procedure TD64ExplorerMainDMod.DataModuleDestroy(Sender: TObject);
    var
    i: Integer;

    begin
    FUpdThread.Terminate;

    for i:= FTaskFrames.Count - 1 downto 0 do
        begin
        FTaskFrames[i].SaveData(FIniFile);

        FTaskFrames[i].Visible:= False;
        FTaskFrames[i].Parent:= nil;
        FTaskFrames[i].Free;
        FTaskFrames[i]:= nil;

        FTaskFrames.Delete(i);
        end;

    if  Assigned(FD64Image) then
    	FD64Image.Free;

   	FTaskFrames.Free;

    FIniFile.Free;
    end;

procedure TD64ExplorerMainDMod.ClearMainForm;
    var
    i: Integer;

    begin
    if  Assigned(FActiveTask) then
        begin
        FActiveTask.Unprepare;
        FActiveTask:= nil;
        end;

    for i:= FTaskFrames.Count - 1 downto 0 do
        if  not (FTaskFrames[i] is TD64ExplorerLibraryFrame) then
            begin
            FTaskFrames[i].SaveData(FIniFile);

            FTaskFrames[i].Visible:= False;
            FTaskFrames[i].Parent:= nil;
            FTaskFrames[i].Free;
            FTaskFrames[i]:= nil;

            FTaskFrames.Delete(i);
            end;

    if  Assigned(D64SectorViewForm) then
        D64SectorViewForm.Close;

    if  Assigned(D64BAMViewForm) then
        D64BAMViewForm.Close;

    if  Assigned(D64DirectoryViewForm) then
        D64DirectoryViewForm.Close;

    ActTaskLibrary.Execute;

    SetApplicationTitle('');
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

procedure TD64ExplorerMainDMod.SetApplicationTitle(const ATitle: string);
    var
    d: string;

    begin
    if  FDirty then
        d:= '* '
    else
        d:= '';

    Application.Title:= ATitle + d + STR_CAP_D64EXPLORER;
    D64ExplorerMainForm.Caption:= ATitle + d + STR_CAP_D64EXPLORER;
    end;

procedure TD64ExplorerMainDMod.SetDirty(const AValue: Boolean);
	begin
//  if  (not FDirty)
//  and AValue then
//      begin
        FDirty:= AValue;
        SetApplicationTitle(QuotedStr(FD64FileName));
//      end;

//  FDirty:= AValue;
	end;

procedure TD64ExplorerMainDMod.SetSaveDialogTypes;
	begin
    if  FD64Image.DiskType = ddt1541 then
        begin
        SaveDialog1.Filter:= 'D64 Image Files (*.d64)';
        SaveDialog1.DefaultExt:= '.d64';
        end
    else if FD64Image.DiskType = ddt1571 then
        begin
        SaveDialog1.Filter:= 'D71 Image Files (*.d71)';
        SaveDialog1.DefaultExt:= '.d71';
        end
    else
	    begin
    	SaveDialog1.Filter:= 'D81 Image Files (*.d81)';
	    SaveDialog1.DefaultExt:= '.d81';
    	end
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
    and (Assigned(FActiveTask)) then
        begin
        i:= 0;
        f:= True;
        while (i < FDropFiles.Count) and FActiveTask.AcceptFile(FDropFiles[i]) do
			Inc(i);
        end;

    FDropFiles.Clear;

    if  f then
        UpdateTasks;
	end;

procedure TD64ExplorerMainDMod.ApplicationClose;
    begin

    end;

procedure TD64ExplorerMainDMod.CloseQuery(var ACanClose: Boolean);
	begin
    ACanClose:= False;

	if  FDirty then
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
    var
    i: Integer;

    begin
    AControl.Parent:= D64ExplorerMainForm.CoolBar1;

    if  not ABreak then
        for i:= 0 to D64ExplorerMainForm.CoolBar1.Bands.Count - 1 do
            if  D64ExplorerMainForm.CoolBar1.Bands[i].Control = AControl then
        		begin
                D64ExplorerMainForm.CoolBar1.Bands[i].Break:= False;
        		end;

    AControl.Visible:= True;
	end;

procedure TD64ExplorerMainDMod.UnbindToCoolbar(const AControl: TControl);
	begin
    AControl.Parent:= nil;
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
    s1,
    s2: string;

    begin
    SetLength(AMenus, Length(AItems));

    for i:= 0 to Length(AItems) - 1 do
        begin
        m:= nil;
        for j:= 0 to MainMenu1.Items.Count - 1 do
            begin
            s1:= MainMenu1.Items[j].Caption;
            s2:= AItems[i].Parent.Caption;

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
    	const ATaskClass: TD64ExplorerTaskFrameClass);
    var
    i: Integer;
    f: TD64ExplorerTaskFrame;
    c: Boolean;

    begin
    f:= nil;
    c:= False;

    for i:= 0 to FTaskFrames.Count - 1 do
        if  FTaskFrames[i] is ATaskClass then
            begin
            f:= FTaskFrames[i];
            Break;
            end;

    if  not Assigned(f) then
        begin
    	f:= ATaskClass.Create(Self);
        f.ParentDoubleBuffered:= False;
        f.DoubleBuffered:= True;
        f.ParentColor:= False;

        f.LoadData(FIniFile);

        c:= True;
        end;

    if  Assigned(FActiveTask) then
        begin
        FActiveTask.Unprepare;
        FActiveTask.Visible:= False;
        end;

    D64ExplorerMainForm.Update;

    D64ExplorerMainForm.CoolBar1.BeginUpdate;
    try
	    f.Prepare;

    	finally
        D64ExplorerMainForm.CoolBar1.EndUpdate;
        end;

    f.Parent:= D64ExplorerMainForm.Panel3;
    f.Align:= alClient;
    f.Visible:= True;
    f.BringToFront;
    f.Invalidate;

    if  c then
        begin
    	f.Initialise;
        FTaskFrames.Add(f);
        end;

    D64ExplorerMainForm.Label1.Caption:= f.GetDescription;

    FActiveTask:= f;

    D64ExplorerMainForm.Repaint;
	end;

procedure TD64ExplorerMainDMod.AddRecentUsed(const AFileName: string);
	begin
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

procedure TD64ExplorerMainDMod.ActFileExitExecute(Sender: TObject);
    var
    cc: Boolean;

    begin
    CloseQuery(cc);
    if  not cc then
        Exit;

    FDirty:= False;
    Application.Terminate;
	end;

procedure TD64ExplorerMainDMod.ActFileNewExecute(Sender: TObject);
    var
    ss: Boolean;
    ty: TD64DiskType;
    nm,
    id: AnsiString;

    begin
	if  FDirty then
    	if  MessageDlg('Confirm discard', 'There are unsaved changes.  Are you ' +
        		'sure you wish to discard them and create a new file?', mtWarning,
                [mbYes, mbNo], 0, mbNo) = mrNo then
        	Exit;

	if  not Assigned(D64ExplorerNewDiskForm) then
        Application.CreateForm(TD64ExplorerNewDiskForm, D64ExplorerNewDiskForm);

    D64ExplorerNewDiskForm.Initialise;

    if  D64ExplorerNewDiskForm.ShowModal = mrOk then
        begin
        FD64FileName:= '';
        FDirty:= False;

        if  Assigned(FD64Image) then
            FreeAndNil(FD64Image);

        FD64Image:= TD64Image.Create;

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

        FD64Image.FormatImage(nm, id, ty, ss,
        		D64ExplorerNewDiskForm.CheckBox1.Checked);
        FDirty:= True;

//      InitialiseMainForm;
		ClearMainForm;
		ActTaskFManager.Execute;

        SetApplicationTitle('Untitled');
		end;
	end;

procedure TD64ExplorerMainDMod.ActFileCloseExecute(Sender: TObject);
	begin
	if  FDirty then
    	if  MessageDlg('Confirm discard', 'There are unsaved changes.  Are you ' +
        		'sure you wish to discard them and close this file?', mtWarning,
                [mbYes, mbNo], 0, mbNo) = mrNo then
        	Exit;

    ClearMainForm;

    if  Assigned(FD64Image) then
        FreeAndNil(FD64Image);

    FDirty:= False;
    end;

procedure TD64ExplorerMainDMod.ActFileCloseUpdate(Sender: TObject);
	begin
	ActFileClose.Enabled:= Assigned(FD64Image);
	end;

end.

