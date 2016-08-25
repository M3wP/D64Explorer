program D64Explorer;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, lazcontrols, FormD64ExplorerMain, C64D64Image, DModD64ExplorerMain,
    FormD64SectorView, FormD64BAMView, FormD64DirectoryView, FrameD64ExplorerMain,
    FormD64ExplorerAbout;

{$R *.res}

begin
Application.Title:= 'D64 Explorer';
RequireDerivedFormResource:= True;
Application.Initialize;
Application.CreateForm(TD64ExplorerMainForm, D64ExplorerMainForm);
Application.CreateForm(TD64ExplorerMainDMod, D64ExplorerMainDMod);
Application.Run;
end.

