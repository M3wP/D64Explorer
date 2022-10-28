unit FormD64ExplorerNewDisk;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
    Buttons;

type

    { TD64ExplorerNewDiskForm }

    TD64ExplorerNewDiskForm = class(TForm)
        Bevel1: TBevel;
        Button1: TButton;
        Button2: TButton;
        CheckBox1: TCheckBox;
        ComboBox1: TComboBox;
        Edit1: TEdit;
        Edit2: TEdit;
        Image1: TImage;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Panel1: TPanel;
    private
    public
        procedure Initialise;
    end;

var
    D64ExplorerNewDiskForm: TD64ExplorerNewDiskForm;

implementation

{$R *.lfm}

{ TD64ExplorerNewDiskForm }


procedure TD64ExplorerNewDiskForm.Initialise;
	begin
   	ComboBox1.ItemIndex:= 3;
    Edit1.Text:= '';
    Edit2.Text:= '';
    ActiveControl:= ComboBox1;
	end;

end.

