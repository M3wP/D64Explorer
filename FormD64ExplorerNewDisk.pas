unit FormD64ExplorerNewDisk;

{$mode Delphi}
{$H+}

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
		procedure Panel1Paint(Sender: TObject);
    private
    public
        procedure Initialise;
    end;

var
    D64ExplorerNewDiskForm: TD64ExplorerNewDiskForm;

implementation

{$R *.lfm}

uses
    D64ExplorerConsts;

{ TD64ExplorerNewDiskForm }

procedure TD64ExplorerNewDiskForm.Panel1Paint(Sender: TObject);
	begin
    Panel1.Canvas.GradientFill(Panel1.ClientRect,
    		ARR_D64_CLR_IDX[dciBnrGrad0], ARR_D64_CLR_IDX[dciBnrGrad1],
            gdVertical);

    Label1.Font.Color:= ARR_D64_CLR_IDX[dciBnrText0];
    Label1.Font.Style:= [fsBold];
	end;

procedure TD64ExplorerNewDiskForm.Initialise;
	begin
   	ComboBox1.ItemIndex:= 3;
    Edit1.Text:= '';
    Edit2.Text:= '';
    ActiveControl:= ComboBox1;
	end;

end.

