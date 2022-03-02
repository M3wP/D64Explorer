unit D64ExplorerUtils;

{$mode Delphi}
{$H+}

interface

uses
    Classes, SysUtils, C64D64Image, StdCtrls, Graphics;


procedure HexDumpStreamToLstBx(const AStream: TStream; const ALstBx: TListBox);

procedure DirectoryDrawComboItem(ACombo: TComboBox; AIndex: Integer;
        ARect: TRect; AState: TOwnerDrawState; ADirs: TD64DirPartitions);

function ConvertPetsciiToScreen(const AChar: AnsiChar): Byte;
function ConvertAsciiToScreen(const AChar: AnsiChar): Byte;
function ConvertAsciiToPetscii(const AChar: AnsiChar;
		const AAltSet: Boolean = True): Char;
function ConvertPetsciiToAscii(const AChar: AnsiChar): Char;

function AsciiToPetsciiString(const AString: AnsiString;
		const AAltSet: Boolean = True): AnsiString;
function PetsciiToAsciiString(const AString: AnsiString): AnsiString;

procedure DrawC64Text(const ACanvas: TCanvas; const AX, AY: Integer;
		const AText: AnsiString; const AColour: TColor;
       	const ADoubleSize: Boolean = False;
       	const APetscii: Boolean = False;
        const ACharsetB: Boolean = False);

implementation

uses
    LCLType;


type
    TPetsciiConvert = record
     	range: set of AnsiChar;
        offset: Integer;
    end;

const
    RT_RCDATA       = PAnsiChar(10);

  	ARR_XLT_PETSCIITOSCRN: array[0..7] of TPetsciiConvert = (
    	(range: [#$00..#$1F]; offset: 128),
        (range: [#$20..#$3F]; offset: 0),
        (range: [#$40..#$5F]; offset: -64),
        (range: [#$60..#$7F]; offset: -32),
        (range: [#$80..#$9F]; offset: 64),
        (range: [#$A0..#$BF]; offset: -64),
        (range: [#$C0..#$DF]; offset: -128),
        (range: [#$E0..#$Fe]; offset: -128));

    ARR_TOK_ASCIITOSCRN: array[0..8] of AnsiChar = (
		'\', '^', '_', '`', '{', '|', '}', '~', ' ');
	ARR_DAT_ASCIITOSCRN: array[0..8] of Byte = (
		$4D, $71, $64, $4A, $73, $42, $6B, $45, $20);
	ARR_DAT_ASCIITOPETSCII0: array[0..8] of Byte = (
		$6D, $B1, $A4, $6A, $B3, $62, $AB, $65, $A0);
	ARR_DAT_ASCIITOPETSCII1: array[0..8] of Byte = (
		$BA, $B1, $A4, $AD, $B3, $AA, $AB, $B7, $A0);

    ARR_XLT_ASCIITOPETSCII: array[0..1] of TPetsciiConvert = (
		(range: [#$41..#$5A]; offset: 32),
        (range: [#$61..#$7A]; offset: -32));

var
	FC64Chars: TMemoryStream;

function AsciiToPetsciiString(const AString: AnsiString;
        const AAltSet: Boolean): AnsiString;
    var
    i: Integer;

	begin
    Result:= '';
    for i:= Low(AString) to High(AString) do
        Result:= Result + ConvertAsciiToPetscii(AString[i], AAltSet);
    end;

function PetsciiToAsciiString(const AString: AnsiString): AnsiString;
    var
    i: Integer;

    begin
    Result:= '';
    for i:= Low(AString) to High(AString) do
        Result:= Result + ConvertPetsciiToAscii(AString[i]);
    end;

function ConvertPetsciiToScreen(const AChar: AnsiChar): Byte;
    var
    i: Integer;

	begin
    Result:= $20;

    if  AChar = #$FF then
        Result:= $5E
    else
    	for i:= Low(ARR_XLT_PETSCIITOSCRN) to High(ARR_XLT_PETSCIITOSCRN) do
            if  AChar in ARR_XLT_PETSCIITOSCRN[i].range then
                begin
                Result:= Ord(AChar) + ARR_XLT_PETSCIITOSCRN[i].offset;
                Break;
                end;
    end;

function ConvertAsciiToScreen(const AChar: AnsiChar): Byte;
    var
    i: Integer;
    c: AnsiChar;

    begin
    for i:= 0 to High(ARR_TOK_ASCIITOSCRN) do
        if  AChar = ARR_TOK_ASCIITOSCRN[i] then
            begin
            Result:= ARR_DAT_ASCIITOSCRN[i];
            Exit;
            end;

    c:= AChar;
    for i:= Low(ARR_XLT_ASCIITOPETSCII) to High(ARR_XLT_ASCIITOPETSCII) do
    	if  c in ARR_XLT_ASCIITOPETSCII[i].range then
            begin
            c:= AnsiChar(Ord(c) + ARR_XLT_ASCIITOPETSCII[i].offset);
            Break;
            end;

   	Result:= ConvertPetsciiToScreen(c);
    end;


function ConvertAsciiToPetscii(const AChar: AnsiChar; const AAltSet: Boolean): Char;
	var
	i: Integer;

	begin
    Result:= AChar;

	for i:= 0 to High(ARR_TOK_ASCIITOSCRN) do
    	if  AChar = ARR_TOK_ASCIITOSCRN[i] then
	        begin
            if  not AAltSet then
        		Result:= AnsiChar(ARR_DAT_ASCIITOPETSCII0[i])
            else
                Result:= AnsiChar(ARR_DAT_ASCIITOPETSCII1[i]);
    	    Exit;
	        end;

    if  not AAltSet then
        Result:= AChar
    else
	    for i:= Low(ARR_XLT_ASCIITOPETSCII) to High(ARR_XLT_ASCIITOPETSCII) do
		    if  AChar in ARR_XLT_ASCIITOPETSCII[i].range then
        	    begin
	            Result:= AnsiChar(Ord(AChar) + ARR_XLT_ASCIITOPETSCII[i].offset);
    	        Break;
        	    end;
    end;


function ConvertPetsciiToAscii(const AChar: AnsiChar): Char;
    var
    i: Integer;

	begin
    Result:= AChar;

	for i:= 0 to High(ARR_TOK_ASCIITOSCRN) do
    	if  Ord(AChar) = ARR_DAT_ASCIITOPETSCII0[i] then
	        begin
            Result:= ARR_TOK_ASCIITOSCRN[i];
    	    Exit;
	        end;

	for i:= 0 to High(ARR_TOK_ASCIITOSCRN) do
    	if  Ord(AChar) = ARR_DAT_ASCIITOPETSCII1[i] then
	        begin
            Result:= ARR_TOK_ASCIITOSCRN[i];
    	    Exit;
	        end;

	for i:= Low(ARR_XLT_ASCIITOPETSCII) to High(ARR_XLT_ASCIITOPETSCII) do
		if  AChar in ARR_XLT_ASCIITOPETSCII[i].range then
        	begin
	        Result:= AnsiChar(Ord(AChar) + ARR_XLT_ASCIITOPETSCII[i].offset);
    	    Break;
        	end;
    end;

procedure DrawC64Text(const ACanvas: TCanvas; const AX, AY: Integer;
    	const AText: AnsiString; const AColour: TColor;
        const ADoubleSize, APetscii, ACharsetB: Boolean);
    var
    c: AnsiChar;
    b: Byte;
    o: Integer;
    i,
  	j,
    k: Integer;
    m: Byte;
    x,
    y: Integer;
    f: Integer;

    begin
    if  ADoubleSize then
        f:= 2
    else
        f:= 1;

    x:= AX;
    for k:= Low(AText) to High(AText) do
        begin
        c:= AText[k];
        if  APetscii then
        	b:= ConvertPetsciiToScreen(c)
        else
            b:= ConvertAsciiToScreen(c);
        if  ACharsetB then
        	o:= 2048 + b * 8
        else
        	o:= b * 8;

        for i:= 0 to 7 do
            begin
            m:= $80;
            b:= PByte(FC64Chars.Memory)[o];

            for j:= 0 to 7 do
                begin
                if  (b and m) <> 0 then
                    begin
                    x:= AX + (k - Low(AText)) * 8 * f + j * f;
                    y:= AY + i * f;

                    ACanvas.Pixels[x, y]:= AColour;

                    if  ADoubleSize then
                        begin
	                    ACanvas.Pixels[x + 1, y]:= AColour;
    	                ACanvas.Pixels[x, y + 1]:= AColour;
        	            ACanvas.Pixels[x + 1, y + 1]:= AColour;
                        end;
                    end;

                m:= m shr 1;
                end;

            Inc(o);
            end;
        end;
    end;

procedure InitC64Chargen;
    var
    r: TResourceStream;

    begin
    r:= TResourceStream.Create(HInstance, 'chargen', RT_RCDATA);
    try
        FC64Chars:= TMemoryStream.Create;
        FC64Chars.CopyFrom(r, r.Size);

    	finally
        r.Free;
        end;
	end;


procedure HexDumpStreamToLstBx(const AStream: TStream; const ALstBx: TListBox);
    var
    i,
    j,
    l: Integer;
    s: string;
    b: Byte;
    d: array[0..$0F] of Byte;

    begin
    i:= 0;
    l:= 0;
//dengland      Stop the compiler complaining
    d[0]:= $00;
    s:= Format('%6.6x   ', [l]);
    while AStream.Position < AStream.Size do
        begin
        if  (i > 0)
        and (i mod 16 = 0) then
            begin
            s:= s + '  ';
            for j:= 0 to $0F do
                if  d[j] in [$20..$7E] then
                    s:= s + string(AnsiChar(d[j]))
                else
                    s:= s + ' ';
            ALstBx.Items.Add(s);

            Inc(l, i);
            i:= 0;
            s:= Format('%6.6x   ', [l]);
            end
        else if (i > 0)
        and (i mod 4 = 0) then
            s:= s + ' ';

        b:= AStream.ReadByte;
        s:= s + Format('%2.2x ', [b]);

        d[i]:= b;
        Inc(i);
        end;

    while i < 16 do
        begin
        if  (i > 0)
        and (i mod 4 = 0) then
            s:= s + ' ';

        s:= s + '   ';
        d[i]:= $00;

        Inc(i);
        end;

    s:= s + '  ';
    for j:= 0 to $0F do
        if  d[j] in [$20..$7E] then
            s:= s + string(AnsiChar(d[j]))
        else
            s:= s + ' ';

    ALstBx.Items.Add(s);
    end;


procedure DirectoryDrawComboItem(ACombo: TComboBox; AIndex: Integer;
        ARect: TRect; AState: TOwnerDrawState; ADirs: TD64DirPartitions);
    var
    s: string;
    i: Integer;
    x: Integer;
    c: TCanvas;

    begin
    //odSelected, odGrayed, odDisabled, odChecked,
    //odFocused, odDefault, odHotLight, odInactive, odNoAccel,
    //odNoFocusRect, odReserved1, odReserved2, odComboBoxEdit,
    //odPainted  // item already painted

    c:= ACombo.Canvas;

    if  not (odBackgroundPainted in AState) then
        begin
        if  odSelected in AState then
            begin
            c.Brush.Color:= clHighlight;
            c.Pen.Color:= clWindow;
            end
        else
            begin
            c.Brush.Color:= clWindow;
            c.Pen.Color:= clWindowText;
            end;

        c.Brush.Style:= bsSolid;
        c.Pen.Style:= psSolid;

        c.FillRect(ARect);

        s:= '/' + TrimRight(ADirs[AIndex].PartFileName);
        if  odComboBoxEdit in AState then
            begin
            i:= ADirs[AIndex].Parent;
            while i > 0 do
                begin
                s:= '/' + TrimRight(ADirs[i].PartFileName) + s;
                i:= ADirs[i].Parent;
                end;
            end
        else
            begin
            if  ADirs[AIndex].HasChildren then
                s:= '+' + s
            else
                s:= '=' + s;

            if  AIndex > 0 then
                s:= '-' + s;

            for i:= ADirs[AIndex].Depth downto 0 do
                if  i = 1 then
                    s:= '|' + s
                else if i > 0 then
                    s:= ' |' + s;
            end;

        x:= ARect.Left + 2;
        i:= 1;
        while i < Length(s) do
            begin
            if  s[i] = '/' then
                Break
            else if s[i] = '|' then
                begin
                c.Pen.Style:= psDot;
                c.Line(x + 2, ARect.Top + 1, x + 2, ARect.Bottom - 1);
                end
            else if s[i] = '-' then
                begin
                c.Pen.Style:= psDot;
                c.Line(x, ARect.Top + 7, x + 8, ARect.Top + 7);
                end
            else if s[i] = '=' then
                begin
                c.Pen.Style:= psSolid;
                c.Line(x, ARect.Top + 7, x + 8, ARect.Top + 7);
                end
            else if s[i] = '+' then
                begin
                c.Pen.Style:= psSolid;
                c.Line(x + 2, ARect.Top + 3, x + 2, ARect.Bottom - 3);
                c.Line(x, ARect.Top + 7, x + 9, ARect.Top + 7);
                end;

            Inc(x, 7);
            Inc(i);
            end;

        c.TextOut(x, ARect.Top, Copy(s, i, MaxInt));
        end;
    end;

initialization
	InitC64Chargen;

finalization
	FC64Chars.Free;

end.

