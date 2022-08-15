unit D64ExplorerConsts;

{$mode delphi}

interface

uses
    Classes, Graphics, SysUtils;

type
	TD64ClrIdx = (
    		dciHdrGrad0, dciHdrGrad1, dciHdrText0,
    		dciOptsGrad0, dciOptsGrad1,
    		dciListText0,
            dciItmGrad0, dciItmGrad1, dciItmHotGrad0, dciItmHotGrad1,
            dciItmActvGrad0, dciItmActvGrad1,
            dciItmText0, dciItmText1,
            dciBnrGrad0, dciBnrGrad1,
            dciBnrText0);

const
{$IFDEF MSWINDOWS}
	ARR_D64_CLR_IDX: array[TD64ClrIdx] of TColor = (
    		clActiveCaption, clBtnFace, clHighlightText,
            clBtnFace, clBtnShadow,
            clWindowText,
            clBtnShadow, clBtnFace, clBtnFace, clActiveCaption,
            clActiveCaption, clHighlight,
            clBtnFace, clHighlightText,
            clHighlight, clActiveCaption,
            clHighlightText);
{$ELSE}
	ARR_D64_CLR_IDX: array[TD64ClrIdx] of TColor = (
			clInactiveCaption, clBackground, clCaptionText,
            clMenuBar, clBackground,
            clWhite,
            clMenuBar, clBackground, clMenuHighlight, clMenuBar,
            clMenuHighlight, clActiveCaption,
            clInactiveCaptionText, clCaptionText,
            clBackground, clMenuHighlight,
            clCaptionText);
{$ENDIF}

implementation

end.

