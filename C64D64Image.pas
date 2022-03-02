//------------------------------------------------------------------------------
//C64D64Image
//===========
//Load, creates, manipulates and saves Commodore 1541, 1571 and 1581 (.D64,
//.D71, and .D81) disk image files.
//
//Please note:
//------------
//Presently, only FPC/Lazarus is supported.  Delphi support is incomplete.
//Needs a class helper for TMemoryStream to include ReadByte (which I'm a fan
//of) or ReadByte changed to ReadBuffer.
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
unit C64D64Image;

{$IFDEF FPC}
    {$MODE DELPHI}
{$ENDIF}

{$H+}

interface

uses
	Classes, SysUtils;

const
//  Size of a sector on a disk
    VAL_SIZ_D64SECTRSIZE = 256;
    VAL_SIZ_D64BLOCKSIZE = 254;

    LIT_LBL_D64BLNKFLN = #$A0#$A0#$A0#$A0#$A0#$A0#$A0#$A0 +
                         #$A0#$A0#$A0#$A0#$A0#$A0#$A0#$A0;

    VAL_TYP_D64FTYPE_DEL = 0;
    VAL_TYP_D64FTYPE_SEQ = 1;
    VAL_TYP_D64FTYPE_PRG = 2;
    VAL_TYP_D64FTYPE_USR = 3;
    VAL_TYP_D64FTYPE_REL = 4;
    VAL_TYP_D64FTYPE_CBM = 5;


type
//  Supported disk types (d71 images may be single or double sided.  d81 images
//      currently do not support directory partitions).
    TD64DiskType = (ddt1541, ddt1571, ddt1581);
//  Range of track numbers on a disk
    TD64TrackNum = 1..80;
//  Range of sector numbers on a track
    TD64SectorNum = 0..39;
//  Range of entry numbers in a sector
    TD64EntryNum = 0..7;
//  Data for a file entry on the disk
    TD64EntryData = array[0..31] of Byte;

    TD64FileType = 0..7;
    TD64FileState = (dfsReadOnly, dfsReplacing, dfsClosed);
    TD64FileStates = set of TD64FileState;

{ TD64DirEntry }
//  Helper record for a directory file entry
    TD64DirEntry = record
        Track: TD64TrackNum;
        Sector: TD64SectorNum;
        EntryNum: TD64EntryNum;
        FileType: Byte;
        DataTrack: TD64TrackNum;
        DataSector: TD64SectorNum;
        FileName: AnsiString;
        FileSize: Word;
        EntryData: TD64EntryData;

//dengland These routines are primarily for manipulating the EntryData.
//      EntryNum must still be logicially handled (etc).
//      procedure SetTrack(const AValue: Byte);
//      procedure SetSector(const AValue: Byte);
        procedure SetFileType(const AValue: Byte);
        procedure SetDataTS(const ATrack, ASector: Byte);
        procedure SetFileName(const AName: string);
        procedure SetRelTS(const ATrack, ASector: Byte);
        procedure SetRelRecSize(const AValue: Byte);
        procedure SetFileSize(const AValue: Word);

        procedure SetGEOSInfoTS(const ATrack, ASector: Byte);   //SetRelTS
        procedure SetGEOSStructure(const AValue: Byte);         //SetRelRecSize
        procedure SetGEOSFileType(const AValue: Byte);
        procedure SetGEOSYear(const AYear: Word);               //The actual year
        procedure SetGEOSMonth(const AValue: Byte);
        procedure SetGEOSDay(const AValue: Byte);
        procedure SetGEOSHour(const AValue: Byte);
        procedure SetGEOSMinute(const AValue: Byte);
        procedure SetGEOSDateTime(const ADateTime: TDateTime);
    end;
    TD64DirEntries = array of TD64DirEntry;

//  Important directory partition info
    TD64DirPartitionInfo = record
        Track: TD64TrackNum;
        PartSize: Word;
    end;

//  Directory partition information helper record
    TD64DirPartition = record
        Info: TD64DirPartitionInfo;
        PartFileName,
        PartDiskName,
        PartDiskID,
        PartDOSType: AnsiString;
        HasChildren: Boolean;
        Depth: Integer;
        Parent: Integer;
    end;
    TD64DirPartitions = array of TD64DirPartition;

//  BAM info for a disk track
    TD64TrackBAM = record
        FreeSectors: Byte;
        Bitmap: array of Byte;
    end;
//  BAM info for the disk
    TD64DiskBAM = array of TD64TrackBAM;


type
{ TD64Image }
//  Class for loading, creating and manipulating a .d64, .d71 or .d81 disk image
    TD64Image = class(TObject)
    private
    	FData: TMemoryStream;
        FDiskType: TD64DiskType;
        FSingleSide: Boolean;
        FTrkCount: TD64TrackNum;
        FMaxSectors: Byte;
        FValidVersion: Boolean;
        FDOSVersion: Byte;
        FDiskName: AnsiString;
        FDiskID: AnsiString;
        FDOSType: AnsiString;
        FGEOSDisk: Boolean;
        FGEOSVerMajor: Byte;
        FGEOSVerMinor: Byte;
        FCurrPartTrk: Byte;
        FCurrPartSiz: Word;

        procedure DoLoadFromFile(const AFile: string);
        procedure DoSaveToFile(const AFile: string);

        function  DoValidateTrackSector(const ATrack: TD64TrackNum;
        		const ASector: TD64SectorNum): Word;
        procedure DoGetSector(const ANum: Word; ADest: TStream);
        procedure DoGetFileEntries(const AStream: TMemoryStream;
                const ATrack: TD64TrackNum; const ASector: TD64SectorNum;
                var AEntries: TD64DirEntries; const AFullList: Boolean = False);
        function  DoGet1581BAM(const AStream: TMemoryStream;
                const ATrack: TD64TrackNum; const ATrkCnt: Byte;
                var ABAM: TD64DiskBAM): Integer;
        procedure DoWriteTrackSectorNextPointer(const ATrack: TD64TrackNum;
                const ASector: TD64SectorNum; const ANxtTrack: Byte;
                const ANxtSector: Byte; out AData: PByte);

        procedure DoAnalyseDOSDetails;
        procedure DoAllocateNewSector(var ABAM: TD64DiskBAM;
                out ATrack: TD64TrackNum; out ASector: TD64SectorNum;
                const AAllocate: Boolean = True);
        procedure DoDeallocateSectorChain(const ATrack: TD64TrackNum;
                const ASector: TD64SectorNum; var ABAM: TD64DiskBAM);
        procedure DoWriteSector(const ATrack: TD64TrackNum;
                const ASector: TD64SectorNum; const ANxtTrack: Byte;
                const ANxtSector: Byte; const AStream: TStream);
        procedure DoAllocateDirEntry(const AEntryData: TD64EntryData);
        procedure DoWriteDiskBAM(const ABAM: TD64DiskBAM);

   	public
        constructor Create; overload;
//dengland Using this constructor when the file is share locked causes the
//      LCL runtime to abort and the application to abnormally terminate. :(
        constructor Create(const AFile: string); overload;
        destructor  Destroy; override;

//      Load an image from a file.  The DiskType will be detected by the filename.
        procedure LoadFromFile(const AFile: string);
//      Save the current image to a file.
        procedure SaveToFile(const AFile: string);

//      Format a new disk image.  The disk name, id and type must be specified.
//          Optionally, you can create a GEOS compatible disk image.
        procedure FormatImage(const ADiskName: AnsiString; const ADiskID: AnsiString;
                const ADiskType: TD64DiskType; const ASingleSide: Boolean = False;
                const AGEOSDisk: Boolean = False);

//		Return whether or not an entry refers to a partition and its data position
        function  IsDirectoryPartition(const AEntry: TD64DirEntry;
                out ADataPos: Cardinal): Boolean; inline;

//      Return the number of sectors on the given track.
        function  GetSectorsForTrack(const ATrack: TD64TrackNum): Byte; inline;
//      Get the whole of the sector data for a given track and sector.
        procedure GetRawSector(const ATrack: TD64TrackNum;
        		const ASector: TD64SectorNum; ADest: TStream);
//      Read the data for a chain of tracks and sectors, starting at the given
//          track and sector.  The track and sector link data is not returned
//          but the whole data of the last sector is.  The returned actual size
//          should be checked for manipulating the returned file data.
        procedure GetDataChain(const ATrack: TD64TrackNum;
        		const ASector: TD64SectorNum; ADest: TStream;
                out ActualSize: Cardinal);
//      Get the file entries on the disk directory.
        procedure GetFileEntries(var AEntries: TD64DirEntries;
                const AFullList: Boolean = False);
//      Get the BAM information for the disk.
        function  GetDiskBAM(var ADiskBAM: TD64DiskBAM): Integer;
//		Get the total number of blocks on the disk
        function  GetDiskBlockCount: Integer;

//      Allocate disk sectors for the data in the given stream.  Allocates as
//          many sectors as required to store the whole of the stream from its
//          current position.  The start track and sector is returned as well
//          as the total number of sectors used (blocks).
        procedure AllocateDiskSectors(const AStream: TStream;
                out AStartTrk: TD64TrackNum;
                out AStartSec: TD64SectorNum;  out ABlocksUsed: Word);
//      Allocates a new file entry for the given file entry data.  Does not
//          attempt to replace existing files with the same name.  Will allocate
//          new director sectors as required.
        procedure AllocateFileEntry(const AEntryData: TD64EntryData);
//      Deletes the given file from the disk, including freeing all of the
//          sectors used by the file in the BAM.  Automatically detects GEOS
//          files and handles them appropriately.
        procedure ScratchFileEntry(const ATrack: TD64TrackNum;
                const ASector: TD64SectorNum; const AEntry: TD64EntryNum);

//      Get a array of directory compatible partitions.
        procedure GetDirPartitions(var AParts: TD64DirPartitions);
//      Set the current directory partition.  Use GetDirPartitions to get the
//          required information.  Returns the previous partition info.
        procedure SetCurrentPartition(const AInfo: TD64DirPartitionInfo); overload;
//      Set the current directory partition.  Use GetDirPartitions to get the
//          required information.
        procedure SetCurrentPartition(const AInfo: TD64DirPartitionInfo;
                out APrev: TD64DirPartitionInfo); overload;
//      Get the file entries for the current directory partition.
        procedure GetPartitionFiles(var AEntries: TD64DirEntries);
//      Get the BAM information for the current directory partition.
        procedure GetPartitionBAM(var ADiskBAM: TD64DiskBAM);

//      The current disk type
        property DiskType: TD64DiskType read FDiskType;
//      If a d71 image, whether the image is a single or double sided one.
        property SingleSide: Boolean read FSingleSide;
//      The total number of tracks in the current image.
        property TrackCount: TD64TrackNum read FTrkCount;
//      The maximum number of sectors on a track.
        property MaxSectors: Byte read FMaxSectors;
//      Whether the disk has valid DOS information.  Uses only very minimal checks.
        property ValidVersion: Boolean read FValidVersion;
//      The disk DOS version.
        property DOSVersion: Byte read FDOSVersion;
//      The disk name.
        property DiskName: AnsiString read FDiskName;
//      The disk ID.
        property DiskID: AnsiString read FDiskID;
//      The DOS type.
        property DOSType: AnsiString read FDOSType;
//      Whether the disk is a GEOS disk.
        property GEOSDisk: Boolean read FGEOSDisk;
//      The major version of the GEOS format if a GEOS disk.
        property GEOSVerMajor: Byte read FGEOSVerMajor;
//      The minor version of the GEOS format if a GEOS disk.
        property GEOSVerMinor: Byte read FGEOSVerMinor;
    end;

//  Base class for D64Image exceptions (for filtering)
    D64ImageException = class(Exception);
//  An exception raised when the disk file does not exist.
    ED64FileDoesNotExist = class(D64ImageException);
//  An exception raised when the disk file format cannot be determined.
    ED64UnknownFormat = class(D64ImageException);
//  An exception raised when an invalid sector is selected.
    ED64InvalidSector = class(D64ImageException);
//  An exception raised when the disk file cannot be opened.
    ED64UnableToOpen = class(D64ImageException);
//  An exception raised when the disk is full.
    ED64DiskFull = class(D64ImageException);


//Convert the given DOS file type byte to a AnsiString (as per BASIC, with or
//      without splat and read-only markers)
function  D64FileTypeToStr(const AType: Byte;
        const AStateFlags: Boolean = True): AnsiString;
//Convert the given GEOS file structure byte into a AnsiString.
function  D64GEOSStructToStr(const AStruct: Byte): AnsiString;
//Convert the given GEOS file type into a AnsiString.
function  D64GEOSFileTypeToStr(const AType: Byte): AnsiString;

procedure D64DecodeFileType(const AType: Byte; out AFileType: TD64FileType;
		out AFileStates: TD64FileStates);
function  D64EncodeFileType(const AFileType: TD64FileType;
		const AFileStates: TD64FileStates): Byte;

implementation

uses
    C64D64ImageStrs;

const
	ARR_VAL_D64TRKSECTRS: array[1..40] of Byte = (
    		21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, //1-17
            19, 19, 19, 19, 19, 19, 19, //18-24
            18, 18, 18, 18, 18, 18,	//25-30
            17, 17, 17, 17, 17, 17, 17, 17, 17, 17);  //31-40
	ARR_VAL_D71TRKSECTRS: array[1..70] of Byte = (
    		21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, //1-17
            19, 19, 19, 19, 19, 19, 19, //18-24
            18, 18, 18, 18, 18, 18,	//25-30
            17, 17, 17, 17, 17,  //31-35
    		21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, //36-52
            19, 19, 19, 19, 19, 19, 19, //53-59
            18, 18, 18, 18, 18, 18,	//60-65
            17, 17, 17, 17, 17); //66-70

    LIT_TOK_D64GEOSFMT = 'GEOS format';
    LIT_TOK_D64FTYPDEL = 'DEL';
    LIT_TOK_D64FTYPSEQ = 'SEQ';
    LIT_TOK_D64FTYPPRG = 'PRG';
    LIT_TOK_D64FTYPUSR = 'USR';
    LIT_TOK_D64FTYPREL = 'REL';
    LIT_TOK_D64FTYPCBM = 'CBM';
    LIT_TOK_D64FTYPUNK = '???';

    LIT_TOK_D64GEOSSEQ = 'Sequential';
    LIT_TOK_D64GEOSVLR = 'GEOS VLIR';
    LIT_TOK_D64GEOSUNK = 'Unknown';

    LIT_TOK_D64GEOSNGS = 'Not a GEOS file';
    LIT_TOK_D64GEOSBAS = 'BASIC';
    LIT_TOK_D64GEOSASM = 'Assembler';
    LIT_TOK_D64GEOSDAT = 'Data file';
    LIT_TOK_D64GEOSSYS = 'System File';
    LIT_TOK_D64GEOSDSA = 'Desk Accessory';
    LIT_TOK_D64GEOSAPP = 'Application';
    LIT_TOK_D64GEOSAPD = 'Application Data';
    LIT_TOK_D64GEOSFNT = 'Font File';
    LIT_TOK_D64GEOSPRN = 'Printer Driver';
    LIT_TOK_D64GEOSINP = 'Input Driver';
    LIT_TOK_D64GEOSDSK = 'Disk Driver/Device';
    LIT_TOK_D64GEOSSBT = 'System Boot File';
    LIT_TOK_D64GEOSTMP = 'Temporary';
    LIT_TOK_D64GEOSAEX = 'Auto-Execute File';


procedure D64DecodeFileType(const AType: Byte; out AFileType: TD64FileType;
		out AFileStates: TD64FileStates);
	begin
    AFileType:= AType and $07;

    AFileStates:= [];

    if  (AType and $20) <> 0 then
        Include(AFileStates, dfsReplacing);

    if  (AType and $40) <> 0 then
        Include(AFileStates, dfsReadOnly);

    if  (AType and $80) <> 0 then
        Include(AFileStates, dfsClosed);
    end;

function  D64EncodeFileType(const AFileType: TD64FileType;
		const AFileStates: TD64FileStates): Byte;
	begin
    Result:= AFileType;

    if  dfsReplacing in AFileStates then
        Result:= Result or $20;

    if  dfsReadOnly in AFileStates then
    	Result:= Result or $40;

    if  dfsClosed in AFileStates then
		Result:= Result or $80;
	end;


function D64FileTypeToStr(const AType: Byte; const AStateFlags: Boolean): AnsiString;
    begin
    case AType and $0F of
        0:
            Result:= LIT_TOK_D64FTYPDEL;
        1:
            Result:= LIT_TOK_D64FTYPSEQ;
        2:
            Result:= LIT_TOK_D64FTYPPRG;
        3:
            Result:= LIT_TOK_D64FTYPUSR;
        4:
            Result:= LIT_TOK_D64FTYPREL;
        5:
            Result:= LIT_TOK_D64FTYPCBM;
        else
            Result:= LIT_TOK_D64FTYPUNK;
        end;

    if  AStateFlags then
        begin
        if  (AType and $40) <> 0 then
            Result:= Result + '<'
        else
            Result:= Result + ' ';

        if  (AType and $80) = 0 then
            Result:= '*' + Result
        else
            Result:= ' ' + Result;

        end;
    end;

function  D64GEOSStructToStr(const AStruct: Byte): AnsiString;
    begin
    case AStruct of
        $00:
            Result:= LIT_TOK_D64GEOSSEQ;
        $01:
            Result:= LIT_TOK_D64GEOSVLR;
        else
            Result:= LIT_TOK_D64GEOSUNK + Format(' ($%2.2x)', [AStruct]);
        end;
    end;

function  D64GEOSFileTypeToStr(const AType: Byte): AnsiString;
    begin
    case AType of
        $00:
            Result:= LIT_TOK_D64GEOSNGS;
        $01:
            Result:= LIT_TOK_D64GEOSBAS;
        $02:
            Result:= LIT_TOK_D64GEOSASM;
        $03:
            Result:= LIT_TOK_D64GEOSDAT;
        $04:
            Result:= LIT_TOK_D64GEOSSYS;
        $05:
            Result:= LIT_TOK_D64GEOSDSA;
        $06:
            Result:= LIT_TOK_D64GEOSAPP;
        $07:
            Result:= LIT_TOK_D64GEOSAPD;
        $08:
            Result:= LIT_TOK_D64GEOSFNT;
        $09:
            Result:= LIT_TOK_D64GEOSPRN;
        $0A:
            Result:= LIT_TOK_D64GEOSINP;
        $0B:
            Result:= LIT_TOK_D64GEOSDSK;
        $0C:
            Result:= LIT_TOK_D64GEOSSBT;
        $0D:
            Result:= LIT_TOK_D64GEOSTMP;
        $0E:
            Result:= LIT_TOK_D64GEOSAEX;
        else
            Result:= LIT_TOK_D64GEOSUNK + Format(' ($%2.2x)', [AType]);
        end;
    end;

{ TD64DirEntry }

//procedure TD64DirEntry.SetTrack(const AValue: Byte);
//  begin
//  Track:= TD64TrackNum(AValue);
//  EntryData[0]:= AValue;
//  end;

//procedure TD64DirEntry.SetSector(const AValue: Byte);
// begin
//  Sector:= AValue;
//  EntryData[1]:= AValue
//  end;

procedure TD64DirEntry.SetFileType(const AValue: Byte);
    begin
    FileType:= AValue;
    EntryData[2]:= AValue;
    end;

procedure TD64DirEntry.SetDataTS(const ATrack, ASector: Byte);
    begin
    EntryData[3]:= ATrack;
    EntryData[4]:= ASector;
    end;

procedure TD64DirEntry.SetFileName(const AName: AnsiString);
    var
    i: Integer;

    begin
    FileName:= Copy(AName + LIT_LBL_D64BLNKFLN, 1, 16);
    for i:= $05 to $14 do
        EntryData[i]:= Byte(AnsiChar(FileName[i - $04]));
    end;

procedure TD64DirEntry.SetRelTS(const ATrack, ASector: Byte);
    begin
    EntryData[$15]:= ATrack;
    EntryData[$16]:= ASector;
    end;

procedure TD64DirEntry.SetRelRecSize(const AValue: Byte);
    begin
    EntryData[$17]:= AValue;
    end;

procedure TD64DirEntry.SetFileSize(const AValue: Word);
    begin
    EntryData[$1E]:= Byte(AValue and $FF);
    EntryData[$1F]:= Byte((AValue and $FF00) shr 8);
    end;

procedure TD64DirEntry.SetGEOSInfoTS(const ATrack, ASector: Byte);
    begin
    SetRelTS(ATrack, ASector);
    end;

procedure TD64DirEntry.SetGEOSStructure(const AValue: Byte);
    begin
    SetRelRecSize(AValue);
    end;

procedure TD64DirEntry.SetGEOSFileType(const AValue: Byte);
    begin
    EntryData[$18]:= AValue;
    end;

procedure TD64DirEntry.SetGEOSYear(const AYear: Word);
    begin
    EntryData[$19]:= Byte(AYear - 1900);
    end;

procedure TD64DirEntry.SetGEOSMonth(const AValue: Byte);
    begin
    EntryData[$1A]:= AValue;
    end;

procedure TD64DirEntry.SetGEOSDay(const AValue: Byte);
    begin
    EntryData[$1B]:= AValue;
    end;

procedure TD64DirEntry.SetGEOSHour(const AValue: Byte);
    begin
    EntryData[$1C]:= AValue;
    end;

procedure TD64DirEntry.SetGEOSMinute(const AValue: Byte);
    begin
    EntryData[$1D]:= AValue;
    end;

procedure TD64DirEntry.SetGEOSDateTime(const ADateTime: TDateTime);
    var
    yr,
    mo,
    dy,
    hr,
    mn,
    sc,
    ms: Word;

    begin
    DecodeDate(ADateTime, yr, mo, dy);
    DecodeTime(ADateTime, hr, mn, sc, ms);

    SetGEOSYear(yr);
    SetGEOSMonth(mo);
    SetGEOSDay(dy);
    SetGEOSHour(hr);
    SetGEOSMinute(mn);
    end;

{ TD64Image }

procedure TD64Image.DoLoadFromFile(const AFile: string);
    var
    f: TFileStream;

    begin
    if  FileExists(AFile) then
        try
		    f:= TFileStream.Create(AFile, fmShareCompat or fmOpenRead);
            try
        	    if  CompareText(ExtractFileExt(AFile), '.D64') = 0 then
                    begin
//                  35 track, no errors        174848
//                  35 track, 683 error bytes  175531
//                  40 track, no errors        196608
//                  40 track, 768 error bytes  197376

				    if  (f.Size = 174848)
                    or  (f.Size = 175531) then
					    FTrkCount:= 35
                    else if (f.Size = 196608)
                    or (f.Size = 197376) then
                	    FTrkCount:= 40
                    else
                	    raise ED64UnknownFormat.Create(
                    		    Format(STR_FMT_D64FORMATUNK, [AFile]));

                    FDiskType:= ddt1541;
                    FSingleSide:= True;
                    end
                else if  CompareText(ExtractFileExt(AFile), '.D71') = 0 then
                    begin
//                  35 track, no errors        174848
//                  35 track, 683 error bytes  175531
//                  70 tracks, 349696 bytes.
//                  70 tracks, 351062, with error bytes.
				    if  (f.Size = 174848)
                    or  (f.Size = 175531) then
                        begin
					    FTrkCount:= 35;
                        FSingleSide:= True;
                        end
                    else if (f.Size = 349696)
                    or (f.Size = 351062) then
                        begin
					    FTrkCount:= 70;
                        FSingleSide:= False;
                        end
                    else
                	    raise ED64UnknownFormat.Create(
                    		    Format(STR_FMT_D64FORMATUNK, [AFile]));

                    FDiskType:= ddt1571;
                    end
                else if  CompareText(ExtractFileExt(AFile), '.D81') = 0 then
                    begin
//                  819200 bytes,
//                  822400 bytes with errors.

                    if (f.Size = 819200)
                    or (f.Size = 822400) then
                        begin
					    FTrkCount:= 80;
                        FSingleSide:= False;
                        end
                    else
                	    raise ED64UnknownFormat.Create(
                    		    Format(STR_FMT_D64FORMATUNK, [AFile]));

                    FDiskType:= ddt1581;
                    end
                else
                    raise ED64UnknownFormat.Create(Format(STR_FMT_D64EXTNSNUNK,
                            [AFile]));

                FData.Clear;
                FData.CopyFrom(f, f.Size);

        	    finally
                f.Free;
                end
            except
            raise ED64UnableToOpen.Create(Format(STR_FMT_D64FILENOTOP, [AFile]));
            end
    else
    	raise ED64FileDoesNotExist.Create(Format(STR_FMT_D64FILENOTEX, [AFile]));

    if  FDiskType in [ddt1541, ddt1571] then
        FMaxSectors:= 21
    else
        FMaxSectors:= 40;

    DoAnalyseDOSDetails;
	end;

procedure TD64Image.DoSaveToFile(const AFile: string);
    begin
//dengland Should do some sanity checking on file extension and DiskType
//fixme dengland Sanity check DoSaveToFile
    FData.Position:= 0;
    FData.SaveToFile(AFile);
    end;

function TD64Image.DoValidateTrackSector(const ATrack: TD64TrackNum;
		const ASector: TD64SectorNum): Word;
    var
    i: TD64TrackNum;

	begin
    if  FTrkCount >= ATrack then
    	if  (GetSectorsForTrack(ATrack) - 1) >= ASector then
            begin
            Result:= 0;
            i:= 1;
            while i < ATrack do
            	begin
        	    Inc(Result, GetSectorsForTrack(i));
                Inc(i);
                end;

            Inc(Result, Ord(ASector));
            end
        else
    	    raise ED64InvalidSector.Create(
            		Format(STR_FMT_D64SECOUTRNG, [Ord(ATrack), Ord(ASector)]))
    else
	    raise ED64InvalidSector.Create(
        		Format(STR_FMT_D64TRKOUTRNG, [Ord(ATrack)]));
	end;

procedure TD64Image.DoGetSector(const ANum: Word; ADest: TStream);
    begin
    FData.Position:= ANum * VAL_SIZ_D64SECTRSIZE;
    ADest.CopyFrom(FData, VAL_SIZ_D64SECTRSIZE);
	end;

procedure TD64Image.DoGetFileEntries(const AStream: TMemoryStream;
        const ATrack: TD64TrackNum; const ASector: TD64SectorNum;
        var AEntries: TD64DirEntries; const AFullList: Boolean);
    var
    b,
    nt,
    ns: Byte;
    fs: Word;
    fn: AnsiString;
    x,
    i: Integer;
    e: Byte;
    fe: TD64DirEntry;
    z: Integer;

    begin
    nt:= ATrack;
    ns:= ASector;

    x:= 0;

    repeat
//      SetLength(AEntries, Length(AEntries) + 8);
        AStream.Clear;
        GetRawSector(nt, ns, AStream);

        AStream.Position:= 0;
        for e:= 0 to 7 do
            begin
            fe.Track:= nt;
            fe.Sector:= ns;
            fe.EntryNum:= e;

            AStream.Position:= e * $20;
            for i:= 0 to $1F do
                fe.EntryData[i]:= AStream.ReadByte;

            AStream.Position:= e * $20 + 2;
            fe.FileType:= AStream.ReadByte;

            b:= AStream.ReadByte;
            if  (not AFullList)
            and (b = 0) then
                Continue;

            fe.DataTrack:= b;
            fe.DataSector:= AStream.ReadByte;

//          AStream.Position:= AStream.Position + 2;
            fn:= '';
            for i:= 0 to 15 do
                begin
                b:= AStream.ReadByte;
//              if  b in [$20..$7E] then
                    fn:= fn + AnsiChar(b);
//              else
//                  fn:= fn + ' ';
                end;
            fe.FileName:= fn;

            AStream.Position:= AStream.Position + 9;
            b:= AStream.ReadByte;
            fs:= b;
            b:= AStream.ReadByte;
            fs:= fs + b shl 8;
            fe.FileSize:= fs;

            z:= SizeOf(TD64DirEntry);

            SetLength(AEntries, Length(AEntries) + 1);
			AEntries[x]:= fe;

            Inc(x);
            end;

        AStream.Position:= 0;
        nt:= AStream.ReadByte;
        ns:= AStream.ReadByte;

        until nt = 0;
    end;

function TD64Image.DoGet1581BAM(const AStream: TMemoryStream;
        const ATrack: TD64TrackNum; const ATrkCnt: Byte;
        var ABAM: TD64DiskBAM): Integer;
    var
    x,
    s,
    o,
    i,
    j: Integer;

    begin
    Result:= 0;
    x:= 0;

    for s:= 0 to 1 do
        begin
        o:= s * 40;
        AStream.Clear;
        GetRawSector(ATrack, s + 1, AStream);

        AStream.Position:= $10;
        for i:= 0 to 39 do
            begin
            ABAM[o + i].FreeSectors:= AStream.ReadByte;

            Inc(Result, ABAM[o + i].FreeSectors);

            for j:= 0 to 4 do
                ABAM[o + i].Bitmap[j]:= AStream.ReadByte;

            Inc(x);
//dengland  This is just as nasty as a darn goto.
            if  x = ATrkCnt then
                Exit;
            end;
        end;
    end;

procedure TD64Image.DoAnalyseDOSDetails;
    var
    p: Cardinal;
    b: Byte;
    i: Byte;
    s: AnsiString;

    begin
    if  FDiskType in [ddt1541, ddt1571] then
        begin
        FCurrPartTrk:= 0;
        FCurrPartSiz:= 0;
        end
    else
        begin
        FCurrPartTrk:= 40;
        FCurrPartSiz:= 3200;
        end;

    FDiskName:= EmptyStr;
    FDiskID:= EmptyStr;
    FDOSType:= EmptyStr;

    if  FDiskType in [ddt1541, ddt1571] then
        p:= DoValidateTrackSector(18, 0) * VAL_SIZ_D64SECTRSIZE
    else
        p:= DoValidateTrackSector(40, 0) * VAL_SIZ_D64SECTRSIZE;

    FData.Position:= p + 2;
    FDOSVersion:= FData.ReadByte;

    FValidVersion:=
            ((FDiskType in [ddt1541, ddt1571]) and (FDOSVersion = $41)) or
            ((FDiskType = ddt1581) and (FDOSVersion = $44));

    if  FDiskType = ddt1571 then
        begin
        FData.Position:= p + 3;
        b:= FData.ReadByte;
        if  ((not FSingleSide)
        and  (b = $00))
        or  (FSingleSide
        and  (b = $80)) then
            FValidVersion:= False;
        end;

    if  FValidVersion then
        begin
        if  FDiskType = ddt1581 then
            FData.Position:= p + $04
        else
            FData.Position:= p + $90;

        for i:= 0 to 15 do
            begin
            b:= FData.ReadByte;
//            if  b in [$20..$7E] then
                FDiskName:= FDiskName + AnsiChar(b);
//            else
//                FDiskName:= FDiskName + ' ';
            end;

        if  FDiskType = ddt1581 then
            FData.Position:= p + $16
        else
            FData.Position:= p + $A2;

        for i:= 0 to 1 do
            begin
            b:= FData.ReadByte;
//            if  b in [$20..$7E] then
                FDiskID:= FDiskID + AnsiChar(b);
//            else
//                FDiskID:= FDiskID + ' ';
            end;

        if  FDiskType = ddt1581 then
            FData.Position:= p + $19
        else
            FData.Position:= p + $A5;

        for i:= 0 to 1 do
            begin
            b:= FData.ReadByte;
//            if  b in [$20..$7E] then
                FDOSType:= FDOSType + AnsiChar(b);
//            else
//                FDOSType:= FDOSType + ' ';
            end;

        FData.Position:= p + $AD;
        s:= '';
        for i:= 0 to 10 do
            begin
            b:= FData.ReadByte;
//            if  b in [$20..$7E] then
                s:= s + AnsiChar(b);
//            else
//                s:= s + ' ';
            end;

        FGEOSDisk:= CompareStr(s, LIT_TOK_D64GEOSFMT) = 0;

        if  FGEOSDisk then
            begin
            FData.ReadByte;
            FData.ReadByte;
            FGEOSVerMajor:= StrToInt(AnsiString(AnsiChar(FData.ReadByte)));
            FData.ReadByte;
            FGEOSVerMinor:= StrToInt(AnsiString(AnsiChar(FData.ReadByte)));
            end;
        end;
    end;

procedure TD64Image.DoAllocateNewSector(var ABAM: TD64DiskBAM;
        out ATrack: TD64TrackNum; out ASector: TD64SectorNum;
        const AAllocate: Boolean);
    var
    i,
    j: Integer;
    d: Byte;

    begin
//dengland Not going to try to do disk type interleave at this point.
    for i:= 0 to FTrkCount - 1 do
        begin
//dengland Don't allocate in directory tracks
        if  (FDiskType in [ddt1541, ddt1571])
        and (i = 17) then
            Continue
        else if (FDiskType = ddt1581)
        and (i = 39) then
            Continue;

        if  ABAM[i].FreeSectors > 0 then
            begin
            ATrack:= TD64TrackNum(i + 1);

            if  AAllocate then
            	Dec(ABAM[i].FreeSectors);

//dengland  Even in d64/d71 images, the BAM free sector count is correct for
//          each track, based on the number of sectors in that track.

            ASector:= 0;
            j:= 0;
            d:= 1;
            while (j < Length(ABAM[i].Bitmap)) and ((d and ABAM[i].Bitmap[j]) = 0) do
                begin
                if  (d = 128) then
                    begin
                    d:= 1;
                    Inc(j);
                    end
                else
                    d:= d shl 1;

                Inc(ASector);
                end;

            if  AAllocate then
            	ABAM[i].Bitmap[j]:= ABAM[i].Bitmap[j] and (not d);

//dengland  Should really do a sanity check here to be sure that there wasn't
//          an error in the BAM but we're assuming everything is okay.

            Exit;
            end;
        end;

    raise ED64DiskFull.Create(STR_ERR_D64DISKISFUL);
    end;

procedure TD64Image.DoDeallocateSectorChain(const ATrack: TD64TrackNum;
        const ASector: TD64SectorNum; var ABAM: TD64DiskBAM);
    var
    b,
    d: Byte;
    nt,
    ns: Byte;
    p: Cardinal;

    begin
    nt:= ATrack;
    ns:= ASector;
    while nt > 0 do
        begin
        p:= DoValidateTrackSector(nt, ns) * VAL_SIZ_D64SECTRSIZE;
        FData.Position:= p;

        b:= ns div 8;
        d:= 1 shl (ns mod 8);

        Inc(ABAM[nt - 1].FreeSectors);
        ABAM[nt - 1].Bitmap[b]:= ABAM[nt - 1].Bitmap[b] or d;

        nt:= FData.ReadByte;
        ns:= FData.ReadByte;
        end;
    end;

procedure TD64Image.DoWriteSector(const ATrack: TD64TrackNum;
        const ASector: TD64SectorNum; const ANxtTrack: Byte;
        const ANxtSector: Byte; const AStream: TStream);
    var
    d: PByte;
    i: Integer;

    begin
    DoWriteTrackSectorNextPointer(ATrack, ASector, ANxtTrack, ANxtSector, d);

    i:= 0;
    while (AStream.Position < AStream.Size) and (i < (VAL_SIZ_D64SECTRSIZE - 2)) do
        begin
        d^:= AStream.ReadByte;
        Inc(i);
        Inc(d);
        end;
    end;

procedure TD64Image.DoAllocateDirEntry(const AEntryData: TD64EntryData);
    var
    e: TD64DirEntries;
    i,
    j: Integer;
    lt,
    ls,
    nt,
    ns: Byte;
    m: Byte;
    b: TD64DiskBAM;
    d: PByte;
    p: Cardinal;

    procedure DoWriteEntryToImage;
        var
        d: PByte;
        p: Cardinal;
        j: Integer;

        begin
        p:= DoValidateTrackSector(e[i].Track, e[i].Sector) * VAL_SIZ_D64SECTRSIZE;
        d:= PByte(FData.Memory + p + e[i].EntryNum * $20);

        for j:= 0 to $1F do
            begin
            d^:= e[i].EntryData[j];
            Inc(d);
            end;
        end;

    begin
    GetFileEntries(e, True);

    i:= 0;
    while i < Length(e) do
        begin
        if  e[i].FileType = $00 then
            Break;

        Inc(i);
        end;

    if  i < Length(e) then
        begin
        Move(AEntryData[$02], e[i].EntryData[02], $1E);
        DoWriteEntryToImage;
        end
    else
        begin
//      Allocate a new directory sector
        GetDiskBAM(b);

        if  FDiskType in [ddt1541, ddt1571] then
            nt:= 18
        else
            nt:= 40;

        if  b[nt - 1].FreeSectors = 0 then
            raise ED64DiskFull.Create(STR_ERR_D64DISKISFUL);

        Dec(b[nt - 1].FreeSectors);

//dengland This (up to -->) should really be put in another procedure to share
//      with DoAllocateNewSector;
        ns:= 0;
        j:= 0;
        m:= 1;
        while (j < Length(b[nt - 1].Bitmap)) and ((m and b[nt - 1].Bitmap[j]) = 0) do
            begin
            if  (m = 128) then
                begin
                m:= 1;
                Inc(j);
                end
            else
                m:= m shl 1;

            Inc(ns);
            end;
//-->

//dengland  Should really do a sanity check here to be sure that there wasn't
//          an error in the BAM but we're assuming everything is okay.

        b[nt - 1].Bitmap[j]:= b[nt - 1].Bitmap[j] and (not m);

        lt:= e[i - 1].Track;
        ls:= e[i - 1].Sector;

        DoWriteTrackSectorNextPointer(lt, ls, nt, ns, d);

        p:= DoValidateTrackSector(nt, ns) * VAL_SIZ_D64SECTRSIZE;
        d:= PByte(FData.Memory + p);
        for j:= $00 to $FF do
            begin
            d^:= $00;
            Inc(d);
            end;

        d:= PByte(FData.Memory + p);
        i:= 0;
        Move(AEntryData[$02], e[i].EntryData[02], $1E);
        e[i].EntryData[0]:= 00;
        e[i].EntryData[1]:= $FF;
        e[i].Track:= nt;
        e[i].Sector:= ns;
        e[i].EntryNum:= 0;

        DoWriteEntryToImage;
        DoWriteDiskBAM(b);
        end;
    end;

procedure TD64Image.DoWriteDiskBAM(const ABAM: TD64DiskBAM);
    var
    s: Integer;
    d: PByte;
    p: Cardinal;
    o,
    i,
    j: Integer;

    begin
    if  FDiskType = ddt1581 then
        begin
        for s:= 0 to 1 do
            begin
            p:= DoValidateTrackSector(40, s + 1) * VAL_SIZ_D64SECTRSIZE;
            d:= PByte(FData.Memory + p + $10);
            o:= s * 40;
            for i:= 0 to 39 do
                begin
                d^:=  ABAM[o + i].FreeSectors;
                Inc(d);

                for j:= 0 to High(ABAM[o + i].Bitmap) do
                    begin
                    d^:= ABAM[o + i].Bitmap[j];
                    Inc(d);
                    end;
                end;
            end;
        end
    else
        begin
        p:= DoValidateTrackSector(18, 0) * VAL_SIZ_D64SECTRSIZE;
        d:= PByte(FData.Memory + p + $04);

        for i:= 0 to 34 do
            begin
            d^:= ABAM[i].FreeSectors;
            Inc(d);

            for j:= 0 to High(ABAM[i].Bitmap) do
                begin
                d^:= ABAM[i].Bitmap[j];
                Inc(d);
                end;
            end;

        if  (FDiskType = ddt1571)
        and not FSingleSide then
            begin
            d:= PByte(FData.Memory + p + $DD);

            for i:= 35 to 69 do
                begin
                d^:= ABAM[i].FreeSectors;
                Inc(d);
                end;

            p:= DoValidateTrackSector(53, 0) * VAL_SIZ_D64SECTRSIZE;
            d:= PByte(FData.Memory + p);

            for i:= 35 to 69 do
                for j:= 0 to High(ABAM[i].Bitmap) do
                    begin
                    d^:= ABAM[i].Bitmap[j];
                    Inc(d);
                    end;
            end;
        end;
    end;

procedure TD64Image.DoWriteTrackSectorNextPointer(const ATrack: TD64TrackNum;
        const ASector: TD64SectorNum; const ANxtTrack: Byte;
        const ANxtSector: Byte; out AData: PByte);
    var
    p: Cardinal;

    begin
    p:= DoValidateTrackSector(ATrack, ASector) * VAL_SIZ_D64SECTRSIZE;

    AData:= PByte(FData.Memory + p);

    AData^:= ANxtTrack;
    Inc(AData);
    AData^:= ANxtSector;
    Inc(AData);
    end;

constructor TD64Image.Create;
	begin
    FCurrPartTrk:= $00;

    FData:= TMemoryStream.Create;
	end;

constructor TD64Image.Create(const AFile: string);
	begin
    FCurrPartTrk:= $00;

    FData:= TMemoryStream.Create;
    try
    	DoLoadFromFile(AFile);

    	except
        on e: Exception do
        	begin
	        FreeAndNil(FData);
    	    raise e;
            end;
        end;
    end;

destructor TD64Image.Destroy;
	begin
    FreeAndNil(FData);

	inherited Destroy;
	end;

procedure TD64Image.LoadFromFile(const AFile: string);
	begin
	DoLoadFromFile(AFile);
	end;

procedure TD64Image.SaveToFile(const AFile: string);
    begin
    DoSaveToFile(AFile);
    end;

procedure TD64Image.FormatImage(const ADiskName: AnsiString;
    const ADiskID: AnsiString; const ADiskType: TD64DiskType;
    const ASingleSide: Boolean; const AGEOSDisk: Boolean);
    var
    w: Integer;

    procedure DoInitData;
        begin
        FData.Clear;

        w:= 0;
        FDiskType:= ADiskType;
        case ADiskType of
            ddt1541:
                begin
                FTrkCount:= 35;
                FMaxSectors:= 21;
    		    FData.SetSize(174848);
                w:= 3;
                end;
            ddt1571:
                begin
                FTrkCount:= 70;
                FMaxSectors:= 21;

                if  FSingleSide then
                    FData.SetSize(174848)
                else
    		    	FData.SetSize(349696);
                w:= 3;
                end;
            ddt1581:
                begin
                FTrkCount:= 80;
                FMaxSectors:= 40;
    		    FData.SetSize(819200);
                w:= 5;
                end;
            end;

        FillChar(PByte(FData.Memory)^, FData.Size, $00);
        end;

    procedure DoInitBAM;
        var
        b: TD64DiskBAM;
        i: Integer;

        begin
        SetLength(b, FTrkCount);
        for i:= 0 to FTrkCount - 1 do
            begin
            b[i].FreeSectors:= GetSectorsForTrack(TD64TrackNum(i + 1));
            SetLength(b[i].Bitmap, w);
            FillChar(b[i].Bitmap[0], w, $FF);

            if  ADiskType in [ddt1541, ddt1571] then
                case GetSectorsForTrack(TD64TrackNum(i + 1)) of
                    21:
                        b[i].Bitmap[w - 1]:= $1F;
                    19:
                        b[i].Bitmap[w - 1]:= $07;
                    18:
                        b[i].Bitmap[w - 1]:= $03;
                    17:
                        b[i].Bitmap[w - 1]:= $01;
                end;
            end;

        case ADiskType of
            ddt1541:
                begin
                Dec(b[17].FreeSectors, 2);
                b[17].Bitmap[0]:= $FC;

                if  AGEOSDisk then
                    begin
//dengland          Actually track 19 not 18 (is less by one here -- index)
                    Dec(b[18].FreeSectors);
                    b[18].Bitmap[0]:= $FD;
                    end;
                end;
            ddt1571:
                begin
                Dec(b[17].FreeSectors, 2);
                b[17].Bitmap[0]:= $FC;

                b[52].FreeSectors:= 0;
                b[52].Bitmap[0]:= $00;
                b[52].Bitmap[1]:= $00;
                b[52].Bitmap[2]:= $00;

//dengland      Don't really know if GEOS does this to a D71 because I can't
//              get GEOS to accept a D71 image but I'm assuming it does.
                if  AGEOSDisk then
                    begin
//dengland          Actually track 19 not 18 (is less by one here -- index)
                    Dec(b[18].FreeSectors);
                    b[18].Bitmap[0]:= $FD;
                    end;
                end;
            ddt1581:
                begin
                Dec(b[39].FreeSectors, 4);
                b[39].Bitmap[0]:= $F0;

                if  AGEOSDisk then
                    begin
                    Dec(b[39].FreeSectors);
                    b[39].Bitmap[2]:= $F7;
                    end;
                end;
            end;

        DoWriteDiskBAM(b);
        end;

    procedure DoInitDOS;
        var
        b: Byte;
        p: Cardinal;
        d: PByte;
        s: AnsiString;
        i,
        j: Integer;

        begin
        if  FDiskType in [ddt1541, ddt1571] then
            begin
            p:= DoValidateTrackSector(18, 0) * VAL_SIZ_D64SECTRSIZE;
            d:= PByte(FData.Memory + p);
            d^:= 18;
            Inc(d);
            d^:= 1;
            Inc(d);
            d^:= $41;
            Inc(d);
            end
        else
            begin
            p:= DoValidateTrackSector(40, 0) * VAL_SIZ_D64SECTRSIZE;
            d:= PByte(FData.Memory + p);
            d^:= 40;
            Inc(d);
            d^:= 3;
            Inc(d);
            d^:= $44;
            Inc(d);
            end;

        if  FDiskType = ddt1571 then
            if  FSingleSide then
              	d^:= $00
            else
            	d^:= $80
        else
            begin
            d^:= $00;
            Inc(d);
            end;


        if  FDiskType <> ddt1581 then
            d:= PByte(FData.Memory + p + $90);

        s:= Copy(ADiskName + LIT_LBL_D64BLNKFLN, 1, 16);
        for i:= 0 to 15 do
            begin
            b:= Byte(AnsiChar(s[i + 1]));
            d^:= b;
            Inc(d);
            end;

        for i:= 0 to 1 do
            begin
            d^:= $A0;
            Inc(d);
            end;

        s:= Copy(ADiskID + LIT_LBL_D64BLNKFLN, 1, 2);
        for i:= 0 to 1 do
            begin
            b:= Byte(AnsiChar(s[i + 1]));
            d^:= b;
            Inc(d);
            end;

        d^:= $A0;
        Inc(d);

        if  FDiskType = ddt1581 then
            begin
            d^:= $33;
            Inc(d);
            d^:= $44;
            Inc(d);

            b:= 1
            end
        else
            begin
            d^:= $32;
            Inc(d);
            d^:= $41;
            Inc(d);

            b:= 3;
            end;

        for i:= 0 to b do
            begin
            d^:= $A0;
            Inc(d);
            end;

        if  AGEOSDisk then
            begin
            d:= PByte(FData.Memory + p + $AD);
            s:= LIT_TOK_D64GEOSFMT + ' V1.0';
            for i:= 0 to 15 do
                begin
                b:= Byte(AnsiChar(s[i + 1]));
                d^:= b;
                Inc(d);
                end;
            end;

        if  FDiskType = ddt1581 then
            begin
            if  AGEOSDisk then
                begin
                d:= PByte(FData.Memory + p + $AB);
                d^:= $28;
                Inc(d);
                d^:= $13;
                end;

            s:= Copy(ADiskID + LIT_LBL_D64BLNKFLN, 1, 2);

            for i:= 0 to 1 do
                begin
                p:= DoValidateTrackSector(40, i + 1) * VAL_SIZ_D64SECTRSIZE;
                d:= PByte(FData.Memory + p);
                if  i = 0 then
                    begin
                    d^:= 40;
                    Inc(d);
                    d^:= 2;
                    Inc(d);
                    end
                else
                    begin
                    d^:= 0;
                    Inc(d);
                    d^:= $FF;
                    Inc(d);
                    end;

                d^:= $44;
                Inc(d);
                d^:= $BB;
                Inc(d);

                for j:= 0 to 1 do
                    begin
                    b:= Byte(AnsiChar(s[j + 1]));
                    d^:= b;
                    Inc(d);
                    end;

                d^:= $C0;
                end;

            p:= DoValidateTrackSector(40, 3) * VAL_SIZ_D64SECTRSIZE;
            d:= PByte(FData.Memory + p + 1);
            d^:= $FF;

            if  AGEOSDisk then
                begin
                p:= DoValidateTrackSector(40, 19) * VAL_SIZ_D64SECTRSIZE;
                d:= PByte(FData.Memory + p + 1);
                d^:= $FF;
                end;
            end
        else
            begin
//dengland  Don't really know if D71 is the same because GEOS is barfing at all
//          my attempts to load a D71 image.  I'll make it the same, anyway.
//          if  ADiskType = ddt1541 then
                if  AGEOSDisk then
                    begin
                    d:= PByte(FData.Memory + p + $AB);
                    d^:= $13;
                    Inc(d);
                    d^:= $01;

                    p:= DoValidateTrackSector(19, 1) * VAL_SIZ_D64SECTRSIZE;
                    d:= PByte(FData.Memory + p + 1);
                    d^:= $FF;
                    end;

            p:= DoValidateTrackSector(18, 1) * VAL_SIZ_D64SECTRSIZE;
            d:= PByte(FData.Memory + p + 1);
            d^:= $FF;
            end;
        end;

    begin
    if  ADiskType = ddt1541 then
        FSingleSide:= True
    else if ADiskType = ddt1571 then
        FSingleSide:= ASingleSide
    else
        FSingleSide:= False;

    DoInitData;
    DoInitBAM;
    DoInitDOS;

    DoAnalyseDOSDetails;
    end;

function TD64Image.IsDirectoryPartition(const AEntry: TD64DirEntry; out
		ADataPos: Cardinal): Boolean;
	var
	d: PByte;

	begin
	ADataPos:= 0;

//  Check the basic structural constraints.
//      - Must be the CBM file type
//      - Must be larger than 3 tracks (120 blocks)
//      - Must allocate whole tracks
//      - Must start on sector 0
//      - Must not cross track 40
    Result:= ((AEntry.FileType and $07) = 5) and (AEntry.FileSize >= 120) and
            ((AEntry.FileSize mod 40) = 0) and (AEntry.EntryData[4] = 0) and
            ((AEntry.EntryData[3] > 40) or
             (AEntry.EntryData[3] + (AEntry.FileSize div 40) < 40));
//dengland Whew...

//  Check that it was formatted, too.
    if  Result then
        begin
        ADataPos:= DoValidateTrackSector(AEntry.EntryData[3],
                AEntry.EntryData[4]) * VAL_SIZ_D64SECTRSIZE;
        d:= PByte(FData.Memory + ADataPos + 2);
        Result:= d^ = $44;
        end;
    end;

procedure TD64Image.GetRawSector(const ATrack: TD64TrackNum;
		const ASector: TD64SectorNum; ADest: TStream);
	begin
    DoGetSector(DoValidateTrackSector(ATrack, ASector), ADest);
	end;

procedure TD64Image.GetDataChain(const ATrack: TD64TrackNum;
		const ASector: TD64SectorNum; ADest: TStream; out ActualSize: Cardinal);
    var
    m: TMemoryStream;
    nt,
    ns: Byte;
    nextTrk: TD64TrackNum;
    nextSec: TD64SectorNum;

    begin
    ActualSize:= 0;

    m:= TMemoryStream.Create;
    try
    	nextTrk:= ATrack;
        nextSec:= ASector;
        repeat
            m.Clear;
        	GetRawSector(nextTrk, nextSec, m);

            m.Position:= 0;
            nt:= m.ReadByte;
            ns:= m.ReadByte;

            ADest.CopyFrom(m, VAL_SIZ_D64SECTRSIZE - 2);

    	    if  nt > 0 then
         	    begin
                Inc(ActualSize, VAL_SIZ_D64SECTRSIZE - 2);

                nextTrk:= TD64TrackNum(nt);
                nextSec:= TD64SectorNum(ns);
                end
           	else
            	Inc(ActualSize, ns - 1);

    	    until nt = 0;
    	finally
        m.Free;
        end;
    end;

procedure TD64Image.GetFileEntries(var AEntries: TD64DirEntries;
 		const AFullList: Boolean);
    var
    m: TMemoryStream;
    nt,
    ns: Byte;

    begin
    SetLength(AEntries, 0);
    if  not FValidVersion then
        Exit;

    m:= TMemoryStream.Create;
    try
        if  FDiskType = ddt1581 then
            begin
            nt:= 40;
            ns:= 3;
            end
        else
            begin
            nt:= 18;
            ns:= 1;
            end;

            DoGetFileEntries(m, nt, ns, AEntries, AFullList);

        finally
        m.Free;
        end;
    end;

function TD64Image.GetDiskBAM(var ADiskBAM: TD64DiskBAM): Integer;
    var
    h,
    w,
    i,
    j: Integer;
    m: TMemoryStream;

    begin
    Result:= 0;

    if  FDiskType = ddt1541 then
        h:= 35
    else
        h:= FTrkCount;

    SetLength(ADiskBAM, h);

    if  FDiskType in [ddt1541, ddt1571] then
        w:= 3
    else
        w:= 5;

    for i:= 0 to h - 1 do
        SetLength(ADiskBAM[i].Bitmap, w);

    m:= TMemoryStream.Create;
    try
        if  FDiskType = ddt1581 then
            Result:= DoGet1581BAM(m, 40, FTrkCount, ADiskBAM)
        else
            begin
            GetRawSector(18, 0, m);

            m.Position:= $04;
            for i:= 0 to 34 do
                begin
                ADiskBAM[i].FreeSectors:= m.ReadByte;

                Inc(Result, ADiskBAM[i].FreeSectors);

                for j:= 0 to 2 do
                    ADiskBAM[i].Bitmap[j]:= m.ReadByte;
                end;

            if  (FDiskType = ddt1571)
            and not FSingleSide then
                begin
                m.Position:= $DD;

                for i:= 35 to FTrkCount - 1 do
                    begin
                    ADiskBAM[i].FreeSectors:= m.ReadByte;
                    Inc(Result, ADiskBAM[i].FreeSectors)
                    end;

                m.Clear;
                GetRawSector(53, 0, m);

                m.Position:= $00;
                for i:= 35 to FTrkCount - 1 do
                    for j:= 0 to 2 do
                        ADiskBAM[i].Bitmap[j]:= m.ReadByte;
                end;
            end;

        finally
        m.Free;
        end;
    end;

function TD64Image.GetDiskBlockCount: Integer;
	var
    i: Integer;

	begin
    Result:= 0;

    case FDiskType of
        ddt1581:
        	Result:= 40 * 80;
    	ddt1541:
            for i:= 1 to FTrkCount do
                Result:= Result + ARR_VAL_D64TRKSECTRS[i];
       	ddt1571:
            for i:= 1 to FTrkCount do
                Result:= Result + ARR_VAL_D71TRKSECTRS[i];
        end;
    end;

procedure TD64Image.AllocateDiskSectors(const AStream: TStream; out
        AStartTrk: TD64TrackNum; out AStartSec: TD64SectorNum;
        out ABlocksUsed: Word);
    var
    b: TD64DiskBAM;
    lt,
    nt: Byte;
    ls,
    ns: Byte;

    begin
    if AStream.Position = AStream.Size then
        Exit;

    GetDiskBAM(b);

    DoAllocateNewSector(b, AStartTrk, AStartSec, False);
    ABlocksUsed:= 0;

    lt:= AStartTrk;
    ls:= AStartSec;
    while AStream.Position < AStream.Size do
        begin
        DoAllocateNewSector(b, TD64TrackNum(lt), TD64SectorNum(ls));

        if  (AStream.Size - AStream.Position) > VAL_SIZ_D64BLOCKSIZE then
            DoAllocateNewSector(b, TD64TrackNum(nt), TD64SectorNum(ns), False)
        else
            begin
        	nt:= 0;
            ns:= AStream.Size - AStream.Position + 1;
            end;

        Inc(ABlocksUsed);

        DoWriteSector(lt, ls, nt, ns, AStream);
        lt:= nt;
        ls:= ns;
        end;

    DoWriteDiskBAM(b);
    end;

procedure TD64Image.AllocateFileEntry(const AEntryData: TD64EntryData);
    begin
    DoAllocateDirEntry(AEntryData);
    end;

procedure TD64Image.ScratchFileEntry(const ATrack: TD64TrackNum;
        const ASector: TD64SectorNum; const AEntry: TD64EntryNum);
    var
    e: TD64EntryData;
    b: TD64DiskBAM;
    g: Boolean;
    nt,
    ns: Byte;
    i: Integer;
    d: PByte;

    begin
    e[0]:= 0;

    FData.Position:=
            DoValidateTrackSector(ATrack, ASector) * VAL_SIZ_D64SECTRSIZE +
            AEntry * $20;
    FData.ReadBuffer(e[0], SizeOf(TD64EntryData));

    if  (e[2] and $07) = 0 then
        Exit;

    g:= ((e[2] and $07) in [1..3]) and (e[$18] <> 0);

    GetDiskBAM(b);

    DoDeallocateSectorChain(e[3], e[4], b);

//dengland If its a REL file, we need to handle it here.  At the moment, REL
//      files aren't handled.
//fixme dengland Implement REL file scratching.
    if  (e[2] and $07) = 4 then
        raise Exception.Create('Currently unable to scratch REL files.');

    if  g then
        begin
        DoDeallocateSectorChain(e[$15], e[$16], b);

        if  e[$17] = 1 then
            begin
            d:= (FData.Memory + DoValidateTrackSector(e[3], e[4]) *
                    VAL_SIZ_D64SECTRSIZE + 2);
            for i:= 0 to 126 do
                begin
                nt:= d^;
                Inc(d);
                ns:= d^;
                Inc(d);

                if  nt > 0 then
                    DoDeallocateSectorChain(nt, ns, b);
                end;
            end;
        end;

    DoWriteDiskBAM(b);

    d:= PByte(FData.Memory +
            DoValidateTrackSector(ATrack, ASector) * VAL_SIZ_D64SECTRSIZE +
            AEntry * $20 + 2);
    d^:= $00;
    end;

procedure TD64Image.GetDirPartitions(var AParts: TD64DirPartitions);
    type
//dengland Using this so as to fragment memory as little as possible and not
//      waste so much time in setting the length of the TD64DirPartitions array.
    PD64DirPartition = ^TD64DirPartition;

    var
    l: TList;
    i: Integer;
    r: PD64DirPartition;
    m: TMemoryStream;

    procedure DoReadPartDiskDetail(const ADataPos: Cardinal;
            out ADiskName: AnsiString; out ADiskID: AnsiString; out ADOSType: AnsiString);
        var
        d: PByte;
        b: Byte;
        i: Integer;

        begin
        d:= PByte(FData.Memory + ADataPos + $04);

        ADiskName:= EmptyStr;
        ADiskID:= EmptyStr;
        ADOSType:= EmptyStr;

        for i:= 0 to 15 do
            begin
            b:= d^;
            Inc(d);
//            if  b in [$20..$7E] then
                ADiskName:= ADiskName + AnsiChar(b);
//            else
//                ADiskName:= ADiskName + ' ';
            end;

        d:= PByte(FData.Memory + ADataPos + $16);

        for i:= 0 to 1 do
            begin
            b:= d^;
            Inc(d);
//            if  b in [$20..$7E] then
                ADiskID:= ADiskID + AnsiChar(b);
//            else
//                ADiskID:= ADiskID + ' ';
            end;

        d:= PByte(FData.Memory + ADataPos + $19);

        for i:= 0 to 1 do
            begin
            b:= d^;
            Inc(d);
//            if  b in [$20..$7E] then
                ADOSType:= ADOSType + AnsiChar(b);
//            else
//                ADOSType:= ADOSType + ' ';
            end;
        end;

//dengland This will be called recursively and could chew memory...  Well,
//      comparitively speaking.
    procedure DoProcessFileEntries(const APart: PD64DirPartition;
            const ATrack: TD64TrackNum; const ADepth: Integer);
        var
        e: TD64DirEntries;
        i: Integer;
        p: Cardinal;
        r: PD64DirPartition;

        begin
        DoGetFileEntries(m, ATrack, 3, e);

        for  i:= 0 to High(e) do
            if  IsDirectoryPartition(e[i], p) then
                begin
                APart^.HasChildren:= True;

                New(r);
                r^.Info.Track:= e[i].EntryData[3];
                r^.Info.PartSize:= e[i].FileSize;

                r^.PartFileName:= e[i].FileName;

                DoReadPartDiskDetail(p, r^.PartDiskName, r^.PartDiskID,
                        r^.PartDOSType);

                r^.HasChildren:= False;
                r^.Depth:= ADepth + 1;
                r^.Parent:= l.IndexOf(APart);

                l.Add(r);

                DoProcessFileEntries(r, r^.Info.Track, ADepth + 1);
                end;
        end;

    begin
//  if  FDiskType <> ddt1581 then
//      begin
//      SetLength(AParts, 0);
//      Exit
//      end;

    l:= TList.Create;
    try
        New(r);

        r^.Info.Track:= 40;
        r^.Info.PartSize:= GetDiskBlockCount;
        r^.PartFileName:= '';
        r^.PartDiskName:= FDiskName;
        r^.PartDiskID:= FDiskID;
        r^.PartDOSType:= FDOSType;
        r^.HasChildren:= False;
        r^.Depth:= 0;
        r^.Parent:= -1;

        l.Add(r);

  		if  FDiskType = ddt1581 then
            begin
            m:= TMemoryStream.Create;
            try
                DoProcessFileEntries(r, 40, 0);

                finally
                m.Free;
                end;
            end;

        SetLength(AParts, l.Count);
        for i:= 0 to l.Count - 1 do
            AParts[i]:= PD64DirPartition(l[i])^;

//dengland This will leave a list of dangling pointers but it doesn't matter
        for i:= l.Count - 1 downto 0 do
            Dispose(PD64DirPartition(l[i]));

        finally
        l.Free;
        end;
    end;

procedure TD64Image.SetCurrentPartition(const AInfo: TD64DirPartitionInfo);
    begin
    FCurrPartTrk:= AInfo.Track;
    FCurrPartSiz:= AInfo.PartSize;
    end;

procedure TD64Image.SetCurrentPartition(const AInfo: TD64DirPartitionInfo; out
        APrev: TD64DirPartitionInfo);
    begin
    APrev.Track:= FCurrPartTrk;
    APrev.PartSize:= FCurrPartSiz;

    SetCurrentPartition(AInfo);
    end;

procedure TD64Image.GetPartitionFiles(var AEntries: TD64DirEntries);
    var
    m: TMemoryStream;

    begin
    SetLength(AEntries, 0);
    if  (not FValidVersion)
    or  (FDiskType <> ddt1581) then
        Exit;

    m:= TMemoryStream.Create;
    try
        DoGetFileEntries(m, FCurrPartTrk, 3, AEntries);

        finally
        m.Free;
        end;
    end;

procedure TD64Image.GetPartitionBAM(var ADiskBAM: TD64DiskBAM);
    var
    h,
    i: Integer;
    m: TMemoryStream;

    begin
    SetLength(ADiskBAM, 0);
    if  (not FValidVersion)
    or  (FDiskType <> ddt1581) then
        Exit;

//  h:= FCurrPartSiz div 40;
    h:= 80;

    SetLength(ADiskBAM, h);

    for i:= 0 to h - 1 do
        SetLength(ADiskBAM[i].Bitmap, 5);

    m:= TMemoryStream.Create;
    try
        DoGet1581BAM(m, FCurrPartTrk, h, ADiskBAM)

        finally
        m.Free;
        end;
    end;

function TD64Image.GetSectorsForTrack(const ATrack: TD64TrackNum): Byte;
    begin
    case FDiskType of
        ddt1541:
            Result:= ARR_VAL_D64TRKSECTRS[ATrack];
        ddt1571:
            Result:= ARR_VAL_D71TRKSECTRS[ATrack];
        else
            Result:= 40;
        end;
    end;

end.

