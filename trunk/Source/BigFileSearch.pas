{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Nov 25, 2008 and is based on NLDFileSearch .
NLDFileSearch was written by me as well and was released on the NLDelphi forum
on Feb 1, 2003. http://www.nldelphi.com/Forum/showthread.php?t=5908
*******************************************************************************
 TBigFileSearch is a wrapper around FindFirst and FindNext procedures.
 It allows searching sub directories and supports various options to process
 the result.
 Use the BigFindFiles procedure to return an entire file/folder list into a
 stringlist.
*******************************************************************************
Needs to be rewritten.
Work in progress...

******************************************************************************}
unit BigFileSearch;

interface

uses
  SysUtils, Mask, Classes;

const
  faNormal     = $00000080;
  faTemporary  = $00000100;
  faCompressed = $00000800;
  faOffline    = $00001000;

  faAnyType    = $0000FFFF; // Warning: Redefine of SysUtils.faAnyFile
  faSpecial    = faHidden or faSysFile or faVolumeID or faDirectory platform;
  faFilesOnly  = faAnyType and not (faSpecial or faVolumeID or faSymLink) platform;


type
  TSearchOption = (soRecursive, soIncludeFolders,
    soReturnFullPath, soReturnRelativePath);
  TSearchOptions = set of TSearchOption;

procedure BigFindFiles(AResult: TStrings; AFileMask: string; AOptions: TSearchOptions = []);

implementation

uses
  Masks;

procedure BigFindFiles(AResult: TStrings; AFileMask: string; AOptions: TSearchOptions = []);
var
  FileMask: string;
  RootFolder: string;
  Mask: TMask;

  procedure SearchFolder(AFolder: string);
  var
    Folders: TStringList;
    Files: TStringList;
  begin
    Folders := TStringList.Create;
    Files := TStringList.Create;
    try
    finally
      Folders.Free;
      Files.Free;
    end;
  end;

begin
end;

end.
