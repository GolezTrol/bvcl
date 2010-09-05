{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2009 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Okt 22, 2009
http://www.nldelphi.com/forum/showthread.php?p=283425#post283425
*******************************************************************************
 TBigImageList is a TImageList descendant that introduces the DisabledImages
 property. You can link a second imagelist with greyed out images to this
 property to draw nicer disabled images in controls that don't support their
 own DisabledImages property, like TMainMenu and TActionList. 
******************************************************************************}
unit BigImagelist;

interface

uses
  Classes, Controls, ImgList, Graphics;

type
  TBigCustomImageList = class(TImageList)
  private
    FDisabledImagesChangeLink: TChangeLink;
    FDisabledImages: TCustomImageList;
    procedure SetDisabledImages(const Value: TCustomImageList);
  protected
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure DisabledImagesChange(Sender: TObject);

    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer;
      Style: Cardinal; Enabled: Boolean = True); override;

    property DisabledImages: TCustomImageList read FDisabledImages write SetDisabledImages;
  end;

  TBigImageList = class(TBigCustomImageList)
  published
    property DisabledImages;
  end;

implementation

{ TBigCustomImageList }

constructor TBigCustomImageList.Create(AOwner: TComponent);
begin
  inherited;

  FDisabledImagesChangeLink := TChangeLink.Create;
  FDisabledImagesChangeLink.OnChange := DisabledImagesChange;
end;

destructor TBigCustomImageList.Destroy;
begin
  FDisabledImagesChangeLink.Free;

  inherited;
end;

procedure TBigCustomImageList.DisabledImagesChange(Sender: TObject);
begin
  if Sender = DisabledImages then
    Change;
end;

procedure TBigCustomImageList.DoDraw(Index: Integer; Canvas: TCanvas;
  X, Y: Integer; Style: Cardinal; Enabled: Boolean);
begin
  if (not Enabled) and Assigned(FDisabledImages) then
    TBigCustomImageList(FDisabledImages).DoDraw(Index, Canvas, X, Y, Style, True)
  else
    inherited;
end;

procedure TBigCustomImageList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = DisabledImages then
      DisabledImages := nil;
end;

procedure TBigCustomImageList.SetDisabledImages(
  const Value: TCustomImageList);
begin
  if DisabledImages <> nil then DisabledImages.UnRegisterChanges(FDisabledImagesChangeLink);
  FDisabledImages := Value;
  if DisabledImages <> nil then
  begin
    DisabledImages.RegisterChanges(FDisabledImagesChangeLink);
    DisabledImages.FreeNotification(Self);
  end;
end;

end.
