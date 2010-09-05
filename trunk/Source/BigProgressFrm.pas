{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2009 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Sep 05, 2009.
http://www.nldelphi.com/forum/showthread.php?p=281131#post281131
*******************************************************************************
 TBigProgressForm is an implementation of IBigThreadProgressMonitor that will
 show a form with a progress bar.
******************************************************************************}
unit BigProgressFrm;

interface

uses
  Graphics, Forms, Controls, StdCtrls, Classes, ComCtrls, BigThreadProgress;

type
  TBigProgressForm = class(TForm, IBigThreadProgressMonitor)
    ProgressBar: TProgressBar;
    InfoLabel: TLabel;
    TitleLabel: TLabel;
  private
    // New methods for decendants to override. Show and Hide are not virtual.
    procedure IBigThreadProgressMonitor.Show = ShowForm;
    procedure IBigThreadProgressMonitor.Hide = HideForm;
    
    procedure ShowForm; virtual;
    procedure SetMax(AMax: Integer); virtual;
    procedure SetPosition(APosition: Integer); virtual;
    procedure StepBy(ADelta: Integer); virtual;
    procedure SetInfoText(AText: string); virtual;
    procedure HideForm; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

{ TBigProgressForm }

constructor TBigProgressForm.Create(AOwner: TComponent);
begin
  inherited;
  // Make the title label font bold
  TitleLabel.Font.Style := TitleLabel.Font.Style + [fsBold];
  // Clear all captions
  Caption := '';
  TitleLabel.Caption := '';
  InfoLabel.Caption := '';
end;

procedure TBigProgressForm.HideForm;
begin
  Hide;
  // Free the form when it is hidden
  Release;
end;

procedure TBigProgressForm.SetInfoText(AText: string);
begin
  InfoLabel.Caption := AText;
end;

procedure TBigProgressForm.SetMax(AMax: Integer);
begin
  ProgressBar.Max := AMax;
end;

procedure TBigProgressForm.SetPosition(APosition: Integer);
begin
  ProgressBar.Position := APosition;
end;

procedure TBigProgressForm.ShowForm;
begin
  Show;
end;

procedure TBigProgressForm.StepBy(ADelta: Integer);
begin
  ProgressBar.StepBy(ADelta);
end;

end.
