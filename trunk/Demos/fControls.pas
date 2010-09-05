unit fControls;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BigNumEdit, StdCtrls, BigEdit, BigDialogs, BigLabel, ComObj,
  BigSubtitleLabel, BigButton, Buttons, BigClipboardMonitor, BigClipbrd,
  BigMiniGames, BigMineSweeper, ExtCtrls, BigPool, BigClasses, Menus,
  BigMRU, BigMRUMenuController, BigMRURegistryStorage;

type
  TForm1 = class(TForm)
    Bevel1: TBevel;
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    Button2: TButton;
    BigMRU1: TBigMRU;
    MainMenu1: TMainMenu;
    ergerg1: TMenuItem;
    Button3: TButton;
    BigMRUMenuController1: TBigMRUMenuController;
    BigMRURegistryStorage1: TBigMRURegistryStorage;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BigMenuMRUController1GetCaption(Sender: TObject;
      const AFile: String; var ACaption: String);
    procedure BigMenuMRUController1Execute(Sender: TObject;
      const AFile: String);
    procedure Button3Click(Sender: TObject);
  private
    FS: TSemaphore;
    a, b: TBitmap;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}



procedure TForm1.FormCreate(Sender: TObject);
begin
  Exit;

  a := TBitmap.Create;
  b := TBitmap.Create;
  a.LoadFromFile('c:\ff\ship.bmp');
  b.LoadFromFile('c:\ff\bg.bmp');
  {with TBigMineSweeper.Create(Self) do
  begin
    Parent := Self;
    Align := alClient;
  end;
  DoubleBuffered := True;}
  FS := TSemaphore.Create(2);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Fs.Release;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  a.Free;
  b.Free;

end;

procedure TForm1.BigMenuMRUController1GetCaption(Sender: TObject;
  const AFile: String; var ACaption: String);
begin
  ACaption := ChangeFileExt(ExtractFileName(AFile), '.');
end;

procedure TForm1.BigMenuMRUController1Execute(Sender: TObject;
  const AFile: String);
begin
   ShowMessage(AFile);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    BigMRU1.Add(OpenDialog1.FileName);
end;

end.
