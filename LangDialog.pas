unit LangDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.pngimage, Lang, System.Generics.Collections;

type
  TForm2 = class(TForm)
    ComboBox1: TComboBox;
    Label1: TLabel;
    Button1: TButton;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FLang: TLang;
    dict: TDictionary<string, string>;
    { Private declarations }
  public
    class function GetLanguage(AOwner: TComponent): TLang;
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

class function TForm2.GetLanguage(AOwner: TComponent): TLang;
var
  d: TForm2;
begin
  d:= TForm2.Create(AOwner);
  try
    d.ShowModal;
    Result:= d.FLang;
  finally
    d.Free;
  end;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  FLang.SelectedLanguage:= dict[ComboBox1.Text];
  Self.ModalResult:= IDOK;
  Self.CloseModal;
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  l: String;
begin
  dict:= TDictionary<string, string>.Create;
  FLang:= TLang.Create;
  for l in FLang.AvailableLanguages do
  begin
    ComboBox1.Items.Add(FLang.GetFullNameLanguage(l));
    dict.Add(FLang.GetFullNameLanguage(l), l);
  end;
  if ComboBox1.Items.Count > 0 then
  begin
    ComboBox1.ItemIndex:= 0;
  end;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  dict.Free;
end;

end.
