program Install;

{$R 'lang.res' 'lang.rc'}

uses
  Vcl.Forms,
  MainWindow in 'MainWindow.pas' {Form1},
  Lang in 'Lang.pas',
  LangDialog;

{$R *.res}
var
  Lang: TLang;

begin


  Lang:= TForm2.GetLanguage(nil);
  if Lang <> nil then
  begin
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.CreateForm(TForm1, Form1);
    Form1.SetLang(Lang);
    Application.Run;
  end;
end.
