program Install;

{$R 'lang.res' 'lang.rc'}

uses
  Vcl.Forms,
  MainWindow in 'MainWindow.pas' {Form1},
  Lang in 'Lang.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
