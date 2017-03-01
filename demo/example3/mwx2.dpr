program mwx2;

uses
  Forms,
  mwx2Main in 'mwx2Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
