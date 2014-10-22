program mwx2;

uses
  Forms,
  mwx2Main in 'mwx2Main.pas' {Form1},
  simpleSock in '..\..\simpleSock.pas',
  bsonDoc in '..\..\bsonDoc.pas',
  mongoID in '..\..\mongoID.pas',
  mongoStream in '..\..\mongoStream.pas',
  mongoWire in '..\..\mongoWire.pas',
  mongoAuth in '..\..\mongoAuth.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
