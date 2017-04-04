program mwx1;

uses
  Forms,
  mwx1Main in 'mwx1Main.pas' {MainForm},
  mwx1Con in 'mwx1Con.pas' {ConnectionForm},
  simpleSock in '..\..\simpleSock.pas',
  jsonDoc in '..\..\jsonDoc.pas',
  bsonTools in '..\..\bsonTools.pas',
  mongoWire in '..\..\mongoWire.pas',
  mwx1Item in 'mwx1Item.pas' {ItemForm},
  mongoID in '..\..\mongoID.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TConnectionForm, ConnectionForm);
  Application.CreateForm(TItemForm, ItemForm);
  Application.Run;
end.
