program mwx1;

uses
  Forms,
  mwx1Main in 'mwx1Main.pas' {MainForm},
  mwx1Con in 'mwx1Con.pas' {ConnectionForm},
  mongoWire in '..\..\mongoWire.pas',
  bsonDoc in '..\..\bsonDoc.pas',
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
