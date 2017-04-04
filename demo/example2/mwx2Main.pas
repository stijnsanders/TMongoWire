unit mwx2Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, mongoWire, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    btnAdd: TButton;
    btnGet: TButton;
    btnDelete: TButton;
    ListView1: TListView;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnGetClick(Sender: TObject);
  private
    FDB:TMongoWire;
  end;

var
  Form1: TForm1;

implementation

uses
  jsonDoc, mongoStream, mongoID;

{$R *.dfm}

const
  FilesNameSpace='test';
  FilesCollection='test2';//TODO: from ParamStr? from ini? registry?

  siType=0;
  siID=1;

procedure TForm1.FormShow(Sender: TObject);
var
  d:IJSONDocument;
  q:TMongoWireQuery;
  li:TListItem;
begin
  FDB:=TMongoWire.Create(FilesNameSpace);
  FDB.Open;//TODO: from ParamStr? from ini? registry?

  q:=TMongoWireQuery.Create(FDB);
  q.Query(FilesCollection+mongoStreamFilesSuffix,nil);

  ListView1.Items.BeginUpdate;
  try
    d:=JSON;
    while q.Next(d) do
     begin
      li:=ListView1.Items.Add;
      li.Caption:=VarToStr(d['name']);
      li.SubItems.Add(VarToStr(d['contentType']));//siType
      li.SubItems.Add(VarToStr(d['_id']));//siID
     end;
  finally
    ListView1.Items.EndUpdate;
  end;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FDB.Free;
end;

procedure TForm1.btnAddClick(Sender: TObject);
var
  id:Variant;
  f:TFileStream;
  fn,contentType:string;
  li:TListItem;
begin
  if OpenDialog1.Execute then
   begin
    fn:=OpenDialog1.FileName;
    contentType:=ExtractFileExt(fn);
    //TODO: get 'Content-Type' from HKEY_CLASSES_ROOT
    f:=TFileStream.Create(fn,fmOpenRead or fmShareDenyWrite);
    try
      fn:=ExtractFileName(fn);
      id:=TMongoStream.Add(FDB,FilesCollection,f,JSON([
        'name',fn,
        'contentType',contentType,
        'created',VarFromDateTime(Now)
        //more?
      ]));
      li:=ListView1.Items.Add;
      li.Caption:=fn;
      li.SubItems.Add(contentType);//siType
      li.SubItems.Add(VarToStr(id));//siID
    finally
      f.Free;
    end;
   end;
end;

procedure TForm1.btnGetClick(Sender: TObject);
var
  li:TListItem;
  m:TMongoStream;
begin
  li:=ListView1.Selected;
  if li<>nil then
   begin
    SaveDialog1.FileName:=li.Caption;
    if SaveDialog1.Execute then
     begin
      m:=TMongoStream.Create(FDB,FilesCollection,li.SubItems[siID]);
      try
        m.SaveToFile(SaveDialog1.FileName);
      finally
        m.Free;
      end;
     end;
   end;
end;

procedure TForm1.btnDeleteClick(Sender: TObject);
var
  li:TListItem;
begin
  li:=ListView1.Selected;
  if li<>nil then
    if MessageBox(Handle,PChar('Are you sure to delete "'+li.Caption+'"?'),
      'mwx2',MB_OKCANCEL or MB_ICONQUESTION)=idOK then
     begin
      FDB.Delete(FilesCollection+mongoStreamFilesSuffix,
        JSON([mongoStreamIDField,li.SubItems[siID]]));
      FDB.Delete(FilesCollection+mongoStreamChunksSuffix,
        JSON([mongoStreamFilesIDField,li.SubItems[siID]]));
      li.Delete;
     end;
end;

end.
