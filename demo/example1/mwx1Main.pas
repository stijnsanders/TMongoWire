unit mwx1Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, mongoWire, ComCtrls, jsonDoc;

type
  TMainForm = class(TForm)
    btnNew: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    ListView1: TListView;
    lblCount: TLabel;
    btnRefresh: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    { Private declarations }
    FMongoWire:TMongoWire;
    procedure LoadItems;
    procedure LoadItem(li:TListItem;const d:IJSONDocument);
    procedure UpdateCount;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  mwx1Con, mwx1Item, mongoID;

{$R *.dfm}

const
  mwx1NameSpace='mwx1';
  mwx1Collection='items';

  //see ListView1.Items[].SubItems below, keep according to ListView1.Columns
  siiAddress=0;
  siiPhone=1;
  siiID=2;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FMongoWire:=TMongoWire.Create(mwx1NameSpace);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FMongoWire.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  try
    if ConnectionForm.ShowModal=mrOk then
      FMongoWire.Open(ConnectionForm.txtHost.Text,StrToInt(ConnectionForm.txtPort.Text))
    else

      raise Exception.Create('User abort');

    LoadItems;
  except
    on e:Exception do
     begin
      MessageDlg('Connection failed: '+e.Message,mtError,[mbOK],0);
      Close;
     end;
  end;
end;

procedure TMainForm.LoadItems;
var
  q:TMongoWireQuery;
  d:IJSONDocument;
begin
  ListView1.Items.BeginUpdate;
  try
    ListView1.Items.Clear;
    d:=JSON;
    q:=TMongoWireQuery.Create(FMongoWire);
    try
      q.Query(mwx1Collection,nil);
      while q.Next(d) do LoadItem(ListView1.Items.Add,d);
    finally
      q.Free;
    end;
    UpdateCount;
  finally
    ListView1.Items.EndUpdate;
  end;
end;

procedure TMainForm.UpdateCount;
begin
  lblCount.Caption:=IntToStr(ListView1.Items.Count)+' item(s)';
end;

procedure TMainForm.LoadItem(li:TListItem;const d:IJSONDocument);
begin
  li.Caption:=VarToStr(d['name']);
  li.SubItems.Clear;
  li.SubItems.Add(StringReplace(VarToStr(d['address']),#13#10,' ',[rfReplaceAll]));
  li.SubItems.Add(VarToStr(d['phone']));
  li.SubItems.Add(VarToStr(d['id']));
end;

procedure TMainForm.btnRefreshClick(Sender: TObject);
begin
  LoadItems;
end;

procedure TMainForm.btnNewClick(Sender: TObject);
var
  d:IJSONDocument;
begin
  ItemForm.txtName.Text:='';
  ItemForm.txtAddress.Text:='';
  ItemForm.txtPhone.Text:='';
  if ItemForm.ShowModal=mrOk then
   begin
    d:=JSON([
      'id',mongoObjectID,
      'name',ItemForm.txtName.Text,
      'address',ItemForm.txtAddress.Text,
      'phone',ItemForm.txtPhone.Text
    ]);
    FMongoWire.Insert(mwx1Collection,d);
    //LoadItems;?
    LoadItem(ListView1.Items.Add,d);
    UpdateCount;
   end;
end;

procedure TMainForm.btnEditClick(Sender: TObject);
var
  dSelector,d:IJSONDocument;
begin
  if ListView1.Selected=nil then raise Exception.Create('Please select an item to edit');
  {
  ItemForm.txtName.Text:=ListView1.Selected.Caption;
  ItemForm.txtAddress.Text:=ListView1.Selected.SubItems[siiAddress];
  ItemForm.txtPhone.Text:=ListView1.Selected.SubItems[siiPhone];
  }
  dSelector:=JSON(['id',ListView1.Selected.SubItems[siiID]]);
  d:=FMongoWire.Get(mwx1Collection,dSelector);
  ItemForm.txtName.Text:=VarToStr(d['name']);
  ItemForm.txtAddress.Text:=VarToStr(d['address']);
  ItemForm.txtPhone.Text:=VarToStr(d['phone']);
  if ItemForm.ShowModal=mrOk then
   begin
    d:=JSON([
      'id',ListView1.Selected.SubItems[siiID],
      'name',ItemForm.txtName.Text,
      'address',ItemForm.txtAddress.Text,
      'phone',ItemForm.txtPhone.Text
    ]);
    FMongoWire.Update(mwx1Collection,dSelector,d);
    //LoadItems;?
    LoadItem(ListView1.Selected,d);
    UpdateCount;
   end;
end;

procedure TMainForm.btnDeleteClick(Sender: TObject);
begin
  if ListView1.Selected=nil then raise Exception.Create('Please select an item to delete');
  if MessageDlg('Are you sure to delete this item?'#13#10'"'+ListView1.Selected.Caption+'"',mtConfirmation,[mbOK,mbCancel],0)=mrOK then
   begin
    FMongoWire.Delete(mwx1Collection,JSON(['id',ListView1.Selected.SubItems[siiID]]));
    //LoadItems;?
    ListView1.Selected.Delete;
   end;
end;

end.
