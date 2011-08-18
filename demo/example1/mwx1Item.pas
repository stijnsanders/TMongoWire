unit mwx1Item;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TItemForm = class(TForm)
    Label1: TLabel;
    txtName: TEdit;
    Label2: TLabel;
    txtAddress: TMemo;
    Label3: TLabel;
    txtPhone: TEdit;
    Button2: TButton;
    Button1: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ItemForm: TItemForm;

implementation

{$R *.dfm}

end.
