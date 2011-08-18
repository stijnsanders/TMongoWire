unit mwx1Con;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TConnectionForm = class(TForm)
    Label1: TLabel;
    txtHost: TEdit;
    Button1: TButton;
    Button2: TButton;
    Label2: TLabel;
    txtPort: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ConnectionForm: TConnectionForm;

implementation

{$R *.dfm}

end.
