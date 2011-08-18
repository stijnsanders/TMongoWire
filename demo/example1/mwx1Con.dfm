object ConnectionForm: TConnectionForm
  Left = 278
  Top = 229
  BorderStyle = bsDialog
  Caption = 'Mongo Wire Connection'
  ClientHeight = 130
  ClientWidth = 171
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    171
    130)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 22
    Height = 13
    Caption = 'Host'
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 20
    Height = 13
    Caption = 'Port'
  end
  object txtHost: TEdit
    Left = 8
    Top = 24
    Width = 153
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'localhost'
  end
  object Button1: TButton
    Left = 8
    Top = 96
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object Button2: TButton
    Left = 88
    Top = 96
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object txtPort: TEdit
    Left = 8
    Top = 64
    Width = 153
    Height = 21
    TabOrder = 1
    Text = '27017'
  end
end
