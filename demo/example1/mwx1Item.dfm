object ItemForm: TItemForm
  Left = 270
  Top = 163
  Width = 369
  Height = 288
  Caption = 'Item Properties'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    353
    249)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 27
    Height = 13
    Caption = 'Name'
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 39
    Height = 13
    Caption = 'Address'
  end
  object Label3: TLabel
    Left = 8
    Top = 168
    Width = 30
    Height = 13
    Anchors = [akBottom]
    Caption = 'Phone'
  end
  object txtName: TEdit
    Left = 8
    Top = 24
    Width = 337
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object txtAddress: TMemo
    Left = 8
    Top = 64
    Width = 337
    Height = 97
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object txtPhone: TEdit
    Left = 8
    Top = 184
    Width = 337
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 2
  end
  object Button2: TButton
    Left = 272
    Top = 216
    Width = 74
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object Button1: TButton
    Left = 192
    Top = 216
    Width = 74
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
end
