object Form1: TForm1
  Left = 192
  Top = 127
  Caption = 'TMongoWire Example 3 - MongoWireAuthenticate'
  ClientHeight = 268
  ClientWidth = 774
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    774
    268)
  PixelsPerInch = 96
  TextHeight = 13
  object btnAdd: TButton
    Left = 685
    Top = 40
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Add...'
    TabOrder = 0
    OnClick = btnAddClick
  end
  object btnGet: TButton
    Left = 685
    Top = 72
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Get...'
    TabOrder = 1
    OnClick = btnGetClick
  end
  object btnDelete: TButton
    Left = 685
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Delete'
    TabOrder = 2
    OnClick = btnDeleteClick
  end
  object ListView1: TListView
    Left = 8
    Top = 40
    Width = 670
    Height = 213
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Name'
        Width = -1
        WidthType = (
          -1)
      end
      item
        Caption = 'Type'
        Width = -1
        WidthType = (
          -1)
      end
      item
        Caption = 'ID'
        Width = -1
        WidthType = (
          -1)
      end
      item
        Caption = 'Create'
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    SortType = stText
    TabOrder = 3
    ViewStyle = vsReport
  end
  object OpenDialog1: TOpenDialog
    Filter = 'All files (*.*)|*.*'
    InitialDir = '.'
    Left = 8
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    Filter = 'All files (*.*)|*.*'
    InitialDir = '.'
    Left = 40
    Top = 8
  end
end
