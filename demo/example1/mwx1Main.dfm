object MainForm: TMainForm
  Left = 192
  Top = 112
  Width = 369
  Height = 360
  Caption = 'TMongoWire Example 1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    353
    321)
  PixelsPerInch = 96
  TextHeight = 13
  object lblCount: TLabel
    Left = 8
    Top = 272
    Width = 12
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '...'
  end
  object btnNew: TButton
    Left = 8
    Top = 288
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'New...'
    TabOrder = 0
    OnClick = btnNewClick
  end
  object btnEdit: TButton
    Left = 96
    Top = 288
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Edit...'
    TabOrder = 1
    OnClick = btnEditClick
  end
  object btnDelete: TButton
    Left = 184
    Top = 288
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Delete'
    TabOrder = 2
    OnClick = btnDeleteClick
  end
  object ListView1: TListView
    Left = 8
    Top = 8
    Width = 337
    Height = 261
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Name'
        Width = -1
        WidthType = (
          -1)
      end
      item
        Caption = 'Address'
        Width = -1
        WidthType = (
          -1)
      end
      item
        Caption = 'Phone'
        Width = -1
        WidthType = (
          -1)
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 4
    ViewStyle = vsReport
  end
  object btnRefresh: TButton
    Left = 272
    Top = 288
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Refresh'
    TabOrder = 3
    OnClick = btnRefreshClick
  end
end
