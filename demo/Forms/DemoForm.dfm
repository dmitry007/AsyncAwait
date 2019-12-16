object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'AsyncAwait Demo'
  ClientHeight = 126
  ClientWidth = 314
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblResult: TLabel
    Left = 16
    Top = 39
    Width = 87
    Height = 13
    Caption = 'Fibonacci number:'
  end
  object btnDemo: TButton
    Left = 8
    Top = 72
    Width = 297
    Height = 25
    Caption = 'Run demo task'
    TabOrder = 0
    OnClick = btnDemoClick
  end
  object btnFibonacci: TButton
    Left = 8
    Top = 8
    Width = 297
    Height = 25
    Caption = 'Calculate random Fibonacci number'
    TabOrder = 1
    OnClick = btnFibonacciClick
  end
end
