object dtmUserLog: TdtmUserLog
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 254
  Width = 341
  object cdsUserLog: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'IndexUserLog'
        Fields = 'LogID'
        Options = [ixPrimary, ixDescending]
      end>
    IndexName = 'IndexUserLog'
    Params = <>
    StoreDefs = True
    Left = 48
    Top = 24
    object cdsUserLogLogID: TStringField
      FieldName = 'LogID'
    end
    object cdsUserLogDescription: TStringField
      FieldName = 'Description'
      Size = 500
    end
    object cdsUserLogLogType: TIntegerField
      FieldName = 'LogType'
    end
    object cdsUserLogCreationDate: TDateTimeField
      FieldName = 'CreationDate'
    end
    object cdsUserLogModificationDate: TDateTimeField
      FieldName = 'ModificationDate'
    end
  end
end
