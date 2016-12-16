object dtmLSNParameters: TdtmLSNParameters
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 377
  Width = 484
  object cdsParameters: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'IdxParamName'
        Fields = 'ParamName'
        Options = [ixPrimary, ixUnique, ixCaseInsensitive]
      end>
    IndexName = 'IdxParamName'
    Params = <>
    StoreDefs = True
    Left = 32
    Top = 16
    object cdsParametersParamName: TStringField
      FieldName = 'ParamName'
      Size = 50
    end
    object cdsParametersValueText: TStringField
      FieldName = 'ValueText'
      Size = 150
    end
    object cdsParametersValueInteger: TIntegerField
      FieldName = 'ValueInteger'
    end
    object cdsParametersValueFloat: TFloatField
      FieldName = 'ValueFloat'
    end
    object cdsParametersValueBoolean: TBooleanField
      FieldName = 'ValueBoolean'
    end
    object cdsParametersValueDatetime: TDateTimeField
      FieldName = 'ValueDatetime'
    end
  end
end
