object dtmTypeList: TdtmTypeList
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 396
  Width = 473
  object cdsProjectStatus: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'IndexType'
        Fields = 'Type'
        Options = [ixPrimary]
      end>
    IndexName = 'IndexType'
    Params = <>
    StoreDefs = True
    Left = 72
    Top = 112
    object cdsProjectStatusType: TIntegerField
      FieldName = 'Type'
    end
    object cdsProjectStatusLabel: TStringField
      FieldName = 'Label'
      Size = 30
    end
  end
  object cdsSprint: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'indexSprintCode'
        Fields = 'SprintCode'
      end>
    IndexName = 'indexSprintCode'
    Params = <>
    StoreDefs = True
    Left = 64
    Top = 24
    object cdsSprintSprintCode: TStringField
      FieldName = 'SprintCode'
      Size = 10
    end
    object cdsSprintYear: TIntegerField
      FieldName = 'Year'
    end
    object cdsSprintWeekNum: TIntegerField
      FieldName = 'WeekNum'
    end
    object cdsSprintBeginDate: TDateTimeField
      FieldName = 'BeginDate'
    end
    object cdsSprintEndDate: TDateTimeField
      FieldName = 'EndDate'
    end
    object cdsSprintCapacity: TIntegerField
      FieldName = 'Capacity'
    end
    object cdsSprintPlanned: TIntegerField
      FieldName = 'Planned'
    end
    object cdsSprintConsumed: TIntegerField
      FieldName = 'Consumed'
    end
  end
  object cdsPriority: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'IndexType'
        Fields = 'Type'
        Options = [ixPrimary]
      end>
    IndexName = 'IndexType'
    Params = <>
    StoreDefs = True
    Left = 184
    Top = 112
    object cdsPriorityType: TIntegerField
      FieldName = 'Type'
    end
    object cdsPriorityLabel: TStringField
      FieldName = 'Label'
      Size = 30
    end
  end
end
