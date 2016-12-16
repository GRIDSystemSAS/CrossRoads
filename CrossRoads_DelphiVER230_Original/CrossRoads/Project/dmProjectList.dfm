object dtmProjectList: TdtmProjectList
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 330
  Width = 396
  object cdsProject: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'indexProject'
        Fields = 'ProjectNum'
        Options = [ixPrimary, ixUnique]
      end>
    IndexName = 'indexProject'
    Params = <>
    StoreDefs = True
    Left = 32
    Top = 24
    object cdsProjectProjectNum: TIntegerField
      FieldName = 'ProjectNum'
    end
    object cdsProjectProjectName: TStringField
      FieldName = 'ProjectName'
      Size = 30
    end
    object cdsProjectDescription: TStringField
      FieldName = 'Description'
      Size = 255
    end
    object cdsProjectCreationDate: TDateTimeField
      FieldName = 'CreationDate'
    end
    object cdsProjectModificationDate: TDateTimeField
      FieldName = 'ModificationDate'
    end
    object cdsProjectColorID: TIntegerField
      FieldName = 'ColorID'
    end
    object cdsProjectStatus: TIntegerField
      FieldName = 'Status'
    end
    object cdsProjectLastTaskNum: TIntegerField
      FieldName = 'LastTaskNum'
    end
    object cdsProjectSortOrder: TIntegerField
      FieldName = 'SortOrder'
    end
    object cdsProjectTag: TIntegerField
      FieldName = 'Tag'
    end
    object cdsProjectCustomDate: TDateTimeField
      FieldName = 'CustomDate'
    end
    object cdsProjectFirstSprint: TStringField
      FieldName = 'FirstSprint'
      Size = 10
    end
    object cdsProjectLastSprint: TStringField
      FieldName = 'LastSprint'
      Size = 10
    end
  end
end
