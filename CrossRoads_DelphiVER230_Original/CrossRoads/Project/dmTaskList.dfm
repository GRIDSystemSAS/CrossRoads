object dtmTaskList: TdtmTaskList
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 314
  Width = 392
  object cdsTask: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'indexTaskNum'
        Fields = 'TaskNum'
        Options = [ixPrimary]
      end>
    IndexName = 'indexTaskNum'
    Params = <>
    StoreDefs = True
    Left = 40
    Top = 24
    object cdsTaskTaskNum: TIntegerField
      FieldName = 'TaskNum'
    end
    object cdsTaskProjectNum: TIntegerField
      FieldName = 'ProjectNum'
    end
    object cdsTaskProjectTaskNum: TIntegerField
      FieldName = 'ProjectTaskNum'
    end
    object cdsTaskTaskName: TStringField
      FieldName = 'TaskName'
      Size = 150
    end
    object cdsTaskDescription: TStringField
      FieldName = 'Description'
      Size = 500
    end
    object cdsTaskTaskType: TIntegerField
      FieldName = 'TaskType'
    end
    object cdsTaskTaskSubType: TIntegerField
      FieldName = 'TaskSubType'
    end
    object cdsTaskPriority: TIntegerField
      FieldName = 'Priority'
    end
    object cdsTaskStatus: TIntegerField
      FieldName = 'Status'
    end
    object cdsTaskParentTaskNum: TIntegerField
      FieldName = 'ParentTaskNum'
    end
    object cdsTaskExternalLink: TStringField
      FieldName = 'ExternalLink'
      Size = 200
    end
    object cdsTaskLinkType: TIntegerField
      FieldName = 'LinkType'
    end
    object cdsTaskCreationDate: TDateTimeField
      FieldName = 'CreationDate'
    end
    object cdsTaskModificationDate: TDateTimeField
      FieldName = 'ModificationDate'
    end
    object cdsTaskEstimation: TIntegerField
      FieldName = 'Estimation'
    end
    object cdsTaskDuration: TIntegerField
      FieldName = 'Duration'
    end
    object cdsTaskInProgressDate: TDateTimeField
      FieldName = 'InProgressDate'
    end
    object cdsTaskDoneDate: TDateTimeField
      FieldName = 'DoneDate'
    end
    object cdsTaskAssignTo: TIntegerField
      FieldName = 'AssignTo'
    end
    object cdsTaskDueDate: TDateTimeField
      FieldName = 'DueDate'
    end
    object cdsTaskSprintCode: TStringField
      FieldName = 'SprintCode'
      Size = 10
    end
    object cdsTaskCategory1: TIntegerField
      FieldName = 'Category1'
    end
    object cdsTaskCategory2: TIntegerField
      FieldName = 'Category2'
    end
    object cdsTaskScheduledDate: TDateTimeField
      FieldName = 'ScheduledDate'
    end
    object cdsTaskAutoPostponedDate: TDateTimeField
      FieldName = 'AutoPostponedDate'
    end
    object cdsTaskAutoPostponedTimes: TIntegerField
      FieldName = 'AutoPostponedTimes'
    end
    object cdsTaskSortOrder: TIntegerField
      FieldName = 'SortOrder'
    end
    object cdsTaskTag: TIntegerField
      FieldName = 'Tag'
    end
    object cdsTaskCustomDate: TDateTimeField
      FieldName = 'CustomDate'
    end
  end
  object cdsStats: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'indexStatDate'
        Fields = 'StatDate'
      end>
    IndexName = 'indexStatDate'
    Params = <>
    StoreDefs = True
    Left = 144
    Top = 24
    object cdsStatsStatDate: TDateField
      FieldName = 'StatDate'
    end
    object cdsStatsSprintCode: TStringField
      FieldName = 'SprintCode'
      Size = 10
    end
    object cdsStatsNbScheduled: TIntegerField
      FieldName = 'NbScheduled'
    end
    object cdsStatsNbUnclassified: TIntegerField
      FieldName = 'NbUnclassified'
    end
    object cdsStatsNbDone: TIntegerField
      FieldName = 'NbDone'
    end
    object cdsStatsNbNew: TIntegerField
      FieldName = 'NbNew'
    end
    object cdsStatsNbNewUnclassified: TIntegerField
      FieldName = 'NbNewUnclassified'
    end
    object cdsStatsNbInProgress: TIntegerField
      FieldName = 'NbInProgress'
    end
    object cdsStatsSumScheduled: TIntegerField
      FieldName = 'SumScheduled'
    end
    object cdsStatsSumUnclassified: TIntegerField
      FieldName = 'SumUnclassified'
    end
    object cdsStatsSumDone: TIntegerField
      FieldName = 'SumDone'
    end
    object cdsStatsSumNew: TIntegerField
      FieldName = 'SumNew'
    end
    object cdsStatsSumNewUnclassified: TIntegerField
      FieldName = 'SumNewUnclassified'
    end
    object cdsStatsSumInProgress: TIntegerField
      FieldName = 'SumInProgress'
    end
  end
end
