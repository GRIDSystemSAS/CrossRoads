unit unStructure;

interface

type
  TProjectRecord = record
    ProjectNum: integer;
    ProjectName: string;
    Description: string;
    CreationDate: TDatetime;
    ModificationDate: TDatetime;
    ColorID: cardinal;
    Status: integer;
    LastTaskNum: integer;
    SortOrder: integer;
    Tag: integer;
    CustomDate: TDatetime;
    FirstSprint: string;
    LastSprint: string;
  end;

  TTaskRecord = record
    TaskNum: Integer;
    ProjectNum: Integer;
    ProjectTaskNum: Integer;
    TaskName: String;
    Description: String;
    TaskType: Integer;
    TaskSubType: Integer;
    Priority: Integer;
    Status: Integer;
    ParentTaskNum: Integer;
    ExternalLink: String;
    LinkType: Integer;
    CreationDate: TDateTime;
    ModificationDate: TDateTime;
    Estimation: Integer;
    Duration: Integer;
    InProgressDate: TDateTime;
    DoneDate: TDateTime;
    AssignTo: Integer;
    SprintCode: String;
    DueDate: TDatetime;
    Category1: Integer;
    Category2: Integer;
    ScheduledDate: TDatetime;
    AutoPostponedDate: TDatetime;
    AutoPostponedTimes: integer;
    SortOrder: integer;
    Tag: integer;
    CustomDate: TDatetime;
  end;

  TSprintRecord = record
    SprintCode: String;
    Year: Integer;
    WeekNum: Integer;
    BeginDate: TDateTime;
    EndDate: TDateTime;
    Capacity: Integer;
    Planned: Integer;
    Consumed: Integer;
  end;

  TUserLogRecord = record
    LogID: String;
    Description: String;
    LogType: Integer;
    CreationDate: TDateTime;
    ModificationDate: TDateTime;
  end;

  TStatsRecord = record
    StatDate: TDatetime;
    SprintCode: string;
    NbScheduled: Integer;
    NbUnclassified: Integer;
    NbDone: Integer;
    NbNew: Integer;
    NbNewUnclassified: Integer;
    NbInProgress: Integer;
    SumScheduled: Integer;
    SumUnclassified: Integer;
    SumDone: Integer;
    SumNew: Integer;
    SumNewUnclassified: Integer;
    SumInProgress: Integer;
  end;

implementation




end.
