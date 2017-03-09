module ProjectTimelineGeneratorEngine

open System
open System.Globalization
open System.Net.Http
open System.Net.Http.Headers
open System.Xml
open Microsoft.TeamFoundation
open Microsoft.TeamFoundation.Common
open Microsoft.TeamFoundation.Server
open Microsoft.TeamFoundation.WorkItemTracking.Client
open Microsoft.TeamFoundation.ProjectManagement
open System.Collections
open System.Collections.Generic
open System.Web
open System.Threading
open System.Runtime.Serialization
open System.Runtime.Serialization.Json
open System.Text
open System.IO

open Common
open Domain

let tfsAddr = "http://tfs.aiscorp.com:8080/tfs"
let tfsUri = new Uri(tfsAddr)

// a series of classes used to hold results from TFS RESTful API queries
[<DataContract>] 
type queryData = 
    { 
        [<DataMember>] mutable query : string 
    }
[<DataContract>] 
type workItemResults = 
    { 
        [<DataMember>] mutable id : string; 
        [<DataMember>] mutable url : string 
    }
[<DataContract>] 
type queryAttributes = 
    { 
        [<DataMember>] mutable referenceName : string; 
        [<DataMember>] mutable name : string; 
        [<DataMember>] mutable url : string 
    }

[<DataContract>]
type queryResults =
    {
        [<DataMember>] mutable queryType : string ;
        [<DataMember>] mutable queryResultType : string;
        [<DataMember>] mutable asOf : string;
        [<DataMember>] mutable columns : ResizeArray<queryAttributes>;
        [<DataMember>] mutable workItems : ResizeArray<workItemResults>;
    }

[<DataContract>] 
type startEndDates =
    {
        [<DataMember>] mutable startDate : string; 
        [<DataMember>] mutable finishDate : string; 
    }

[<DataContract>] 
type iterationData = 
    { 
        [<DataMember>] mutable id : string; 
        [<DataMember>] mutable name : string; 
        [<DataMember>] mutable path : string; 
        [<DataMember>] mutable attributes : startEndDates; 
        [<DataMember>] mutable url : string; 
    }

[<DataContract>]
type iterationResults = 
    {
        [<DataMember>] mutable count : int;
        [<DataMember>] mutable value : ResizeArray<iterationData>
    }

[<DataContract>]
type teamMember = 
    {
        [<DataMember>] mutable id : string;
        [<DataMember>] mutable displayName : string;
        [<DataMember>] mutable uniqueName : string;
        [<DataMember>] mutable url : string;
        [<DataMember>] mutable imageUrl : string;
    }

[<DataContract>]
type activity = 
    {
        [<DataMember>] mutable capacityPerDay : int;
        [<DataMember>] mutable name : string;
    }

[<DataContract>]
type dayOff = 
    {
        [<DataMember>] mutable start : string;
        [<DataMember>] mutable ``end`` : string;
    }

[<DataContract>]
type capacityData = 
    {
        [<DataMember>] mutable teamMember : teamMember;
        [<DataMember>] mutable activities : ResizeArray<activity>;
        [<DataMember>] mutable daysOff : ResizeArray<dayOff>;
        [<DataMember>] mutable url : string;
    }

[<DataContract>]
type capacityResults = 
    {
        [<DataMember>] mutable count : int;
        [<DataMember>] mutable value : ResizeArray<capacityData>
    }

// a .NET class used for exposing functionality to our C# client (see public memeber at the end)
type Class1() = 
    
    // represents our default TFS team project collection
    let tpc = Client.TfsTeamProjectCollectionFactory.GetTeamProjectCollection(new Uri(tfsAddr + "/DefaultCollection"));

    // represents the TFS workItem repository we will be querying against
    // we instantiate with BypassRule flag so that we can modify data (like dates) to suit our purposes
    let workItemStore = new WorkItemStore(tpc, WorkItemStoreFlags.BypassRules)

    // a service needed for fetching key project info (like iterations and developer capacities)
    let commonStructureService = tpc.GetService<ICommonStructureService4>()

    // fetches and returns a sequence of iterations as defined in TFS RESTful API
    // used for fetching iteration ids needed to fetch developer capacity info 
    let getIterations (name : string) (password : string) =
        let iterationsUrl = "http://tfs.aiscorp.com:8080/tfs/DefaultCollection/Version 8/_apis/work/TeamSettings/Iterations?api-version=2.0-preview.1"
        let results = Async.RunSynchronously (getAsync<iterationResults>(iterationsUrl, name, password))
        results.value
               .GetEnumerator() 
               |> EnumeratorToEnumerableEx<iterationData>

    // fetches and returns a sequence of developer capacity info as defined in TFS
    let getIterationCapacities (name : string) (password : string) (id : string) =
        let capacitiesUrl = "http://tfs.aiscorp.com:8080/tfs/DefaultCollection/Version 8/_apis/work/TeamSettings/Iterations/" 
                                + id + "/capacities?api-version=2.0-preview.1"
        let results = Async.RunSynchronously (getAsync<capacityResults>(capacitiesUrl, name, password))
        results.value
               .GetEnumerator() 
               |> EnumeratorToEnumerableEx<capacityData>

    // represents the parent TFS project containing our Features, User Stories and Tasks
    let project = commonStructureService.GetProjectFromName("Version 8")

    // iterations themselves are queried via the object model (not the RESTful API)
    // this is a TFS representation of iterations (as NodeInfo structures)
    let iterations = commonStructureService.ListStructures(project.Uri)
                             |> Seq.where(fun s -> s.StructureType = "ProjectLifecycle")
                             |> Seq.head 

    // get the particular project associated with the R&D team
    let teamProjects = workItemStore.Projects
    let RDProject = 
        EnumeratorToEnumerable<Project>(teamProjects.GetEnumerator())
        |> Seq.where (fun p -> p.Name = "Version 8")
        |> Seq.head

    // construct our program's representation of iterations from TFS representation
    let iterationList = 
        if not (iterations = null) then 
            let projectName = RDProject.Name
            let iterationsTree = commonStructureService.GetNodesXml([| iterations.Uri |], true)
            let doc = XmlDocument() 
            doc.LoadXml(iterationsTree.InnerXml)
            let root2017IterationsNode = doc.SelectSingleNode("/descendant::*[@Name=2017]")

            // we need to skip the intermediate <Children> tag
            let nodesList = root2017IterationsNode.ChildNodes.[0].ChildNodes.GetEnumerator()
                                 |> EnumeratorToEnumerable<XmlNode>
                                 |> Seq.toList

            // we are explicitly adding iteration structures for the final three iterations of 2016   
            // and generating structures from the TFS representation for all those in 2017      
            {path= "\Version 8\2016\Iteration 7";
             startDate = Nullable<DateTime>(new DateTime(2016,10,31));
             endDate = Nullable<DateTime>(new DateTime(2016,11,18))
            }
            ::
            {path= "\Version 8\2016\Iteration 8";
             startDate = Nullable<DateTime>(new DateTime(2016,11,21));
             endDate = Nullable<DateTime>(new DateTime(2016,12,09))
            }
            ::
            {path= "\Version 8\2016\Iteration 9";
             startDate = Nullable<DateTime>(new DateTime(2016,12,12));
             endDate = Nullable<DateTime>(new DateTime(2016,12,30))
            }
            ::
            (nodesList
            |> List.map(fun n -> let iterationPath = n.Attributes.GetNamedItem("Path").Value
                                 // we are shedding one component of the XML path
                                 let trimmedIterationPath = iterationPath.Substring(1)
                                 let extraIterationIndex = trimmedIterationPath.IndexOf("Iteration\\")
                                 let trimmedIterationPath = trimmedIterationPath.Remove(extraIterationIndex,10)
                                 {  path = trimmedIterationPath;
                                    startDate = Nullable<DateTime>(DateTime.Parse(n.Attributes.GetNamedItem("StartDate").Value));
                                    endDate = Nullable<DateTime>(DateTime.Parse(n.Attributes.GetNamedItem("FinishDate").Value))
                                 }))
        else []

    // here are the developers on the R&D team
    let developerNames = ["Developer1"; "Developer2";  "Developer3"; "Developer4"; "Developer5"]

    let getUsableDevName displayName =
        (displayName
        |> Seq.takeWhile(fun c -> not (c = '<'))
        |> String.Concat).TrimEnd()

    // build a dictionary of developer capacities keyed by iteration path
    // each dictionary entry is iteself a dictionary of developer capacities keyed by dev name
    let mutable capacities = null
    let getCapacities (name : string) (password : string) =
        let iterationCapacities =
            getIterations name password 
            |> Seq.map(fun i -> (i.path,i.id)) 
            |> Seq.map(fun (path,id) -> let thisIterationCapacities = 
                                            let capacities = 
                                                getIterationCapacities name password id
                                                |> Seq.toList
                                            let teamCapacities =
                                                capacities
                                                |> Seq.where(fun c -> let nameToMatch =
                                                                        getUsableDevName
                                                                         c.teamMember.displayName
                     
                                                                      developerNames
                                                                        |> List.contains(
                                                                            nameToMatch))
                                            let dictTuples = 
                                                teamCapacities
                                                |> Seq.map (fun c ->  let devName =
                                                                        getUsableDevName
                                                                         c.teamMember.displayName
                                                                      (devName, c))
                                            let capacityDict = 
                                                dictTuples
                                                |> dict
                                            capacityDict
                                        (path,thisIterationCapacities))
            |> dict

        capacities <- iterationCapacities
        capacities
    
    // use the TFS RESTful API to get a collection of developer capacities
    // keyed by developer and by iteration path
    do (getCapacities "myLogin" "myPassword" |> ignore)  

    // return a nullable DateTime by parsing an incoming string
    let myDateTryParse dateStr = 
        let refDate = ref DateTime.Now
        let couldParse, refDate = DateTime.TryParse(dateStr)
        if (couldParse) then
            Nullable refDate
        else
            Nullable()

    let getDeveloperAvailableHoursInIterationWeek iterationPath week developer =
        let iteration = 
            iterationList
            |> List.where(fun i -> i.path = iterationPath) 
            |> List.exactlyOne
        let iterationCapacities = capacities.[iterationPath]

        // for non-team members (represented by "Resource1") assume 30 available hours
        // for team members derive hours available based on TFS capacities
        let hoursAvailableInSelectedWeek = 
            if (developer = "Resource1") then
                30
            else
                let daysOffInIteration = 
                        let developerIterationCapacity = iterationCapacities.[developer]
                        developerIterationCapacity.daysOff
                let weekStartDay = iteration.startDate.Value.AddDays(float (week-1) * float 7)
                let weekEndDay = weekStartDay.AddDays(float 6)
                let daysOffInSelectedWeek = 
                    daysOffInIteration
                        .FindAll(fun dayOff -> 
                                    (DateTime.Parse(dayOff.start).Day <= weekEndDay.Day) &&
                                    (DateTime.Parse(dayOff.``end``).Day >= weekStartDay.Day))
                        .GetEnumerator()
                    |> EnumeratorToEnumerableEx<dayOff>
                    |> Seq.map(fun dayOff -> let startDate = DateTime.Parse(dayOff.start, 
                                                                null, DateTimeStyles.RoundtripKind)
                                             let endDate = DateTime.Parse(dayOff.``end``, 
                                                                null, DateTimeStyles.RoundtripKind)
                                             let startDay = (int (startDate.DayOfWeek))
                                             let endDay = (int (endDate.DayOfWeek))
                                             seq { 
                                                    for day in startDay..endDay do
                                                        if ((day >= 1) && (day <= 5)) then yield day
                                                 })
                    |> Seq.collect(fun dayOff -> dayOff)
                    |> Seq.distinct
                (30 - (daysOffInSelectedWeek |> Seq.length) * 6)
        float hoursAvailableInSelectedWeek
   
    // generate a schedule of iteration capacities for each developer
    let initDeveloperSchedule name = 
        [for x in [1..17] 
            do for y in [1..3] 
                do 
                    let iterationPath = @"Version 8\2017\Iteration " + x.ToString()
                    let weekInIteration = y
                    let totalHours = getDeveloperAvailableHoursInIterationWeek iterationPath weekInIteration name
                    yield
                        { developer = name;
                          iteration = iterationPath; 
                          weekInIteration = weekInIteration; 
                          totalHours = totalHours; 
                        } // do for each iteration (there are rougly 17)
       ]

    // this is used for anyone not on the R&D team
    let anonDeveloper = 
        { developerName = "Resource1";
            schedule = 
            initDeveloperSchedule "Resource1" }

    // build a dictionary of developers keyed by developer name
    let developers =
        let developerList =
            developerNames
            |> List.map(fun n -> { developerName = n;
                                   schedule = initDeveloperSchedule n
                                 })

        let developerList2 =
             anonDeveloper :: developerList

        let developerDict =
            developerList2
            |> List.map(fun d -> d.developerName, d)
            |> dict

        developerDict
    
    // returns a developer if on the team, or the anonymous Developer if not
    let getDeveloper name =
      try
        developers.[name]
      with
      | _ -> anonDeveloper

    // get the current iteration based on today's date
    let currentIteration = 
        iterationList |> List.where(fun i -> let now = DateTime.Now
                                             i.startDate.HasValue &&
                                             i.endDate.HasValue && 
                                             (i.startDate.Value).Date <= now.Date &&
                                             (i.endDate.Value).Date > now.Date)
                      |> List.exactlyOne

    // get the current iteration week based on today's date
    // is a number between 1 and 3
    let currentWeekInIteration = (((DateTime.Now - currentIteration.startDate.Value).Days)/ 7) + 1

    // get the iteration following that passed in as a parameter
    let getNextIteration currentIteration =
        iterationList |> List.where(fun i -> i.startDate.HasValue &&
                                             i.endDate.HasValue && 
                                             (i.startDate.Value = currentIteration.endDate.Value))
                      |> List.exactlyOne

    // look up an iteration by its path
    let getIterationFromPath path =
        iterationList |> List.where(fun i -> i.path = path)
                      |> List.exactlyOne

    // return a list of TFS WorkItem fieldChanges for an incoming list of fields
    let getFieldChanges(wi : WorkItem, fieldsToCompare : string list) =
        let compareRevision (rev1 : Revision) (rev2 : Revision) = 
          try  
            let revisionComparison = fieldsToCompare 
                                     |> List.map(fun f -> 
                                                    if not (rev1.Fields.[f].Value = rev2.Fields.[f].Value) then
                                                        Some {
                                                                taskId = wi.Id;
                                                                fieldName = f;
                                                                preChangeValue = rev1.Fields.[f].Value;
                                                                postChangeValue = rev2.Fields.[f].Value;
                                                                changedBy = getDeveloper (rev2.Fields.["Changed By"].Value.ToString());
                                                                changedDate = DateTime.Parse (rev2.Fields.["Changed Date"].Value.ToString())
                                                             }
                                                    else 
                                                        None
                                                )
                                     |> List.where(fun rd -> not (rd = None))
                                     |> List.map(fun rd -> rd.Value)

            revisionComparison
          with
            | _ -> printfn "this is an exception"    ; []  
            
        // get list of fieldChanges by stepping through workItem's revisions and comparing each to the one before it                  
        let revisions = wi.Revisions.GetEnumerator()
                        |> EnumeratorToEnumerable<Revision>
                        |> Seq.windowed(2)
                        |> Seq.map(fun delta -> compareRevision delta.[0] delta.[1])
                        |> Seq.collect(fun delta -> delta)
                        |> Seq.toList
        
        // some fields may have been initialized in the first revision 
        let initialRevs =
            fieldsToCompare
            |> List.map(fun f -> let rev = wi.Revisions.[0]
                                 if not (rev.Fields.[f].Value = null) then 
                                     Some {
                                            taskId = wi.Id;
                                            fieldName = f;
                                            preChangeValue = rev.Fields.[f].Value;
                                            postChangeValue = rev.Fields.[f].Value;
                                            changedBy = getDeveloper (rev.Fields.["Changed By"].Value.ToString());
                                            changedDate = DateTime.Parse (rev.Fields.["Changed Date"].Value.ToString())
                                          }
                                 else
                                    None)
            |> List.where(fun rd -> not (rd = None))
            |> List.map(fun rd -> rd.Value)

        initialRevs
        |> List.append revisions

    // return a TFS WorkItem's immediate child tasks
    // (there can also be child user stories)
    let getImmediateChildTasks(wi : WorkItem)  =
        let childLinks = wi.WorkItemLinks.GetEnumerator()
                        |> EnumeratorToEnumerable<WorkItemLink>
                        |> Seq.where(fun wil -> wil.LinkTypeEnd.Name = "Child")

        let tasks = childLinks |> Seq.map(fun wil -> wil.TargetId)
                        |> Seq.map(fun id -> workItemStore.GetWorkItem(id))
                        |> Seq.where(fun wi -> wi.Type.Name = "Task" &&
                                                not (wi.State = "Removed"))
                        |> Seq.toList
        tasks

    // get all descendant child tasks of a parent recursively
    let rec getAllChildTasks(parent : WorkItem) =
        let retVal = 
            let fieldsToCompare = ["State";"Iteration Path";"Assigned To";"Completed Work";"Remaining Work";"Original Estimate"]
            let fieldChanges = getFieldChanges(parent, fieldsToCompare)
            {  task = parent;
               scheduled = false;
               hoursScheduledSoFar = 0.0;
               remainingWork = parent.Fields.["Remaining Work"].Value
                                        |> (fun d -> if d = null then 0.0 else float(d.ToString()));
               stateChanges = fieldChanges
                               |> List.where(fun fc -> fc.fieldName = "State")
                               |> List.sortBy(fun fc -> fc.changedDate)
               iterationChanges = fieldChanges
                               |> List.where(fun fc -> fc.fieldName = "Iteration Path")
                               |> List.sortBy(fun fc -> fc.changedDate)
               assignedToChanges = fieldChanges
                               |> List.where(fun fc -> fc.fieldName = "Assigned To")
                               |> List.sortBy(fun fc -> fc.changedDate)
               completedChanges = fieldChanges
                               |> List.where(fun fc -> fc.fieldName = "Completed Work") 
                               |> List.sortBy(fun fc -> fc.changedDate)
               remainingChanges = fieldChanges
                               |> List.where(fun fc -> fc.fieldName = "Remaining Work")
                               |> List.sortBy(fun fc -> fc.changedDate)
               originalEstimateChanges = fieldChanges
                               |> List.where(fun fc -> fc.fieldName = "Original Estimate")
                               |> List.sortBy(fun fc -> fc.changedDate)
            } ::
                (getImmediateChildTasks parent
                |> Seq.map(fun wi -> getAllChildTasks wi)
                |> Seq.concat
                |> Seq.toList)
        retVal

    // get the immediate child User Stories of a particular workItem
    // (whch can be either the top-level Feature or a parent User Stor)
    let getImmediateChildUserStories(wi : WorkItem)  =
        let childLinks = wi.WorkItemLinks.GetEnumerator()
                        |> EnumeratorToEnumerable<WorkItemLink>
                        |> Seq.where(fun wil -> wil.LinkTypeEnd.Name = "Child")

        let userStories = childLinks |> Seq.map(fun wil -> wil.TargetId)

                        |> Seq.map(fun id -> workItemStore.GetWorkItem(id))
                        |> Seq.where(fun wi -> wi.Type.Name = "User Story")
                        |> Seq.toList
        userStories

    // get all child User Stories of a particular workItem recursively 
    // (whch can be either the top-level Feature or a parent User Story)
    let rec getAllChildUserStories(parent : WorkItem) =
        let retVal = {  userStory = parent;
                        sortOrder = float(parent.Fields.["Story Points"].Value.ToString())                  
                        tasks = getImmediateChildTasks parent
                        |> Seq.map(fun wi -> getAllChildTasks wi)
                        |> Seq.concat
                        |> Seq.toList
                     } ::
                        (getImmediateChildUserStories parent
                        |> Seq.map(fun wi -> getAllChildUserStories wi)
                        |> Seq.concat
                        |> Seq.toList)
        retVal


    // this is the TFS query language query we will use to fetch our features of interest
    let query = "Select [State], [Title] 
                From WorkItems
                Where [Work Item Type] = 'Feature'
                And [Iteration Path] = 'Version 8\\2017'
                Order By [State] Asc, [Changed Date] Desc"

    // execute the TFS query to fetch applicable features
    // returns a TFS WorkItemCollection object
    let featureCollection = workItemStore.Query(query)

    // now contruct a list of this program's representation of returned features
    let features = featureCollection.GetEnumerator()
                    |> EnumeratorToEnumerable<WorkItem>
                    |> Seq.map(fun feat -> let childUserStories = 
                                                feat.WorkItemLinks.GetEnumerator()
                                                    |> EnumeratorToEnumerable<WorkItemLink>
                                                    |> Seq.map(fun wil -> wil.TargetId)
                                                    |> Seq.map(fun id -> workItemStore.GetWorkItem(id))
                                                    |> Seq.where(fun wi -> wi.Type.Name = "User Story")
                                                    |> Seq.map(fun wi -> getAllChildUserStories wi)
                                                    |> Seq.concat
                                                    |> Seq.toList
                                            
                                           let iterationNotes =
                                                feat.WorkItemLinks.GetEnumerator()
                                                    |> EnumeratorToEnumerable<WorkItemLink>
                                                    |> Seq.map(fun wil -> wil.TargetId)
                                                    |> Seq.map(fun id -> workItemStore.GetWorkItem(id))
                                                    |> Seq.where(fun wi -> ((wi.Type.Name = "Issue") &&
                                                                            (wi.Title.Contains("Iteration Notes"))))
                                                    |> Seq.toList

                                           let projectedDates =
                                                feat.WorkItemLinks.GetEnumerator()
                                                    |> EnumeratorToEnumerable<WorkItemLink>
                                                    |> Seq.map(fun wil -> wil.TargetId)
                                                    |> Seq.map(fun id -> workItemStore.GetWorkItem(id))
                                                    |> Seq.where(fun wi -> ((wi.Type.Name = "Issue") &&
                                                                            (wi.Title.Contains("Projected Date"))))
                                                    |> Seq.toList
                                           
                                            // used only for debugging to see list of possible fields
                                           let fields = if not (iterationNotes = []) then
                                                           iterationNotes.Head.Fields.GetEnumerator()
                                                            |> EnumeratorToEnumerable<Field>
                                                            |> Seq.map(fun f -> f.Name + ": " + (if (f.Value = null) then "" else f.Value.ToString()))
                                                            |> Seq.toList
                                                        else
                                                            []
                                           
                                           { feature = feat; 
                                             userStories = childUserStories;
                                             iterationNotes = iterationNotes;
                                             projectedDates = projectedDates                                                             
                                           })
                    |> Seq.sortBy(fun f -> Int32.Parse (f.feature.Fields.["Business Value"].Value.ToString()))
                    |> Seq.toList

    // for debugging purposes to output task information
    // ususally not invoked, because very costly
    let printTaskFields (taskToBePrinted : task) =
        taskToBePrinted.task.Fields.GetEnumerator()
            |> EnumeratorToEnumerable<Field>
            |> Seq.map(fun f -> if Object.ReferenceEquals(f.Value,null) then
                                    () 
                                else
                                    printf "%s: %s\n" f.Name (f.Value.ToString()))
            |> Seq.toList
             

    // get hours available for a developer for a particular week based on TFS capacity info
    let getDeveloperIterationWeekHours developer iteration iterationWeek =
        let developerWeek = 
            developer.schedule
            |> List.where(fun dw -> dw.iteration = iteration &&
                                    dw.weekInIteration = iterationWeek)
            |> List.exactlyOne
        developerWeek.totalHours

    // derive a dictionary of developerLayout structures keyed by dev name
    // (used to track developer commitments across projects and iteration weeks)
    let developerLayoutSchedules =
        developers.Values.GetEnumerator()
        |> EnumeratorToEnumerableEx<developer>
        |> Seq.map(fun d -> let remainingHours =
                                    getDeveloperIterationWeekHours d currentIteration.path 
                                        currentWeekInIteration
                            let layoutSchedule =
                                {
                                    developerToSchedule = d;
                                    currentLayoutIteration = currentIteration;
                                    currentLayoutIterationWeek = currentWeekInIteration;
                                    remainingHours = float remainingHours
                                }
                            (d.developerName, layoutSchedule))
        |> dict

    // main function that generates task commitment info for developers assigned to implement user stories
    // used in a map below to derive a list of developer task commitments for each feature
    let scheduleFeatureIntoIdealDeveloperSprint (feature : feature) =
        let orderedUserStories = feature.userStories
                                 |> List.sortBy(fun f -> f.sortOrder)
        
//        printTaskFields orderedUserStories.Head.tasks.Head |> ignore

        // schedule tasks for a particular week; 
        // generate dev task commitment records, while there are remaining hours left
        // if the last task can't fit withing remaining hours, then
        // allocate the remaining hours and generate overage record for scheduling in the 
        // following week
        // also generates historical task commitment records for tasks completed in the past

        // find the particular iteration that includes the incoming date
        let findIteration (fromDate : DateTime) = 
            let iteration = iterationList |> List.where(fun i -> i.startDate.HasValue && i.endDate.HasValue &&
                                                                 i.startDate.Value.Date <= fromDate.Date && i.endDate.Value.Date > fromDate.Date)
                                          |> List.exactlyOne
            iteration.path

        // find the particular iteration week that includes the incoming date
        let findIterationWeek (fromDate : DateTime) =
            let iteration = iterationList |> List.where(fun i -> i.startDate.HasValue && i.endDate.HasValue &&
                                                                 i.startDate.Value.Date <= fromDate.Date && i.endDate.Value.Date > fromDate.Date)
                                          |> List.exactlyOne
            // is a number from 1 to 3
            let week = (((fromDate - iteration.startDate.Value).Days)/ 7) + 1
            week

        // get a developer's week capacity info given his or her commitments thus far 
        let getDeveloperIterationWeek developerLayout =
            let currentIteration = developerLayout.currentLayoutIteration
            let currentIterationWeek = developerLayout.currentLayoutIterationWeek
            let developer = developerLayout.developerToSchedule
            let weekSchedule = 
                developer.schedule
                |> List.where(fun iw -> iw.iteration = currentIteration.path &&
                                        iw.weekInIteration = currentIterationWeek)
                |> List.exactlyOne
            weekSchedule

        // get all Completed Work changes that happened for a task during a particular iteration
        let getInterimCompleted task (iteration : scheduleInfo) (iterationWeek : int) =
            let matchingCompleted = task.completedChanges
                                    |> List.where(fun c -> c.changedDate >= iteration.startDate.Value &&
                                                           c.changedDate < iteration.endDate.Value)
            matchingCompleted
                       
        // get all Remaining Work changes that happened for a task during a particular iteration
        let getInterimRemaining task (iteration : scheduleInfo) (iterationWeek : int) =
            let matchingRemaining = task.remainingChanges
                                    |> List.where(fun c -> c.changedDate >= iteration.startDate.Value &&
                                                           c.changedDate < iteration.endDate.Value)
            matchingRemaining

        // create a new task commitment record given a task, a developer's availability info 
        // and the parent user story
        let createTaskCommitment t developerLayout us =
            let taskId = t.task.Id
            let taskState = t.task.Fields.["State"].Value.ToString()
            let taskTitle = t.task.Fields.["Title"].Value.ToString()
            let originalEstimate = t.task.Fields.["Original Estimate"].Value
                                    |> (fun d -> if d = null then 0.0 else float(d.ToString()))
            let mutable remainingWork = t.task.Fields.["Remaining Work"].Value
                                        |> (fun d -> if d = null then 0.0 else float(d.ToString()))
            let mutable completedWork = t.task.Fields.["Completed Work"].Value
                                        |> (fun d -> if d = null then 0.0 else float(d.ToString()))
            let assignedTo = t.task.Fields.["Assigned To"].Value
                                |> (fun d -> if d = null then "" else d.ToString())
            let activatedDate = t.task.Fields.["Activated Date"].Value
                                |> (fun d -> if d = null then "" else d.ToString())
            let completedDate = t.task.Fields.["Closed Date"].Value
                                |> (fun d -> if d = null then "" else d.ToString())
            let iterationActivated = if activatedDate = "" then "" 
                                        else findIteration (DateTime.Parse activatedDate)
            let iterationCompleted = if completedDate = "" then "" 
                                        else findIteration (DateTime.Parse completedDate)
            let iterationWeekCompleted = if completedDate = "" then -1
                                            else findIterationWeek (DateTime.Parse completedDate)
            let isContinuedTask = if (t.hoursScheduledSoFar > 0.0) then true else false;
            let isGeneratedPrecedingTask = false

            {   committedDeveloper = developerLayout.developerToSchedule;
                committedTask = t;
                parentUserStory = us;
                taskId = taskId;
                taskTitle = taskTitle;
                taskState = taskState;
                originalEstimate = originalEstimate;
                remainingWork = remainingWork;
                completedWork = completedWork;
                projectedCompletedWork = 0.0;
                projectedRemainingWork = 0.0;
                activatedDate = activatedDate;
                completedDate = completedDate;
                iterationActivated = iterationActivated;
                iterationCompleted = iterationCompleted;
                iterationWeekCompleted = iterationWeekCompleted;
                committedIteration = developerLayout.currentLayoutIteration;
                committedIterationWeek = developerLayout.currentLayoutIterationWeek;
                hoursAgainstBudget = 0.0;
                isContinuedTask = isContinuedTask;
                isGeneratedPrecedingTask = isGeneratedPrecedingTask
            }

        // this is the main workhorse algorithm that steps through a list of 
        // tasks to schedule for a particular developer and generates task commitment 
        // records (both for past already-completed tasks and future to-be-done tasks)
        let rec addStoryTasksToDeveloperSchedule (developerLayout : developerLayout) 
                                                 (tasksToSchedule : task list) 
                                                 devTaskCommits us =
                // iterate through unscheduled tasks and generate new developerTaskCommitment records for each one;
                // on each iteration return hours left after subtracting this task's hours , as well as the list of
                // developerTaskCommitments accumulated so far
                let devTaskCommitmentLists =
                    tasksToSchedule
                    |> List.scan (fun (acc, devTaskCommitList) (t : task) -> 
//                                        let retVal =
                                        if ((acc >= 0.0) && ((t.remainingWork > 0.0) || ((t.remainingWork = 0.0) && (t.task.State = "Closed")))) then
                                            let mutable overrideScheduled = false
                                            let devTaskCommitment =
                                                createTaskCommitment t developerLayout us
                                            let availableHoursAgainstBudget =
                                                match devTaskCommitment.taskState with
                                                | "New" -> devTaskCommitment.remainingWork - t.hoursScheduledSoFar
                                                | "Active" -> devTaskCommitment.remainingWork - t.hoursScheduledSoFar
                                                | _ -> 0.0

                                            // adjust status of scheduled task and calculate hours budgeted,
                                            // depending on whether it was able to be completely scheduled
                                            let mutable updatedBudget = acc - availableHoursAgainstBudget
                                            let mutable hoursAgainstBudget =
                                                if updatedBudget >= 0.0 then 
                                                    t.scheduled <- true; t.hoursScheduledSoFar <- 0.0
                                                    availableHoursAgainstBudget
                                                else 
                                                    t.scheduled <- false; t.hoursScheduledSoFar <- availableHoursAgainstBudget + updatedBudget;
                                                    t.remainingWork <- t.remainingWork - availableHoursAgainstBudget;
                                                    availableHoursAgainstBudget + updatedBudget

                                            devTaskCommitment.hoursAgainstBudget <- hoursAgainstBudget

                                            // if record is active, we may need to generate records for previous
                                            // weeks in this iteration
                                            let mutable updatedDevTaskCommitList = 
                                                match devTaskCommitment.taskState with
                                                | "Active" ->
                                                    let iterationWeekList = [1 .. (currentWeekInIteration-1) ]
                                                    let mutable updatedBudget2 = acc
                                                    let listWithPriorIterations =
                                                        iterationWeekList
                                                        |> List.fold 
                                                            (fun (acc2 : developerTaskCommitment list) 
                                                                 (i : int) -> let overrideCompleted =
                                                                                getInterimCompleted t currentIteration i
                                                                              let currentTask = devTaskCommitment
                                                                              if overrideCompleted.Length > 0 then
                                                                                let priorTask = createTaskCommitment t developerLayout us
                                                                                priorTask.isGeneratedPrecedingTask <- true
                                                                                priorTask.completedWork <- overrideCompleted
                                                                                                        |> List.last
                                                                                                        |> (fun fc -> float (fc.postChangeValue.ToString()))
                                                                                priorTask.committedIterationWeek <- i
                                                                                let overrideRemaining =
                                                                                    getInterimRemaining t currentIteration i                            
                                                                                if overrideRemaining.Length > 0 then
                                                                                    priorTask.remainingWork <- overrideRemaining
                                                                                                            |> List.last
                                                                                                            |> (fun fc -> float (fc.postChangeValue.ToString()))
    //                                                                            devTaskCommitment.completedWork <- priorTask.remainingWork
                                                                                priorTask :: acc2
                                                                              else 
                                                                                acc2
                                                            ) 
                                                            devTaskCommitList
                                                    devTaskCommitment.projectedCompletedWork <- hoursAgainstBudget
                                                    devTaskCommitment.projectedRemainingWork <- availableHoursAgainstBudget - hoursAgainstBudget
                                                    listWithPriorIterations
                                                | "Closed" ->
                                                    devTaskCommitment.committedIteration <- getIterationFromPath devTaskCommitment.iterationCompleted
                                                    devTaskCommitment.committedIterationWeek <- devTaskCommitment.iterationWeekCompleted
                                                    devTaskCommitList
                                                | "New" ->
                                                    devTaskCommitment.projectedCompletedWork <- hoursAgainstBudget
                                                    devTaskCommitment.projectedRemainingWork <- availableHoursAgainstBudget - hoursAgainstBudget
                                                    devTaskCommitList
                                                | _ ->
                                                    devTaskCommitList
                                            (updatedBudget, devTaskCommitment :: updatedDevTaskCommitList)
                                        else
                                            (acc, devTaskCommitList))
                            (developerLayout.remainingHours, devTaskCommits)

                // the scan above has returned a list of intermediate states after each iteration
                // return the first record with 0 or negative hours remaining; if there are none, take the last record
                // selected record's list will be the list of tasks that had at least some time scheduled against time remaining
                let remainingCommitmentLists =  
                    devTaskCommitmentLists
                    |> List.skipWhile(fun (hr,cl) -> hr >= 0.0 )

                let devTaskCommitmentList =
                    if remainingCommitmentLists = []then // all tasks fit with time left over so last record has correct task list
                        devTaskCommitmentLists |> List.last 

                    else  // some tasks that couldn't fit, so first remaining record has list 
                        let (hr,cl) = remainingCommitmentLists |> List.head 
                        if developerLayout.currentLayoutIterationWeek = 3 then
                            let nextIteration = getNextIteration(developerLayout.currentLayoutIteration)
                            developerLayout.currentLayoutIteration <- nextIteration
                            developerLayout.currentLayoutIterationWeek <- 1
                            cl.Head.isGeneratedPrecedingTask <- true
                        else
                            let nextIterationWeek = (developerLayout.currentLayoutIterationWeek + 1)
                            developerLayout.currentLayoutIterationWeek <- nextIterationWeek

                        developerLayout.remainingHours <- getDeveloperIterationWeek(developerLayout).totalHours
                        addStoryTasksToDeveloperSchedule developerLayout 
                                    (tasksToSchedule |> List.where(fun t -> t.scheduled = false))cl us
    
                // this will always be >= 0, because if it weren't, addStoryTasksToDeveloperSchedule
                // woud be called recursively until the user story is completely laid out
                developerLayout.remainingHours <- devTaskCommitmentList |> fst
                devTaskCommitmentList

        // schedule a user story's tasks for each assigned developer
        // used in a fold below to accumulate a list of task commitments for all a feature's user stories
        let addStoryToSchedule accDeveloperTaskCommits us =
            // filter out tasks that have already been scheduled
            let usTaskCommits = 
                us.tasks 
                |> List.where(fun t -> t.scheduled = false)
                |> List.groupBy(fun t -> t.task.Fields.["Assigned To"].Value.ToString())
                |> List.map(
                    fun g -> let developerLayout = 
                                match g |> fst with
                                | "Developer3" -> developerLayoutSchedules.["Developer3"]
                                | "Developer2" -> developerLayoutSchedules.["Developer2"]
                                | "Developer4" -> developerLayoutSchedules.["Developer4"]
                                | "Developer1" -> developerLayoutSchedules.["Developer1"]
                                | "Developer5" -> developerLayoutSchedules.["Developer5"]
                                | _ -> developerLayoutSchedules.["Resource1"]

                             let tasksToSchedule = g |> snd
                             let developerTaskCommits = 
                                addStoryTasksToDeveloperSchedule developerLayout
                                                                 tasksToSchedule
                                                                 [] us
                             developerTaskCommits) 
                
                |> List.fold (fun acc tc -> tc |> snd
                                            |> List.rev
                                            |> List.append acc) []
            usTaskCommits
            |> List.append accDeveloperTaskCommits
                             

        // accumulate a list of all tasks from all user stories for this feature
        let scheduledStories = List.fold addStoryToSchedule [] orderedUserStories

        scheduledStories

    // executes a query to find all tasks done by R&D on behalf of System Engineering Services
    let ``getR&DForSysEngTasks`` =

        let sysEngWorkQuery = "Select [State], [Title] 
            From WorkItems
            Where [Work Item Type] = 'Task'
            And [Tags] Contain 'R&D For Sys Eng'
            Order By [State] Asc, [Changed Date] Desc"

        let sysEngWorkTasks = workItemStore.Query(query)

        sysEngWorkTasks


    // publicly available functions to C# client
    member public this.getFeatureTimelines = 
        features |> List.map(fun f -> (f,scheduleFeatureIntoIdealDeveloperSprint(f)))

    member public this.getSysEngTasks = 
        ``getR&DForSysEngTasks``

    member public this.getCurrentIteration = 
        currentIteration

    member public this.getCurrentIterationWeek =
        currentWeekInIteration

    member public this.getAllIterations = 
        iterationList

    member public this.getCapacity() = 
        capacities

    member public this.getWorkItemChildUserStories workItem =
        getImmediateChildUserStories workItem

    member public this.getWorkItemChildTasks workItem =
        getImmediateChildTasks workItem

    member public this.getDevelopers =
        developers

    member public this.getDeveloperByName name =
        getDeveloper name

    // call to this function is commented in or out of calling program
    // simply to add records to TFS that violate TFS's default rules and 
    // can only be done programmatically (to explicity set workItem revision date, for example) 
    member public this.addInitialBallparkProjection (featureName : string) = 
        let issueType = workItemStore.Projects.["Version 8"].WorkItemTypes.["Issue"]
        let newInitialBallparkProjection = 
            new WorkItem(issueType)
        newInitialBallparkProjection.Title <- "Projected Date: Original Target Completion Date"
        newInitialBallparkProjection.IterationPath <- @"Version 8\2017\Iteration 3"
        newInitialBallparkProjection.Fields.["Due Date"].Value <- Convert.ToDateTime("2017-04-07")
        newInitialBallparkProjection.Fields.["System.ChangedDate"].Value <- Convert.ToDateTime("2017-02-27")

        let validationErrors = newInitialBallparkProjection.Validate()

        newInitialBallparkProjection.Save()

        let myCamJPFeature = features
                             |> List.where(fun f -> f.feature.Title = featureName)
                             |> List.exactlyOne

        let hierarchicalLink = workItemStore.WorkItemLinkTypes.["System.LinkTypes.Hierarchy"]
        myCamJPFeature.feature.WorkItemLinks.Add(new WorkItemLink(hierarchicalLink.ForwardEnd, newInitialBallparkProjection.Id))
        |> ignore
        myCamJPFeature.feature.Save();


    member this.X = "F#"

