module WorkItems

open System
open Microsoft.TeamFoundation.WorkItemTracking.Client
open Common
open ProjectTimelineGeneratorLib.Domain
open TFS
open Developers
open Iterations
    
// return a list of TFS WorkItem fieldChanges for an incoming list of fields
let getFieldChanges(wi : WorkItem, fieldsToCompare : string list) =
    let compareRevision (rev1 : Revision) (rev2 : Revision) = 
        try  
        let revisionComparison = [ for f in fieldsToCompare do
                                    if not (rev1.Fields.[f].Value = rev2.Fields.[f].Value) then
                                        yield {
                                            taskId = wi.Id;
                                            fieldName = f;
                                            preChangeValue = rev1.Fields.[f].Value;
                                            postChangeValue = rev2.Fields.[f].Value;
                                            changedBy = getDeveloper (rev2.Fields.["Changed By"].Value.ToString());
                                            changedDate = DateTime.Parse (rev2.Fields.["Changed Date"].Value.ToString())
                                        }
                                ]

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
        let rev = wi.Revisions.[0]
        [ for f in fieldsToCompare do
            if not (rev.Fields.[f].Value = null) then 
                yield {
                    taskId = wi.Id;
                    fieldName = f;
                    preChangeValue = rev.Fields.[f].Value;
                    postChangeValue = rev.Fields.[f].Value;
                    changedBy = getDeveloper (rev.Fields.["Changed By"].Value.ToString());
                    changedDate = DateTime.Parse (rev.Fields.["Changed Date"].Value.ToString())
                }
       ]

    initialRevs
    |> List.append revisions

// return a TFS WorkItem's immediate child tasks
// (there can also be child user stories)
let getImmediateChildTasks(wi : WorkItem)  =
    let childLinks = wi.WorkItemLinks.GetEnumerator()
                    |> EnumeratorToEnumerable<WorkItemLink>
                    |> Seq.where(fun wil -> wil.LinkTypeEnd.Name = "Child")

    let tasks = childLinks 
                    |> Seq.map(fun wil -> workItemStore.GetWorkItem(wil.TargetId))
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

// get all Completed Work changes that happened for a task during a particular iteration
let getInterimCompleted task (iteration : scheduleInfo)  =
    let matchingCompleted = task.completedChanges
                            |> List.where(fun c -> c.changedDate >= iteration.startDate.Value &&
                                                    c.changedDate < iteration.endDate.Value)
    matchingCompleted
                       
// get all Remaining Work changes that happened for a task during a particular iteration
let getInterimRemaining task (iteration : scheduleInfo)  =
    let matchingRemaining = task.remainingChanges
                            |> List.where(fun c -> c.changedDate >= iteration.startDate.Value &&
                                                    c.changedDate < iteration.endDate.Value)
    matchingRemaining

let getWorkItemFromId (id : int) =
    workItemStore.GetWorkItem(id)

let executeQuery queryText =
    workItemStore.Query(queryText)

let getWorkItemType (typeName : string) = 
    workItemStore.Projects.["Version 8"].WorkItemTypes.[typeName]

let getWorkItemLinkType (linkTypeName : string) = 
    workItemStore.WorkItemLinkTypes.[linkTypeName]