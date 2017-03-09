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

let getWorkItemFromId (id : int) =
    workItemStore.GetWorkItem(id)

let executeQuery queryText =
    workItemStore.Query(queryText)

let getWorkItemType (typeName : string) = 
    workItemStore.Projects.["Version 8"].WorkItemTypes.[typeName]

let getWorkItemLinkType (linkTypeName : string) = 
    workItemStore.WorkItemLinkTypes.[linkTypeName]