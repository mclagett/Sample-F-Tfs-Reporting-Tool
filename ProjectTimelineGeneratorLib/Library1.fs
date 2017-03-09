module ProjectTimelineGeneratorEngine

open System
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
open ProjectTimelineGeneratorLib.Domain
open Iterations
open Developers
open WorkItems
open Features

// a .NET class used for exposing functionality to our C# client (see public memeber at the end)
type TaskIterationLayoutEngine() = 
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

        let sysEngWorkTasks = executeQuery(sysEngWorkQuery)

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
        let issueType = getWorkItemType "Issue"
        let newInitialBallparkProjection = 
            new WorkItem(issueType)
        newInitialBallparkProjection.Title <- "Projected Date: Original Target Completion Date"
        newInitialBallparkProjection.IterationPath <- @"Version 8\2017\Iteration 3"
        newInitialBallparkProjection.Fields.["Due Date"].Value <- Convert.ToDateTime("2017-04-07")
        newInitialBallparkProjection.Fields.["System.ChangedDate"].Value <- Convert.ToDateTime("2017-02-27")

        let validationErrors = newInitialBallparkProjection.Validate()

        newInitialBallparkProjection.Save()

        let featureToProject = features
                             |> List.where(fun f -> f.feature.Title = featureName)
                             |> List.exactlyOne

        let hierarchicalLink = getWorkItemLinkType("System.LinkTypes.Hierarchy")
        featureToProject.feature.WorkItemLinks.Add(new WorkItemLink(hierarchicalLink.ForwardEnd, newInitialBallparkProjection.Id))
        |> ignore
        featureToProject.feature.Save();


    member this.X = "F#"

