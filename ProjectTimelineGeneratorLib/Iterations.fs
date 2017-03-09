module Iterations

open System
open System.Xml
open System.Globalization
open Common
open ProjectTimelineGeneratorLib.Domain
open TFS

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
        {   path= "\Version 8\2016\Iteration 7";
            startDate = Nullable<DateTime>(new DateTime(2016,10,31));
            endDate = Nullable<DateTime>(new DateTime(2016,11,18))
        }
        ::
        {   path= "\Version 8\2016\Iteration 8";
            startDate = Nullable<DateTime>(new DateTime(2016,11,21));
            endDate = Nullable<DateTime>(new DateTime(2016,12,09))
        }
        ::
        {   path= "\Version 8\2016\Iteration 9";
            startDate = Nullable<DateTime>(new DateTime(2016,12,12));
            endDate = Nullable<DateTime>(new DateTime(2016,12,30))
        }
        ::
        (nodesList
        |> List.map(fun n ->    let iterationPath = n.Attributes.GetNamedItem("Path").Value
                                // we are shedding one component of the XML path
                                let trimmedIterationPath = iterationPath.Substring(1)
                                let extraIterationIndex = trimmedIterationPath.IndexOf("Iteration\\")
                                let trimmedIterationPath = trimmedIterationPath.Remove(extraIterationIndex,10)
                                {  path = trimmedIterationPath;
                                startDate = Nullable<DateTime>(DateTime.Parse(n.Attributes.GetNamedItem("StartDate").Value));
                                endDate = Nullable<DateTime>(DateTime.Parse(n.Attributes.GetNamedItem("FinishDate").Value))
                                }))
    else []

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

