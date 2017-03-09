module Developers

open System
open Common
open ProjectTimelineGeneratorLib.Domain
open Iterations

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

// get hours available for a developer for a particular week based on TFS capacity info
let getDeveloperIterationWeekHours developer iteration iterationWeek =
    let developerWeek = 
        developer.schedule
        |> List.where(fun dw -> dw.iteration = iteration &&
                                dw.weekInIteration = iterationWeek)
        |> List.exactlyOne
    developerWeek.totalHours

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

