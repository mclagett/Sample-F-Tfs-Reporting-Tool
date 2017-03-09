module Features

open System
open Microsoft.TeamFoundation.WorkItemTracking.Client
open ProjectTimelineGeneratorLib.Domain
open Common
open WorkItems

// this is the TFS query language query we will use to fetch our features of interest
let query = "Select [State], [Title] 
            From WorkItems
            Where [Work Item Type] = 'Feature'
            And [Iteration Path] = 'Version 8\\2017'
            Order By [State] Asc, [Changed Date] Desc"

// execute the TFS query to fetch applicable features
// returns a TFS WorkItemCollection object
let featureCollection = executeQuery(query)

// now contruct a list of this program's representation of returned features
let features = featureCollection.GetEnumerator()
               |> EnumeratorToEnumerable<WorkItem>
               |> Seq.map(fun feat -> let childUserStories = 
                                            feat.WorkItemLinks.GetEnumerator()
                                            |> EnumeratorToEnumerable<WorkItemLink>
                                            |> Seq.map(fun wil -> wil.TargetId)
                                            |> Seq.map(fun id -> getWorkItemFromId(id))
                                            |> Seq.where(fun wi -> wi.Type.Name = "User Story")
                                            |> Seq.map(fun wi -> getAllChildUserStories wi)
                                            |> Seq.concat
                                            |> Seq.toList
                                            
                                      let iterationNotes =
                                            feat.WorkItemLinks.GetEnumerator()
                                            |> EnumeratorToEnumerable<WorkItemLink>
                                            |> Seq.map(fun wil -> wil.TargetId)
                                            |> Seq.map(fun id -> getWorkItemFromId(id))
                                            |> Seq.where(fun wi -> ((wi.Type.Name = "Issue") &&
                                                                    (wi.Title.Contains("Iteration Notes"))))
                                            |> Seq.toList

                                      let projectedDates =
                                            feat.WorkItemLinks.GetEnumerator()
                                            |> EnumeratorToEnumerable<WorkItemLink>
                                            |> Seq.map(fun wil -> wil.TargetId)
                                            |> Seq.map(fun id -> getWorkItemFromId(id))
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

