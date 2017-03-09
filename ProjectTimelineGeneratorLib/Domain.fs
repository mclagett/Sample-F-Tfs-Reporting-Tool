namespace ProjectTimelineGeneratorLib.Domain

open System
//open Microsoft.TeamFoundation
//open Microsoft.TeamFoundation.Common
//open Microsoft.TeamFoundation.Server
open Microsoft.TeamFoundation.WorkItemTracking.Client
//open Microsoft.TeamFoundation.ProjectManagement

// represents a TFS iteration
type scheduleInfo = {path : string; startDate : Nullable<DateTime>; endDate : Nullable<DateTime>}

// these are the task states we are interested in
type taskState = | New of int | Active of int | Closed of int 

// more self-documenting
type iterationWeek = int

// tracks developer's availability for a given sprint week
type developerWeek = { developer : string; iteration : string ; weekInIteration : iterationWeek; 
                        totalHours : float; }

// represents an individual developer resource
type developer = { developerName : string; schedule : developerWeek list}

// represents a TFS workItem value change; 
// used in stepping through revision history to look for changes
type fieldChange = { taskId : int; fieldName : string; preChangeValue : obj; postChangeValue : obj; 
                        changedBy : developer; changedDate : DateTime}

// used to track important info for a TFS task as we step through our iteration
// schedule and either report on what's happened or project out future work
type task = { task : WorkItem; mutable scheduled : bool; mutable hoursScheduledSoFar : float;
                mutable remainingWork : float;
                stateChanges : fieldChange list; iterationChanges : fieldChange list;
                assignedToChanges : fieldChange list ; completedChanges : fieldChange list;
                remainingChanges : fieldChange list; originalEstimateChanges : fieldChange list}

// represents a TFS userStory that will have tasks committed against it
type userStory = {userStory : WorkItem; sortOrder : float; tasks : task list }

// represents a TFS feature, which is used to represent a project and to group 
// user stories, iteration Notes and projected dates
type feature = { feature : WorkItem; userStories : userStory list; iterationNotes : WorkItem list; projectedDates : WorkItem list}

// represents a chunk of developer time committed to a particular task
// this will either be actual hours for past or projected hours for future activities 
type developerTaskCommitment = { committedDeveloper : developer; committedTask : task; originalEstimate : float;
                                    mutable remainingWork : float; mutable completedWork : float; taskId : int; taskState : string; 
                                    mutable projectedCompletedWork : float; mutable projectedRemainingWork : float;
                                    taskTitle : string; parentUserStory : userStory; 
                                    iterationActivated : string; iterationCompleted : string; iterationWeekCompleted : int; 
                                    mutable committedIteration : scheduleInfo; mutable committedIterationWeek : int; 
                                    mutable hoursAgainstBudget : float; isContinuedTask : bool; mutable isGeneratedPrecedingTask : bool;
                                    activatedDate : string; completedDate : string }

// used to track the availability of a particular developer as we commit 
// developers to tasks for an iteration (used in projecting future work)
type developerLayout = {developerToSchedule : developer; mutable currentLayoutIteration : scheduleInfo; 
                            mutable currentLayoutIterationWeek : int; mutable remainingHours : float}

