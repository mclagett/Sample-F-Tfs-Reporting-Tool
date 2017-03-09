module TFS

open System
open System.Runtime.Serialization
open Microsoft.TeamFoundation
open Microsoft.TeamFoundation.Common
open Microsoft.TeamFoundation.Server
open Microsoft.TeamFoundation.WorkItemTracking.Client
open Microsoft.TeamFoundation.ProjectManagement
//open System.Web
//open System.Net.Http
//open System.Net.Http.Headers
//open System.IO

open Common
open ProjectTimelineGeneratorLib.Domain

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


