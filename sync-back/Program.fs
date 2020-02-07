module MaterialUiResearch.Backend

[<AutoOpen>]
module Prelude =
    let inline (^) f x = f x
    let inline (<!) f a () = f a
    let inline (>>=) ma mf = async.Bind(ma, mf)
    let inline (>>-) ma f = async.Bind(ma, f >> async.Return)
    let inline flip f a b = f b a
    let wrap fmodel fmsg (a, b) = a |> fmodel, b |> Elmish.Cmd.map fmsg

type Post = { userId: int; id: int; title: string; body: string }
type Comment = { postId: int; id: int; name: string; email: string; body: string }

module Store =
    type Db = { posts: Map<unit, Post []>; comments: Map<int, Comment []> }

module StoreSync =
    module Diff =
        let diffMapsAdd (ma : Map<'k, 'v>) (mb : Map<'k, 'v>) : Map<'k, 'v>=
            let added = Map.filter (fun k _ -> Map.containsKey k ma |> not) mb
            let changed =
                Map.toSeq ma
                |> Seq.choose (fun (k, v) -> if Map.containsKey k mb && Map.find k mb <> v then Some (k, Map.find k mb) else None)
                |> Map.ofSeq
            Map.fold (fun acc k v -> Map.add k v acc) added changed
        let diffMapsRemove (ma : Map<'k, 'v>) (mb : Map<'k, 'v>) : Set<'k> =
            Map.toSeq ma |> Seq.choose (fun (k, _) -> if Map.containsKey k mb then None else Some k) |> Set.ofSeq
        let diffMaps (ma : Map<'k, 'v>) (mb : Map<'k, 'v>) : Map<'k, 'v> * Set<'k> =
            let added = Map.filter (fun k _ -> Map.containsKey k ma |> not) mb
            let removed = Map.toSeq ma |> Seq.choose (fun (k, _) -> if Map.containsKey k mb then None else Some k) |> Set.ofSeq
            let changed =
                Map.toSeq ma
                |> Seq.choose (fun (k, v) -> if Map.containsKey k mb && Map.find k mb <> v then Some (k, Map.find k mb) else None)
                |> Map.ofSeq
            Map.fold (fun acc k v -> Map.add k v acc) added changed, removed
        let applyDiff m changed deleted =
            let m = deleted |> Set.fold (fun acc k -> Map.remove k acc) m
            Map.fold (fun acc k v -> Map.add k v acc) m changed

    type SyncDb = 
        { posts_changed: Map<unit, Post []>
          posts_removed: Set<unit>
          comments_changed: Map<int, Comment []>
          comments_removed: Set<int> }
    type LocalDb = Store.Db

    open Thoth.Json.Net

    let serializeDiff (a : LocalDb) (b : LocalDb) =
        { posts_changed = Diff.diffMapsAdd a.posts b.posts
          posts_removed = Diff.diffMapsRemove a.posts b.posts
          comments_changed = Diff.diffMapsAdd a.comments b.comments
          comments_removed = Diff.diffMapsRemove a.comments b.comments }
        |> fun x -> Encode.Auto.toString (0, x)

    let applyDiff (a : LocalDb) (data : string) : LocalDb =
        let df = Decode.Auto.unsafeFromString<SyncDb> data
        { a with
            posts = Diff.applyDiff a.posts df.posts_changed df.posts_removed
            comments = Diff.applyDiff a.comments df.comments_changed df.comments_removed }

module StoreHandler =
    open System

    type RequestTag = PostsRequest | CommentRequest of int

    let mkRequests (db: Store.Db) =
        let needDownloadComments =
            db.comments 
            |> Map.filter (fun _ v -> Seq.isEmpty v) 
            |> Map.toList
            |> List.map ^ fun (k, _) -> k
        [ if not ^ Map.isEmpty db.posts then
              yield PostsRequest, Uri "https://jsonplaceholder.typicode.com/posts"
          for i in needDownloadComments do
              yield CommentRequest i, Uri ^ sprintf "https://jsonplaceholder.typicode.com/posts/%i/comments" i ]

    let private convert<'a> (json: string) : 'a =
        Newtonsoft.Json.JsonConvert.DeserializeObject<'a> json

    let handleResponses responses db =
        let handleResponse (db: Store.Db) = function
            | PostsRequest, json -> 
                let posts = convert<Post []> json
                { db with posts = Map.add () posts db.posts }
            | CommentRequest id, json -> 
                let comments = convert<Comment []> json
                { db with comments = Map.add id comments db.comments }
        Seq.fold handleResponse db responses

    let private client = new Net.Http.HttpClient()

    let handle db = 
        mkRequests db
        |> List.map ^ fun (tag, uri: Uri) ->
            client.GetStringAsync uri |> Async.AwaitTask
            >>- fun json -> tag, json
        |> Async.Sequential
        >>- fun responses -> handleResponses responses db

module Server =
    open Suave
    open Suave.Successful
    open Suave.Filters
    open Suave.Operators
    open Thoth.Json.Net

    let private updateRouteCmd = 
        request ^ fun r ctx -> 
            async {
                let! result = 
                    r.rawForm 
                    |> System.Text.Encoding.UTF8.GetString
                    |> Decode.Auto.fromString<Store.Db>
                    |> function Ok x -> x | Error e -> failwith e
                    |> StoreHandler.handle 
                return! Encode.Auto.toString(0, result) |> OK |> fun wp -> wp ctx 
            }

    let start =
        choose [ 
            POST >=> path "/update" >=> updateRouteCmd ]
        |> startWebServerAsync { defaultConfig with bindings = [ HttpBinding.createSimple HTTP "0.0.0.0" 8081 ] }
        |> snd
    
[<EntryPoint>]
let main _ = 
    Server.start |> Async.RunSynchronously
    0
