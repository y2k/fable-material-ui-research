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
    type Db = { started: bool; posts: Post []; comments: Map<int, Comment []> }

module StoreHandler =
    open System

    type RequestTag = PostsRequest | CommentRequest of int

    let mkRequests (db: Store.Db) =
        let needDownloadComments =
            db.comments 
            |> Map.filter (fun _ v -> Seq.isEmpty v) 
            |> Map.toList
            |> List.map ^ fun (k, _) -> k
        [ if Array.isEmpty db.posts then
              yield PostsRequest, Uri "https://jsonplaceholder.typicode.com/posts"
          for i in needDownloadComments do
              yield CommentRequest i, Uri ^ sprintf "https://jsonplaceholder.typicode.com/posts/%i/comments" i ]

    let private convert<'a> (json: string) : 'a =
        Newtonsoft.Json.JsonConvert.DeserializeObject<'a> json

    let handleResponses responses db =
        let handleResponse (db: Store.Db) = function
            | PostsRequest, json -> 
                let posts = convert<Post []> json
                { db with posts = posts }
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
