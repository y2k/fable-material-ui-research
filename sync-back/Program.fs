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
type 'a States = Loading | Failed of exn | Success of 'a
    with static member map f = function Success x -> Success ^ f x | x -> x

module Store =
    type Db = { started: bool; posts: Post []; comments: Map<int, Comment []> }
    [<System.Obsolete>] 
    let update (_: Db -> Db) : Db Async = failwith "???"

module StoreHandle =
    open System

    let convert<'a> (_json: string) : 'a = failwith "???"

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

    let handleResponses responses db =
        let handleResponse (db: Store.Db) = function
            | PostsRequest, json -> 
                let posts = convert<Post []> json
                { db with posts = posts }
            | CommentRequest id, json -> 
                let comments = convert<Comment []> json
                { db with comments = Map.add id comments db.comments }
        Seq.fold handleResponse db responses

    let handle (mkRequests: Store.Db -> ('a * Uri) list) handleResponses = 
        use client = new Net.Http.HttpClient()
        Store.update id
        >>- mkRequests
        >>- List.map ^ fun (tag, uri: Uri) ->
                client.GetStringAsync uri |> Async.AwaitTask
                >>- fun json -> tag, json
        >>= Async.Sequential
        >>= fun responses -> Store.update ^ handleResponses responses
        |> Async.Ignore 

    let test _ = handle mkRequests handleResponses

    let handle' db (mkRequests: Store.Db -> ('a * Uri) list) handleResponses = 
        use client = new Net.Http.HttpClient()
        
        mkRequests db
        |> List.map ^ fun (tag, uri: Uri) ->
                client.GetStringAsync uri |> Async.AwaitTask
                >>- fun json -> tag, json
        |> Async.Sequential
        >>- fun responses -> handleResponses responses db

    let test _ = handle mkRequests handleResponsesw

    module Server =
        open Suave
        open Suave.Successful
        open Suave.Filters
        open Suave.Operators
        open Thoth.Json.Net
    
        let private updateRouteCmd = 
            request ^ fun r ctx ->
                async {
                    let inputDb =
                        r.rawForm 
                        |> System.Text.Encoding.UTF8.GetString
                        |> Decode.Auto.fromString<Store.Db>
                        |> function Ok x -> x | Error e -> failwith e
    
                    // do! Async.FromContinuations ^ fun (succ, _, _) ->
                    //         let cmdsInQueue = ref 0
                    //         let rec invokeCmds cmd =
                    //             cmdsInQueue := !cmdsInQueue + (List.length cmd)
                    //             if !cmdsInQueue = 0 then succ ()
                    //             for sub in cmd do
                    //                 sub ^ fun msg -> 
                    //                     let (m, c) = Application.Domain.update !model msg
                    //                     model := m
                    //                     invokeCmds c
                    //                     cmdsInQueue := !cmdsInQueue - 1
                    //                     if !cmdsInQueue = 0 then succ ()
                            
                    //         let (m, cmd) = Application.Domain.update !model initMsg
                    //         model := m
                    //         invokeCmds cmd
    
                    // return! Encode.Auto.toString(0, !model) |> OK |> fun wp -> wp ctx 
                    ()
                }
    
        let start =
            choose [
                // POST >=> path "/update-cmd" >=> updateRouteCmd 
            ]
            |> startWebServerAsync { defaultConfig with bindings = [ HttpBinding.createSimple HTTP "0.0.0.0" 8081 ] }
            |> snd
    
[<EntryPoint>]
let main _ = 0
