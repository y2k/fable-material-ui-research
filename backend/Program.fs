module MaterialUiResearch.Backend

module Fetch =
    let fetch<'a> (url: string) _ : 'a Async = async {
        use client = new System.Net.WebClient()
        let! json = client.DownloadStringTaskAsync url |> Async.AwaitTask
        let result = Newtonsoft.Json.JsonConvert.DeserializeObject<'a>(json)
        return result }

module PostScreen =
    open MaterialUiResearch.PostScreen

    module Domain =
        open Fetch
        open Elmish

        let download<'a> url = 
            sprintf "https://jsonplaceholder.typicode.com%s" url
            |> flip fetch<'a> []
        let downloadPost = sprintf "/posts/%i" >> download<Post>
        let downloadComments = sprintf "/posts/%i/comments" >> download<Comment []>

        let init id = 
            { post = Loading; comments = Loading },
            Cmd.batch [
                Cmd.OfAsync.either downloadPost id (Success >> PostChanged) (Failed >> PostChanged)
                Cmd.OfAsync.either downloadComments id (Success >> CommentsChanged) (Failed >> CommentsChanged) ]
        let update model = function
            | PostChanged x -> { model with post = x }, Cmd.none
            | CommentsChanged x -> 
                { model with comments = States<_>.map (Array.take 10) x }, Cmd.none

module FeedScreen =
    open MaterialUiResearch.FeedScreen

    module Domain =
        open Fetch
        open Elmish

        let downloadPosts() =
            fetch<Post []> "https://jsonplaceholder.typicode.com/posts" []

        let init _ = 
            Loading, Cmd.OfAsync.either downloadPosts () (Ok >> PostsLoaded) (Error >> PostsLoaded)

        let update model = function
            | PostsLoaded (Ok posts) -> Success posts, Cmd.none
            | PostsLoaded (Error e) -> Failed e, Cmd.none
            | OpenPost _ -> model, []

module String =
    let take n (s : string) = s.Substring(0, min n s.Length)

module Application =
    open MaterialUiResearch.FeedScreen
    open MaterialUiResearch.Application

    module Domain =
        open Elmish
    
        type Route = Posts | Post of int

        let init _ = FeedScreen.Domain.init() |> wrap PostsModel PostsMsg
        let update model msg =
            match model, msg with
            | _, PostsMsg (OpenPost id) -> 
                PostScreen.Domain.init id |> wrap PostModel PostMsg
            | PostsModel m, PostsMsg mg ->
                FeedScreen.Domain.update m mg |> wrap PostsModel PostsMsg
            | PostModel m, PostMsg mg ->
                PostScreen.Domain.update m mg |> wrap PostModel PostMsg
            | _ -> model, Cmd.none

module Server =
    open Suave
    open Suave.Successful
    open Suave.Filters
    open Suave.Operators
    open Thoth.Json.Net

    let model = Application.Domain.init () |> fst |> ref

    let private initRoute = 
        request ^ fun _ -> Encode.Auto.toString(0, !model) |> OK

    let private initRouteCmd =
        request ^ fun _ ctx ->
            async {
                do! Async.FromContinuations ^ fun (succ, _, _) ->
                        let cmdsInQueue = ref 0
                        let rec invokeCmds cmd =
                            cmdsInQueue := !cmdsInQueue + (List.length cmd)
                            for sub in cmd do
                                sub ^ fun msg -> 
                                    let (m, c) = Application.Domain.update !model msg
                                    model := m
                                    invokeCmds c
                                    cmdsInQueue := !cmdsInQueue - 1
                                    if !cmdsInQueue = 0 then succ ()
                        
                        let (m, cmd) = Application.Domain.init () 
                        model := m
                        invokeCmds cmd

                return! Encode.Auto.toString(0, !model) |> OK |> fun wp -> wp ctx }

    let private updateRoute = 
        request ^ fun r ctx ->
            async {
                let initMsg =
                    r.rawForm 
                    |> System.Text.Encoding.UTF8.GetString
                    |> Decode.Auto.fromString<Application.Msg>
                    |> function Ok x -> x | Error e -> failwith e

                let (m, _) = Application.Domain.update !model initMsg
                return! Encode.Auto.toString(0, m) |> OK |> fun wp -> wp ctx }

    let private updateRouteCmd = 
        request ^ fun r ctx ->
            async {
                let initMsg =
                    r.rawForm 
                    |> System.Text.Encoding.UTF8.GetString
                    |> Decode.Auto.fromString<Application.Msg>
                    |> function Ok x -> x | Error e -> failwith e

                do! Async.FromContinuations ^ fun (succ, _, _) ->
                        let cmdsInQueue = ref 0
                        let rec invokeCmds cmd =
                            cmdsInQueue := !cmdsInQueue + (List.length cmd)
                            if !cmdsInQueue = 0 then succ ()
                            for sub in cmd do
                                sub ^ fun msg -> 
                                    let (m, c) = Application.Domain.update !model msg
                                    model := m
                                    invokeCmds c
                                    cmdsInQueue := !cmdsInQueue - 1
                                    if !cmdsInQueue = 0 then succ ()
                        
                        let (m, cmd) = Application.Domain.update !model initMsg
                        model := m
                        invokeCmds cmd

                return! Encode.Auto.toString(0, !model) |> OK |> fun wp -> wp ctx }

    let start =
        choose [
            POST >=> path "/init" >=> initRoute
            POST >=> path "/init-cmd" >=> initRouteCmd
            POST >=> path "/update" >=> updateRoute
            POST >=> path "/update-cmd" >=> updateRouteCmd ]
        |> startWebServerAsync { defaultConfig with bindings = [ HttpBinding.createSimple HTTP "0.0.0.0" 8081 ] }
        |> snd

[<EntryPoint>]
let main _ =
    Async.RunSynchronously Server.start
    0
