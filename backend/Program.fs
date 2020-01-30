module MaterialUiResearch.Backend

module Fetch =
    let fetch<'a> (url: string) _ : 'a Async = async {
        use client = new System.Net.WebClient()
        let! json = client.DownloadStringTaskAsync url |> Async.AwaitTask
        let result = Newtonsoft.Json.JsonConvert.DeserializeObject<'a>(json)
        return result }

module Navigation =
    module Navigation =
        let newUrl _ : Elmish.Cmd<_> = []

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
            | OpenPost id -> model, Navigation.Navigation.newUrl ^ sprintf "#post/%i" id
            | PostsLoaded (Ok posts) -> Success posts, Cmd.none
            | PostsLoaded (Error e) -> Failed e, Cmd.none

module Application =
    open MaterialUiResearch.Application

    module Domain =
        open Elmish
    
        type Route = Posts | Post of int

        let init _ = FeedScreen.Domain.init() |> wrap PostsModel PostsMsg
        let update model msg = 
            match model, msg with
            | PostsModel m, PostsMsg mg ->
                FeedScreen.Domain.update m mg |> wrap PostsModel PostsMsg
            | PostModel m, PostMsg mg ->
                PostScreen.Domain.update m mg |> wrap PostModel PostMsg
            | _ -> model, Cmd.none

[<EntryPoint>]
let main _ =
    let (model, cmd) = Application.Domain.init None
    printfn "LOG 1 :: %O" model
    for sub in cmd do
        printfn "LOG 2 :: %O" sub
        sub ^ fun msg -> printfn "LOG 3 :: %s" ^ (sprintf "%O" msg).Substring(0, 512)
    System.Threading.Thread.Sleep 5_000
    0
