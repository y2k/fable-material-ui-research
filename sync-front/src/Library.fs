module MaterialUiResearch

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
    let update (_: Db -> Db) : Db Async = failwith "???"

module StoreHandle =
    open System

    let convert<'a> (_json: string) : 'a = failwith "???"

    type RequestTag = PostsRequest | CommentRequest of int

    let mkRequests (db: Store.Db) = function
        | PostsRequest, json -> 
            let posts = convert<Post []> json
            { db with posts = posts }
        | CommentRequest id, json -> 
            let comments = convert<Comment []> json
            { db with comments = Map.add id comments db.comments }

    let mkRequest (db: Store.Db) =
        let needDownloadComments =
            db.comments 
            |> Map.filter (fun k v -> Seq.isEmpty v) 
            |> Map.toList
            |> List.map ^ fun (k, _) -> k

        [ if Array.isEmpty db.posts then
              yield PostsRequest, Uri "https://jsonplaceholder.typicode.com/posts"
          for i in needDownloadComments do
              yield CommentRequest i, Uri ^ sprintf "https://jsonplaceholder.typicode.com/posts/%i/comments" i ]

    let handle mkRequests handleResponse = async {
        let! db = Store.update id
        use client = new Net.Http.HttpClient()

        let requests = mkRequests db

        let! responses =
            requests
            |> List.map ^ fun (tag, uri: Uri) ->
                client.GetStringAsync uri |> Async.AwaitTask
                >>- fun json -> tag, json
            |> Async.Sequential

        let! _ = Store.update ^ fun db -> responses |> Array.fold handleResponse db
        () }

module Styles =
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.Core

    let appBar title =
        appBar [ AppBarProp.Position AppBarPosition.Fixed ] [
            toolbar [] [
                typography 
                    [ Style [ FlexGrow 1 ]
                      Variant TypographyVariant.H6
                      MaterialProp.Color ComponentColor.Inherit ] [ 
                    str title ]
                iconButton 
                    [ Style [ MarginRight -12 ]
                      MaterialProp.Color ComponentColor.Inherit ] [ 
                    icon [] [ str "more_vert" ] ] ] ]

module PostScreen =
    type Model = { post: Post States; comments: Comment [] States }
    type Msg = PostChanged of Post States | CommentsChanged of Comment [] States

    module Domain =
        open Fetch
        open Elmish

        // let download<'a> url = 
        //     sprintf "https://jsonplaceholder.typicode.com%s" url
        //     |> flip fetch []
        //     |> Promise.bind ^ fun r -> r.json<'a>()
        // let downloadPost = sprintf "/posts/%i" >> download<Post>
        // let downloadComments = sprintf "/posts/%i/comments" >> download<Comment []>
        let downloadPost' postId =
            Store.update id
            >>- fun db -> db.posts |> Array.find (fun x -> x.id = postId)
        let downloadComments' postId =
            Store.update ^ fun db -> { db with comments = Map.add postId [||] db.comments }
            >>- fun db -> Map.find postId db.comments

        let init id = 
            { post = Loading; comments = Loading },
            Cmd.batch [
                Cmd.OfAsync.either downloadPost' id (Success >> PostChanged) (Failed >> PostChanged)
                Cmd.OfAsync.either downloadComments' id (Success >> CommentsChanged) (Failed >> CommentsChanged) ]
        let update model = function
            | PostChanged x -> { model with post = x }, Cmd.none
            | CommentsChanged x -> 
                { model with comments = States<_>.map (Array.take 10) x }, Cmd.none

    module View =
        open Fable.React
        open Fable.React.Props
        open Fable.MaterialUI.Props
        open Fable.MaterialUI.Core

        let viewPost (i : Post) =
            fragment [] [
                typography [ Variant TypographyVariant.H6 ] [ 
                    str i.title ]
                typography [ Variant TypographyVariant.Subtitle1 ] [ 
                    str i.body ] ]

        let viewContent model =
            match model with
            | Loading -> 
                div [ Style [ Display DisplayOptions.Flex; JustifyContent "center" ] ] [
                    circularProgress [ LinearProgressProp.Color LinearProgressColor.Secondary ] ]
            | Success p -> viewPost p
            | Failed _ -> div [] []

        let viewComment (c : Comment) = 
            typography [ Variant TypographyVariant.Subtitle1 ] [ 
                str c.body ]

        let viewComments = function
            | Loading -> 
                div [ Style [ Display DisplayOptions.Flex; JustifyContent "center" ] ] [
                    circularProgress [ LinearProgressProp.Color LinearProgressColor.Secondary ] ]
            | Success commnets -> 
                list [] [ yield! commnets |> Array.map (fun x -> listItem [ ListItemProp.Divider true ] [ viewComment x ]) ]
            | Failed _ -> div [] []

        let view (model : Model) _ =
            fragment [] [
                Styles.appBar "Post"
                div [ Style [ PaddingTop 60 ] ] [
                    viewContent model.post
                    typography [ Variant TypographyVariant.H6 ] [ str "Comments:" ]
                    viewComments model.comments ] ]

module FeedScreen =
    type Model = Post [] States
    type Msg = PostsLoaded of Result<Post [], exn> | OpenPost of int

    module Domain =
        open Fetch
        open Fable.Core
        open Elmish

        let downloadPosts() =
            Store.update ^ fun db -> { db with started = true }
            >>- fun db -> db.posts
            // let! r = fetch "https://jsonplaceholder.typicode.com/posts" [] |> Async.AwaitPromise
            // return! r.json<Post []>() |> Async.AwaitPromise }

        let init _ = 
            Loading, Cmd.OfAsync.either downloadPosts () (Ok >> PostsLoaded) (Error >> PostsLoaded)

        let update model = function
            | OpenPost id -> model, Navigation.Navigation.newUrl ^ sprintf "#post/%i" id
            | PostsLoaded (Ok posts) -> Success posts, Cmd.none
            | PostsLoaded (Error e) -> Failed e, Cmd.none

    module View =
        open Fable.React
        open Fable.React.Props
        open Fable.MaterialUI.Props
        open Fable.MaterialUI.Core

        let viewItem dispatch (i : Post) =
            card [] [
                cardContent [] [
                    typography [ Variant TypographyVariant.H6 ] [ 
                        str i.title ]
                    typography [ Variant TypographyVariant.Subtitle1 ] [ 
                        str i.body ] ]
                cardActions [] [
                    button 
                        [ ButtonProp.Size ButtonSize.Small
                          OnClick ^ fun _ -> dispatch ^ OpenPost i.id ] [ 
                        str "Learn more" ] ] ]

        let contentView model dispatch =
            div [ Style [ PaddingTop 60; PaddingBottom 50 ] ] [
                yield
                    match model with
                    | Loading ->
                        div [ Style [ Display DisplayOptions.Flex; JustifyContent "center" ] ] [
                            if model = Loading then
                                yield circularProgress [ LinearProgressProp.Color LinearProgressColor.Secondary ] ]
                    | Success posts ->
                        list [] [ yield! posts |> Array.map (fun x -> listItem [] [ viewItem dispatch x ]) ]
                    | Failed _ -> div [] []
                yield
                    snackbar 
                        [ Open ^ match model with Failed _ -> true | _ -> false
                          Message ^ str "Error" ] [] ]

        let view model dispatch =
            fragment [] [
                Styles.appBar "Posts"
                contentView model dispatch
                appBar 
                    [ Style [ Bottom 0; Top "auto" ]
                      AppBarProp.Position AppBarPosition.Fixed ] [
                    bottomNavigation [ ShowLabels true ] [
                        bottomNavigationAction [ Label ^ str "Feed" ] 
                        bottomNavigationAction [ Label ^ str "Tags" ] 
                        bottomNavigationAction [ Label ^ str "Messages" ] 
                        bottomNavigationAction [ Label ^ str "Profile" ] ] ] ]

module Application =
    type Model =
        | PostModel of PostScreen.Model
        | PostsModel of FeedScreen.Model
        | NoneModel
    type Msg =
        | PostMsg of PostScreen.Msg
        | PostsMsg of FeedScreen.Msg

    module Domain =
        open Elmish.UrlParser
        open Elmish.Navigation
        open Elmish
    
        type Route = Posts | Post of int

        let route = oneOf [ map Post (s "post" </> i32); map Posts top ]
        let urlUpdate (result: Route option) model = 
            match result with
            | Some Posts -> FeedScreen.Domain.init() |> wrap PostsModel PostsMsg
            | Some (Post id) ->  PostScreen.Domain.init id |> wrap PostModel PostMsg
            | None -> model, Navigation.modifyUrl "#"
        let init r = urlUpdate r NoneModel
        let update model msg = 
            match model, msg with
            | PostsModel m, PostsMsg mg ->
                FeedScreen.Domain.update m mg |> wrap PostsModel PostsMsg
            | PostModel m, PostMsg mg ->
                PostScreen.Domain.update m mg |> wrap PostModel PostMsg
            | _ -> model, Cmd.none
    
    module View =
        open Fable.React
        open Fable.MaterialUI.Core
        let view model dispatch =
            fragment [] [
                cssBaseline []
                (match model with
                 | PostModel m -> PostScreen.View.view m (PostMsg >> dispatch)
                 | PostsModel m -> FeedScreen.View.view m (PostsMsg >> dispatch)
                 | _ -> failwith "???") ]

open Elmish
open Elmish.React
open Elmish.Navigation
open Elmish.UrlParser
open Elmish.HMR

Program.mkProgram Application.Domain.init (flip Application.Domain.update) Application.View.view
|> Program.toNavigable (parseHash Application.Domain.route) Application.Domain.urlUpdate
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
