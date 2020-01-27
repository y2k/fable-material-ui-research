module MaterialUiResearch

[<AutoOpen>]
module Prelude =
    let inline (^) f x = f x
    let inline (<!) f a () = f a
    let inline (>>=) ma mf = async.Bind(ma, mf)
    let inline (>>-) ma f = async.Bind(ma, f >> async.Return)
    let inline flip f a b = f b a

type Post = { userId: int; id: int; title: string; body: string }
type 'a States = Loading | Failed of exn | Success of 'a

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
    module Domain =
        open Fetch
        open Elmish

        type Model = Post States
        type Msg = Post States

        let downloadPost id = 
            sprintf "https://jsonplaceholder.typicode.com/posts/%i" id
            |> flip fetch []
            |> Promise.bind ^ fun r -> r.json<Post>()

        let init id = Loading, Cmd.OfPromise.either downloadPost id Success Failed
        let update _ msg = msg, Cmd.none

    module View =
        open Fable.React
        open Fable.React.Props
        open Fable.MaterialUI.Props
        open Fable.MaterialUI.Core

        let viewPost (i : Post) =
            card [] [
                cardContent [] [
                    typography [ Variant TypographyVariant.H6 ] [ 
                        str i.title ]
                    typography [ Variant TypographyVariant.Subtitle1 ] [ 
                        str i.body ] ]
                cardActions [] [
                    button [ ButtonProp.Size ButtonSize.Small ] [ str "Learn more" ] ] ]

        let viewContent model =
            match model with
            | Loading -> 
                div [ Style [ Display DisplayOptions.Flex; JustifyContent "center" ] ] [
                    circularProgress [ LinearProgressProp.Color LinearProgressColor.Secondary ] ]
            | Success p -> viewPost p
            | Failed _ -> div [] []

        let view (model: Domain.Model) _ =
            fragment [] [
                Styles.appBar "Post"
                div [ Style [ PaddingTop 60 ] ] [
                    viewContent model ] ]

module FeedScreen =
    module Domain =
        open Fable.Core
        open Fetch
        open Elmish

        type Model = { posts: Post []; error: bool; isBusy: bool }
        type Msg = PostsLoaded of Result<Post [], exn> | OpenPost of int

        let downloadPosts = async { 
            let! r = fetch "https://jsonplaceholder.typicode.com/posts" [] |> Async.AwaitPromise
            return! r.json<Post []> () |> Async.AwaitPromise }

        let init _ = 
            { posts = [||]; error = false; isBusy = true },
            downloadPosts
            |> fun a -> Cmd.OfAsync.either (fun _ -> a) () (Ok >> PostsLoaded) (Error >> PostsLoaded)

        let update model = function
            | OpenPost id -> model, Navigation.Navigation.modifyUrl ^ sprintf "/post/%i" id
            | PostsLoaded (Ok posts) -> { model with posts = Array.take 15 posts; isBusy = false }, Cmd.none
            | PostsLoaded (Error _) -> { model with error = true; isBusy = false }, Cmd.none

    module View =
        open Fable.React
        open Fable.React.Props
        open Fable.MaterialUI.Props
        open Fable.MaterialUI.Core

        let viewItem (i : Post) =
            card [] [
                cardContent [] [
                    typography [ Variant TypographyVariant.H6 ] [ 
                        str i.title ]
                    typography [ Variant TypographyVariant.Subtitle1 ] [ 
                        str i.body ] ]
                cardActions [] [
                    button 
                        [ ButtonProp.Size ButtonSize.Small
                          Href ^ sprintf "#post/%i" i.id ] [ 
                        str "Learn more" ] ] ]

        let contentView (model : Domain.Model) =
            div [ Style [ PaddingTop 60; PaddingBottom 50 ] ] [
                div [ Style [ Display DisplayOptions.Flex; JustifyContent "center" ] ] [
                    if model.isBusy then
                        yield circularProgress [ LinearProgressProp.Color LinearProgressColor.Secondary ] ]
                snackbar 
                    [ Open model.error
                      Message ^ str "Error" ] []
                list [] [
                    yield! 
                        model.posts 
                        |> Array.map (fun x -> listItem [] [ viewItem x ]) ] ]

        let view model _ =
            fragment [] [
                cssBaseline []
                Styles.appBar "Posts"
                contentView model
                appBar 
                    [ Style [ Bottom 0; Top "auto" ]
                      AppBarProp.Position AppBarPosition.Fixed ] [
                    bottomNavigation [ ShowLabels true ] [
                        bottomNavigationAction [ Label ^ str "Feed" ] 
                        bottomNavigationAction [ Label ^ str "Tags" ] 
                        bottomNavigationAction [ Label ^ str "Messages" ] 
                        bottomNavigationAction [ Label ^ str "Profile" ] ] ] ]

module Routing =
    open Elmish.UrlParser
    open Elmish.Navigation

    type Route = Posts | Post of int
    type SubModel =
        | PostModel of PostScreen.Domain.Model
        | PostsModel of FeedScreen.Domain.Model
        | NoneModel
    type Msg =
        | PostMsg of PostScreen.Domain.Msg
        | PostsMsg of FeedScreen.Domain.Msg

    module Domain =
        open Elmish
    
        let route =
            oneOf [ 
                map Post (s "post" </> i32)
                map Posts (s "posts") ]
        let urlUpdate (result: Route option) model = 
            match result with
            | Some Posts -> 
                let (a, b) = FeedScreen.Domain.init () 
                a |> PostsModel, b |> Cmd.map PostsMsg
            | Some (Post id) -> 
                let (a, b) = PostScreen.Domain.init id
                a |> PostModel, b |> Cmd.map PostMsg
            | None -> model, Navigation.modifyUrl "#"
        let init _ = urlUpdate (Some Posts) NoneModel
        let update model msg = 
            match model, msg with
            | PostsModel m, PostsMsg mg ->
                let (a, b) = FeedScreen.Domain.update m mg
                a |> PostsModel, b |> Cmd.map PostsMsg
            | PostModel m, PostMsg mg ->
                let (a, b) = PostScreen.Domain.update m mg
                a |> PostModel, b |> Cmd.map PostMsg
            | _ -> model, Cmd.none
    
    module View =
        let view model _ = 
            match model with
            | PostModel m -> PostScreen.View.view m ()
            | PostsModel m -> FeedScreen.View.view m ()
            | _ -> failwith "???"

open Elmish
open Elmish.React
open Elmish.Navigation
open Elmish.UrlParser

Program.mkProgram Routing.Domain.init (flip Routing.Domain.update) Routing.View.view
|> Program.toNavigable (parseHash Routing.Domain.route) Routing.Domain.urlUpdate
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
