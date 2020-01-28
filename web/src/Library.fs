module MaterialUiResearch

[<AutoOpen>]
module Prelude =
    let inline (^) f x = f x
    let inline (<!) f a () = f a
    let inline (>>=) ma mf = async.Bind(ma, mf)
    let inline (>>-) ma f = async.Bind(ma, f >> async.Return)
    let inline flip f a b = f b a
    let inline cmap fmodel fmsg (model, cmd) = model |> fmodel, cmd |> Elmish.Cmd.map fmsg
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
        open Fetch
        open Elmish

        type Model = Post [] States
        type Msg = PostsLoaded of Result<Post [], exn> | OpenPost of int

        let downloadPosts() =
            fetch "https://jsonplaceholder.typicode.com/posts" []
            |> Promise.bind ^ fun r -> r.json<Post []>()

        let init _ = 
            Loading, Cmd.OfPromise.either downloadPosts () (Ok >> PostsLoaded) (Error >> PostsLoaded)

        let update model = function
            | OpenPost id -> model, Navigation.Navigation.modifyUrl ^ sprintf "/post/%i" id
            | PostsLoaded (Ok posts) -> Success posts, Cmd.none
            | PostsLoaded (Error e) -> Failed e, Cmd.none

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

        let contentView model =
            div [ Style [ PaddingTop 60; PaddingBottom 50 ] ] [
                yield
                    match model with
                    | Loading ->
                        div [ Style [ Display DisplayOptions.Flex; JustifyContent "center" ] ] [
                            if model = Loading then
                                yield circularProgress [ LinearProgressProp.Color LinearProgressColor.Secondary ] ]
                    | Success posts ->
                        list [] [ yield! posts |> Array.map (fun x -> listItem [] [ viewItem x ]) ]
                    | Failed _ -> div [] []
                yield
                    snackbar 
                        [ MaterialProp.Open (match model with Failed _ -> true | _ -> false)
                          SnackbarProp.Message ^ str "Error" ] [] ]

        let view model _ =
            fragment [] [
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
    
        let route = oneOf [ map Post (s "post" </> i32); map Posts top ]
        let wrap fmodel fmsg (a, b) = a |> fmodel, b |> Cmd.map fmsg
        let urlUpdate (result: Route option) model = 
            match result with
            | Some Posts -> FeedScreen.Domain.init() |> wrap PostsModel PostsMsg
            | Some (Post id) ->  PostScreen.Domain.init id |> wrap PostModel PostMsg
            | None -> model, Navigation.modifyUrl "#"
        let init _ = FeedScreen.Domain.init() |> wrap PostsModel PostsMsg
        let update model msg = 
            match model, msg with
            | PostsModel m, PostsMsg mg ->
                FeedScreen.Domain.update m mg |> wrap PostsModel PostsMsg
            | PostModel m, PostMsg mg ->
                PostScreen.Domain.update m mg |> wrap PostModel PostMsg
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
open Elmish.HMR

Program.mkProgram Routing.Domain.init (flip Routing.Domain.update) Routing.View.view
|> Program.toNavigable (parseHash Routing.Domain.route) Routing.Domain.urlUpdate
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
