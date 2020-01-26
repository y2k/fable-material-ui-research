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

module PostScreen =
    module Domain =
        open Fetch
        open Elmish

        type Model = Post States

        let downloadPost id = 
            sprintf "https://jsonplaceholder.typicode.com/posts/%i" id
            |> flip fetch [] 
            |> Promise.bind (fun r -> r.json<Post> ())

        let init id = Loading, Cmd.OfPromise.either downloadPost id Success Failed
        let update _ msg = msg, Cmd.none

    module View =
        open Fable.React
        open Fable.React.Props
        open Fable.MaterialUI.Props
        open Fable.MaterialUI.Core

        let view (model: Domain.Model) _ =
            div [] []

module FeedScreen =
    module Domain =
        open Fable.Core
        open Fetch
        open Elmish

        type Model = { posts: Post []; error: bool; isBusy: bool }
        type Msg = PostsLoaded of Result<Post [], exn>

        let downloadPosts = async { 
            let! r = fetch "https://jsonplaceholder.typicode.com/posts" [] |> Async.AwaitPromise
            return! r.json<Post []> () |> Async.AwaitPromise }

        let init _ = 
            { posts = [||]; error = false; isBusy = true },
            downloadPosts
            |> fun a -> Cmd.OfAsync.either (fun _ -> a) () (Ok >> PostsLoaded) (Error >> PostsLoaded)

        let update model = function
            | PostsLoaded (Ok posts) -> { model with posts = Array.take 15 posts; isBusy = false }, Cmd.none
            | PostsLoaded (Error _) -> { model with error = true; isBusy = false }, Cmd.none

    module View =
        open Fable.React
        open Fable.React.Props
        open Fable.MaterialUI.Props
        open Fable.MaterialUI.Core

        let viewAppBar title =
            appBar [ AppBarProp.Position AppBarPosition.Fixed ] [
                toolbar [] [
                    typography 
                        [ Style [ FlexGrow 1 ]
                          TypographyProp.Variant TypographyVariant.H6
                          MaterialProp.Color ComponentColor.Inherit ] [ 
                        str title ]
                    iconButton 
                        [ Style [ MarginRight -12 ]
                          MaterialProp.Color ComponentColor.Inherit ] [ 
                        icon [] [ str "more_vert" ] ] ] ]

        let viewItem (i : Post) =
            card [] [
                cardContent [] [
                    typography [ TypographyProp.Variant TypographyVariant.H6 ] [ 
                        str i.title ]
                    typography [ TypographyProp.Variant TypographyVariant.Subtitle1 ] [ 
                        str i.body ] ]
                cardActions [] [
                    button [ ButtonProp.Size ButtonSize.Small ] [ str "Learn more" ] ] ]

        let contentView (model : Domain.Model) =
            div [ Style [ PaddingTop 60; PaddingBottom 50 ] ] [
                div [ Style [ Display DisplayOptions.Flex; JustifyContent "center" ] ] [
                    if model.isBusy then
                        yield circularProgress [ LinearProgressProp.Color LinearProgressColor.Secondary ] ]
                snackbar 
                    [ MaterialProp.Open model.error
                      SnackbarProp.Message ^ str "Error" ] []
                list [] [
                    yield! 
                        model.posts 
                        |> Array.map (fun x -> listItem [] [ viewItem x ]) ] ]

        let view model _ =
            fragment [] [
                cssBaseline []
                viewAppBar "Posts"
                contentView model
                appBar 
                    [ Style [ Bottom 0; Top "auto" ]
                      AppBarProp.Position AppBarPosition.Fixed ] [
                    bottomNavigation [ BottomNavigationProp.ShowLabels true ] [
                        bottomNavigationAction [ MaterialProp.Label ^ str "Feed" ] 
                        bottomNavigationAction [ MaterialProp.Label ^ str "Tags" ] 
                        bottomNavigationAction [ MaterialProp.Label ^ str "Messages" ] 
                        bottomNavigationAction [ MaterialProp.Label ^ str "Profile" ] ] ] ]

open Elmish
open Elmish.React

Program.mkProgram PostScreen.Domain.init (flip PostScreen.Domain.update) PostScreen.View.view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.runWith 5
