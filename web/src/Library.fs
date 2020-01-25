module MaterialUiResearch

[<AutoOpen>]
module Prelude =
    let inline (^) f x = f x
    let inline (<!) f a () = f a
    let inline (>>=) ma mf = async.Bind(ma, mf)
    let inline (>>-) ma f = async.Bind(ma, f >> async.Return)

module Domain =
    open Elmish
    open FSharp.Data

    type PostsApi = JsonProvider<"https://jsonplaceholder.typicode.com/posts">

    type Model = { posts : PostsApi.Root [] }
    type Msg = PostsLoaded of Result<PostsApi.Root [], exn>

    let init _ = 
        { posts = [||] },
        PostsApi.AsyncLoad "https://jsonplaceholder.typicode.com/posts"
        |> fun a -> Cmd.OfAsync.either (fun _ -> a) () (Ok >> PostsLoaded) (Error >> PostsLoaded)
    let update msg model =
        match msg with
        | PostsLoaded (Ok posts) -> { model with posts = posts }, Cmd.none
        | PostsLoaded (Error _) -> model, Cmd.none

module View =
    open Browser
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.Core
    open Fable.MaterialUI.Themes

    let viewAppBar title buttonTitle =
        appBar [ AppBarProp.Position AppBarPosition.Static ] [
            toolbar [] [
                typography [ 
                    Style [ FlexGrow 1 ]
                    TypographyProp.Variant TypographyVariant.H6
                    MaterialProp.Color ComponentColor.Inherit ] [ 
                    str title ]
                iconButton [] []
                button [ MaterialProp.Color ComponentColor.Inherit ] [ str buttonTitle ] ] ]

    let viewItem (i : Domain.PostsApi.Root) =
        card [] [
            div [ Style [ Display DisplayOptions.Flex; MarginTop "8dp"; FlexDirection "column" ] ] [
                button [ ButtonProp.Variant ButtonVariant.Contained ] [ 
                    str <| sprintf "Slide #%O" i ] ] ]

    let view (model : Domain.Model) _ =
        div [
            Style [ 
                Display DisplayOptions.Flex
                FlexGrow 1
                FlexDirection "column" ] ] [
            viewAppBar "Example" "Login"
            list [
                Style [
                    FlexGrow 1
                    Display DisplayOptions.Flex
                    FlexDirection "column" ] ] [
                    yield! model.posts |> Array.map viewItem ]
                    
            bottomNavigation [ BottomNavigationProp.ShowLabels true ] [
                bottomNavigationAction [ MaterialProp.Label <| str "Feed" ] 
                bottomNavigationAction [ MaterialProp.Label <| str "Tags" ] 
                bottomNavigationAction [ MaterialProp.Label <| str "Messages" ] 
                bottomNavigationAction [ MaterialProp.Label <| str "Profile" ] ] ]

open Elmish
open Elmish.React

Program.mkProgram Domain.init Domain.update View.view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
