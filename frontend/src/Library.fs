module MaterialUiResearch.Frontend

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
    open MaterialUiResearch.PostScreen

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
    open MaterialUiResearch.FeedScreen

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
    open MaterialUiResearch.Application

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

module Proxy =
    open Thoth.Json
    open Elmish
    open Fable.Core.JsInterop
    open Fable.React
    open Fetch

    type Model = Application.Model option
    type Msg = SubMsg of Application.Msg | ModelUpdated of Application.Model

    let init () : Model * Cmd<Msg> = None, []

    let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
        match msg with
        | ModelUpdated model -> Some model, []
        | SubMsg msg ->
            model,
            Cmd.ofSub ^ fun dispatch -> 
                promise {
                    let serMsg = Encode.Auto.toString (0, msg)
                    let! response = fetch "http://localhost:8081/update" [ Body !^ serMsg; Method HttpMethod.POST ]
                    let! json = response.text()
                    let model = Decode.Auto.fromString json |> function Ok x -> x | Error e -> failwith e
                    dispatch ^ ModelUpdated model

                    let serMsg = Encode.Auto.toString (0, msg)
                    let! response = fetch "http://localhost:8081/update-cmd" [ Body !^ serMsg; Method HttpMethod.POST ]
                    let! json = response.text()
                    let model = Decode.Auto.fromString json |> function Ok x -> x | Error e -> failwith e
                    dispatch ^ ModelUpdated model
                } |> Promise.start

    let sub _ : Cmd<Msg> =
        Cmd.ofSub ^ fun dispatch -> 
            promise {
                let! response = fetch "http://localhost:8081/init" [ Method HttpMethod.POST ]
                let! json = response.text()
                let model = Decode.Auto.fromString json |> function Ok x -> x | Error e -> failwith e
                dispatch ^ ModelUpdated model

                let! response = fetch "http://localhost:8081/init-cmd" [ Method HttpMethod.POST ]
                let! json = response.text()
                let model = Decode.Auto.fromString json |> function Ok x -> x | Error e -> failwith e
                dispatch ^ ModelUpdated model
            } |> Promise.start

    let view model dispatch =
        match model with 
        | Some model -> Application.View.view model (SubMsg >> dispatch)
        | None -> str "Loading..."

open Elmish
open Elmish.React
open Elmish.HMR

Program.mkProgram Proxy.init Proxy.update Proxy.view
|> Program.withSubscription Proxy.sub
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
