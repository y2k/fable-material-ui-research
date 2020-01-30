open System

[<AutoOpen>]
module Prelude =
    let inline (^) f x = f x
    let inline (<!) f a () = f a
    let inline (>>=) ma mf = async.Bind(ma, mf)
    let inline (>>-) ma f = async.Bind(ma, f >> async.Return)
    let inline flip f a b = f b a
    // let wrap fmodel fmsg (a, b) = a |> fmodel, b |> Elmish.Cmd.map fmsg

type Post = { userId: int; id: int; title: string; body: string }
type Comment = { postId: int; id: int; name: string; email: string; body: string }
type 'a States = Loading | Failed of exn | Success of 'a
    with static member map f = function Success x -> Success ^ f x | x -> x

module Fetch =
    let fetch<'a> _url _ : 'a Async = failwith "???"

module PostScreen =
    type Model = { post: Post States; comments: Comment [] States }
    type Msg =  PostChanged of Post States | CommentsChanged of Comment [] States

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

[<EntryPoint>]
let main _ =
    printfn "Start"
    0
