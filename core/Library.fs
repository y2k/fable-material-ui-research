namespace MaterialUiResearch

[<AutoOpen>]
module Prelude =
    let inline (^) f x = f x
    let inline (<!) f a () = f a
    let inline (>>=) ma mf = async.Bind(ma, mf)
    let inline (>>-) ma f = async.Bind(ma, f >> async.Return)
    let inline flip f a b = f b a
    let inline wrap fmodel fmsg (a, b) = a |> fmodel, b |> Elmish.Cmd.map fmsg
    let inline fst (a, _) = a
    let inline snd (_, b) = b
    [<System.Obsolete>]
    let TODO() = failwith "???"

type Post = { userId: int; id: int; title: string; body: string }
type Comment = { postId: int; id: int; name: string; email: string; body: string }
type 'a States = Loading | Failed of exn | Success of 'a
    with static member map f = function Success x -> Success ^ f x | x -> x

module PostScreen =
    type Model = { post: Post States; comments: Comment [] States }
    type Msg =  PostChanged of Post States | CommentsChanged of Comment [] States

module FeedScreen =
    type Model = Post [] States
    type Msg = PostsLoaded of Result<Post [], exn> | OpenPost of int

module Application =
    type Model =
        | PostModel of PostScreen.Model
        | PostsModel of FeedScreen.Model
        | NoneModel
    type Msg =
        | PostMsg of PostScreen.Msg
        | PostsMsg of FeedScreen.Msg
    