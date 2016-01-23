namespace TicTacToe.Tests

open FsUnit
open NUnit.Framework

module TicTacToe =

    type Player = P1 | P2
    type State  = Player option[,]
    type EvalRes =
        | Tie 
        | Winner of Player
        | InProg

    let createState () = Array2D.zeroCreate<Player option> 3 3

    let choose player x y (state : State) =
        match state.[x, y] with
        | None -> 
            state.[x, y] <- Some player
            true
        | _ -> false

    let eval (state : State) : EvalRes =
        let isFilled (state : State) =
            seq {
                for x in 0..2 do
                    for y in 0..2 do
                        yield state.[x, y]
            }
            |> Seq.forall Option.isSome

        let areTheSame (players : Player option[]) =
            let fst = players.[0]
            if fst.IsSome && players |> Array.forall ((=) fst)
            then fst
            else None

        let patterns =
            seq {
                yield! [0..2] |> List.map (fun r -> state.[0..2, r])
                yield! [0..2] |> List.map (fun c -> state.[c, 0..2])
                yield  [|0..2|] |> Array.map (fun n -> state.[n, n])
                yield  [|0..2|] |> Array.map (fun n -> state.[n, 2-n])
            }
        
        match patterns |> Seq.tryPick areTheSame with
        | Some p -> Winner p
        | _ when isFilled state -> Tie
        | _ -> InProg

//    type GameOverRes = 
//        | Winner of Player
//        | Tie
//
//    type TicTacToeMoveRes =
//        | GameOver of GameOverRes
//        | InProg   of ITicTacToeGame
//        | WrongPlayer of ITicTacToeGame
//        | CellNotFree of ITicTacToeGame
//
//    and ITicTacToeGame =
//        abstract member Move : player:Player * x:int * y:int -> TicTacToeMoveRes
//
//    type TicTacToeGame () =
//        let state = createState()
//        let mutable currentP = P1
//
//        interface ITicTacToeGame with
//            member this.Move (p, x, y) =
//                if p <> currentP then WrongPlayer (this :> ITicTacToeGame)
//                elif not <| choose p x y state then CellNotFree (this :> ITicTacToeGame)
//                else
//                    currentP <- if currentP = P1 then P2 else P1
//
//                    match eval state with
//                    | EvalRes.Tie      -> GameOver Tie
//                    | EvalRes.Winner p -> GameOver (Winner p)
//                    | _                -> InProg (this :> ITicTacToeGame)
//
//        override this.ToString() =
//            state 
//            |> Array2D.map (function 
//                | Some P1 -> "O"
//                | Some P2 -> "X"
//                | _       -> " ")
//            |> sprintf "\n%A"

open TicTacToe

[<TestFixture>]
type MyTests ()=
    [<Test>]
    member __.``when P1 chooses 0, 0 and it's free then 0, 0 becomes P1`` () =    
        let state = createState()
        choose P1 0 0 state |> should equal true
        state.[0, 0] |> should equal <| Some P1

    [<Test>]
    member __.``when P1 chooses 0, 0 and it's not free then it should return false`` () =    
        let state = createState()
        choose P2 0 0 state |> should equal true
        choose P1 0 0 state |> should equal false

    [<Test>]
    member __.``when P1 choose 0, 0; 1, 0; and 2, 0 then P1 wins`` () =
        let state = createState()
        [ 0..2 ]
        |> List.iter (fun x -> choose P1 x 0 state |> ignore)

        eval state |> should equal <| Winner P1
    
    [<Test>]
    member __.``when P2 choose 0, 0; 1, 0; and 2, 0 then P2 wins`` () =
        let state = createState()
        [ 0..2 ]
        |> List.iter (fun x -> choose P2 x 0 state |> ignore)

        eval state |> should equal <| Winner P2
    
    [<Test>]
    member __.``when P1 choose 0, 1; 1, 1; and 2, 1 then P1 wins`` () =
        let state = createState()
        [ 0..2 ]
        |> List.iter (fun x -> choose P1 x 1 state |> ignore)

        eval state |> should equal <| Winner P1

    [<Test>]
    member __.``when P1 choose 0, 0; 0, 1; and 0, 2 then P1 wins`` () =
        let state = createState()
        [ 0..2 ]
        |> List.iter (fun y -> choose P1 0 y state |> ignore)

        eval state |> should equal <| Winner P1

    [<Test>]
    member __.``when P1 choose 0, 0; 1, 1; and 2, 2 then P1 wins`` () =
        let state = createState()
        [ 0..2 ]
        |> List.iter (fun n -> choose P1 n n state |> ignore)

        eval state |> should equal <| Winner P1

    [<Test>]
    member __.``when P1 choose 0, 2; 1, 1; and 2, 0 then P1 wins`` () =
        let state = createState()
        [ 0..2 ]
        |> List.iter (fun x -> choose P1 x (2-x)  state |> ignore)

        eval state |> should equal <| Winner P1

    [<Test>]
    member __.``when the grid is filled and no streak then it's a Tie`` () =
        let state = createState()
        state.[0,0] <- Some P1
        state.[0,1] <- Some P2
        state.[0,2] <- Some P2
        state.[1,0] <- Some P2
        state.[1,1] <- Some P1
        state.[1,2] <- Some P1
        state.[2,0] <- Some P1
        state.[2,1] <- Some P2
        state.[2,2] <- Some P2

        eval state |> should equal Tie

    [<Test>]
    member __.``when P1 choose 0, 0; 1, 1 then it's still InProg`` () =
        let state = createState()
        [ 0..1 ]
        |> List.iter (fun n -> choose P1 n n state |> ignore)

        eval state |> should equal InProg