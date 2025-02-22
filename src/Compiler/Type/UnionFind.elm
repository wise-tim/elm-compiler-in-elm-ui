{- MANUALLY FORMATTED -}


module Compiler.Type.UnionFind exposing
    ( IO
    , LocalState
    , Point
    , doNothing
    , equivalent
    , fresh
    , get
    , init
    , modify
    , redundant
    , set
    , union
    )

{- This is based on the following implementations:

     - https://hackage.haskell.org/package/union-find-0.2/docs/src/Data-UnionFind-IO.html
     - http://yann.regis-gianas.org/public/mini/code_UnionFind.html

   It seems like the OCaml one came first, but I am not sure.

   Compared to the Haskell implementation, the major changes here include:

     1. No more reallocating PointInfo when changing the weight
     2. Using the strict modifyIORef

-}

import Extra.System.IO.Pure as IO
import Extra.System.IORef as IORef exposing (IORef)



-- POINT


type Point a
    = Pt (IORef (PointInfo a))


type PointInfo a
    = Info (IORef Int) (IORef a)
    | Link (Point a)



-- IO


type alias IO s t a =
    IO.IO ( LocalState s, t ) a


type alias LocalState a =
    ( -- weights
      IORef.State Int
    , -- descs
      IORef.State a
    , -- infos
      IORef.State (PointInfo a)
    )


init : LocalState a
init =
    ( -- weights
      IORef.init
    , -- descs
      IORef.init
    , -- infos
      IORef.init
    )


doNothing : IO.IO s ()
doNothing =
    \s -> ( (), s )


liftW : IORef.IO Int a -> IO s t a
liftW =
    IO.liftS (\( ( x, _, _ ), _ ) -> x) (\x ( ( _, bi, ci ), b ) -> ( ( x, bi, ci ), b ))


liftD : IORef.IO s a -> IO s t a
liftD =
    IO.liftS (\( ( _, x, _ ), _ ) -> x) (\x ( ( ai, _, ci ), b ) -> ( ( ai, x, ci ), b ))


liftI : IORef.IO (PointInfo s) a -> IO s t a
liftI =
    IO.liftS (\( ( _, _, x ), _ ) -> x) (\x ( ( ai, bi, _ ), b ) -> ( ( ai, bi, x ), b ))



-- HELPERS


fresh : a -> IO a t (Point a)
fresh value =
    IO.bind (liftW (IORef.new 1)) <|
        \weight ->
            IO.bind (liftD (IORef.new value)) <|
                \desc ->
                    IO.bind (liftI (IORef.new (Info weight desc))) <|
                        \link ->
                            IO.return (Pt link)


repr : Point a -> IO a t (Point a)
repr ((Pt ref) as point) =
    IO.bind (liftI (IORef.tryRead ref)) <|
        \pInfo ->
            case pInfo of
                Just (Info _ _) ->
                    IO.return point

                Just (Link ((Pt ref1) as point1)) ->
                    IO.bind (repr point1) <|
                        \point2 ->
                            IO.bind
                                (IO.when (point2 /= point1) <|
                                    \() ->
                                        IO.bind (liftI (IORef.tryRead ref1)) <|
                                            \pInfo1 ->
                                                case pInfo1 of
                                                    Just i ->
                                                        liftI (IORef.write ref i)

                                                    Nothing ->
                                                        IO.return ()
                                )
                            <|
                                \_ ->
                                    IO.return point2

                Nothing ->
                    IO.return point


get : Point a -> IO a t (Maybe a)
get ((Pt ref) as point) =
    IO.bind (liftI (IORef.tryRead ref)) <|
        \pInfo ->
            case pInfo of
                Just (Info _ descRef) ->
                    liftD (IORef.tryRead descRef)

                Just (Link (Pt ref1)) ->
                    IO.bind (liftI (IORef.tryRead ref1)) <|
                        \link ->
                            case link of
                                Just (Info _ descRef) ->
                                    liftD (IORef.tryRead descRef)

                                Just (Link _) ->
                                    IO.andThen get <| repr point

                                Nothing ->
                                    IO.return Nothing

                Nothing ->
                    IO.return Nothing


set : Point a -> a -> IO a t ()
set ((Pt ref) as point) newDesc =
    IO.bind (liftI (IORef.tryRead ref)) <|
        \pInfo ->
            case pInfo of
                Just (Info _ descRef) ->
                    liftD (IORef.write descRef newDesc)

                Just (Link (Pt ref1)) ->
                    IO.bind (liftI (IORef.tryRead ref1)) <|
                        \link ->
                            case link of
                                Just (Info _ descRef) ->
                                    liftD (IORef.write descRef newDesc)

                                Just (Link _) ->
                                    IO.bind (repr point) <|
                                        \newPoint ->
                                            set newPoint newDesc

                                Nothing ->
                                    IO.return ()

                Nothing ->
                    IO.return ()


modify : Point a -> (a -> a) -> IO a t ()
modify ((Pt ref) as point) func =
    IO.bind (liftI (IORef.tryRead ref)) <|
        \pInfo ->
            case pInfo of
                Just (Info _ descRef) ->
                    liftD (IORef.modify descRef func)

                Just (Link (Pt ref1)) ->
                    IO.bind (liftI (IORef.tryRead ref1)) <|
                        \link ->
                            case link of
                                Just (Info _ descRef) ->
                                    liftD (IORef.modify descRef func)

                                Just (Link _) ->
                                    IO.bind (repr point) <|
                                        \newPoint ->
                                            modify newPoint func

                                Nothing ->
                                    IO.return ()

                Nothing ->
                    IO.return ()


union : Point a -> Point a -> a -> IO a t ()
union p1 p2 newDesc =
    IO.bind (repr p1) <|
        \((Pt ref1) as point1) ->
            IO.bind (repr p2) <|
                \((Pt ref2) as point2) ->
                    IO.bind (liftI (IORef.tryRead ref1)) <|
                        \info1 ->
                            IO.bind (liftI (IORef.tryRead ref2)) <|
                                \info2 ->
                                    case ( info1, info2 ) of
                                        ( Just (Info w1 d1), Just (Info w2 d2) ) ->
                                            if point1 == point2 then
                                                liftD (IORef.write d1 newDesc)

                                            else
                                                IO.bind (liftW (IORef.tryRead w1)) <|
                                                    \maybeWeight1 ->
                                                        IO.bind (liftW (IORef.tryRead w2)) <|
                                                            \maybeWeight2 ->
                                                                case ( maybeWeight1, maybeWeight2 ) of
                                                                    ( Just weight1, Just weight2 ) ->
                                                                        let
                                                                            newWeight =
                                                                                weight1 + weight2
                                                                        in
                                                                        if weight1 >= weight2 then
                                                                            IO.bind (liftI (IORef.write ref2 (Link point1))) <|
                                                                                \() ->
                                                                                    IO.bind (liftW (IORef.write w1 newWeight)) <|
                                                                                        \() ->
                                                                                            liftD (IORef.write d1 newDesc)

                                                                        else
                                                                            IO.bind (liftI (IORef.write ref1 (Link point2))) <|
                                                                                \() ->
                                                                                    IO.bind (liftW (IORef.write w2 newWeight)) <|
                                                                                        \() ->
                                                                                            liftD (IORef.write d2 newDesc)

                                                                    _ ->
                                                                        doNothing

                                        _ ->
                                            doNothing



-- TODO "UnionFind.union: invalid PointInfos"


equivalent : Point a -> Point a -> IO a t Bool
equivalent p1 p2 =
    IO.bind (repr p1) <|
        \v1 ->
            IO.bind (repr p2) <|
                \v2 ->
                    IO.return (v1 == v2)


redundant : Point a -> IO a t Bool
redundant (Pt ref) =
    IO.bind (liftI (IORef.tryRead ref)) <|
        \pInfo ->
            case pInfo of
                Just (Info _ _) ->
                    IO.return False

                Just (Link _) ->
                    IO.return True

                Nothing ->
                    IO.return True
