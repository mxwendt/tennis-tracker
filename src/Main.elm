module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, details, div, span, summary, table, td, text, tr)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Player
    = Player1
    | Player2


type alias Match =
    { players : ( Player, Player )
    , score : ( Int, Int )
    , firstSet : ( Int, Int )
    , secondSet : ( Int, Int )
    , matchTieBreak : ( Int, Int )
    , isDeuce : Bool
    , isServing : Player
    , advantage : Maybe Player
    }


type alias Model =
    Match


init : Model
init =
    { players = ( Player1, Player2 )
    , score = ( 0, 0 )
    , firstSet = ( 0, 0 )
    , secondSet = ( 0, 0 )
    , matchTieBreak = ( 0, 0 )
    , isDeuce = False
    , isServing = Player1
    , advantage = Nothing
    }



-- UPDATE


type Msg
    = StartMatch
    | Player1Scored
    | Player2Scored
    | Player1Fault
    | Player2Fault
    | Player1UnforcedError
    | Player2UnforcedError
    | Undo


update : Msg -> Model -> Model
update msg model =
    case msg of
        StartMatch ->
            { model
                | score = ( 0, 0 )
                , firstSet = ( 0, 0 )
                , secondSet = ( 0, 0 )
                , matchTieBreak = ( 0, 0 )
                , isDeuce = False
                , isServing = Player1
                , advantage = Nothing
            }

        Player1Scored ->
            let
                ( p1, p2 ) =
                    model.score

                ( newScore, newAdvantage, isGameWon ) =
                    updateScore Player1 ( p1, p2 ) model.advantage
            in
            if isGameWon then
                { model
                    | score = ( 0, 0 )
                    , firstSet = updateSet Player1 model.firstSet
                    , isDeuce = False
                    , isServing = switchServer model.isServing
                    , advantage = Nothing
                }

            else
                { model
                    | score = newScore
                    , advantage = newAdvantage
                    , isDeuce = isDeuce newScore
                }

        Player2Scored ->
            let
                ( p1, p2 ) =
                    model.score

                ( newScore, newAdvantage, isGameWon ) =
                    updateScore Player2 ( p1, p2 ) model.advantage
            in
            if isGameWon then
                { model
                    | score = ( 0, 0 )
                    , firstSet = updateSet Player2 model.firstSet
                    , isDeuce = False
                    , isServing = switchServer model.isServing
                    , advantage = Nothing
                }

            else
                { model
                    | score = newScore
                    , advantage = newAdvantage
                    , isDeuce = isDeuce newScore
                }

        Player1Fault ->
            update Player2Scored model

        Player2Fault ->
            update Player1Scored model

        Player1UnforcedError ->
            update Player2Scored model

        Player2UnforcedError ->
            update Player1Scored model

        Undo ->
            { model
                | score = ( 0, 0 )
                , firstSet = updateSet Player2 model.firstSet
                , isDeuce = False
                , isServing = switchServer model.isServing
                , advantage = Nothing
            }


updateScore : Player -> ( Int, Int ) -> Maybe Player -> ( ( Int, Int ), Maybe Player, Bool )
updateScore player ( p1, p2 ) advantage =
    case player of
        Player1 ->
            if p1 == 40 && p2 < 40 then
                ( ( 0, 0 ), Nothing, True )

            else if isDeuce ( p1, p2 ) then
                if advantage == Just Player1 then
                    ( ( 0, 0 ), Nothing, True )

                else if advantage == Just Player2 then
                    ( ( 40, 40 ), Nothing, False )

                else
                    ( ( 40, 40 ), Just Player1, False )

            else if p1 == 30 then
                ( ( 40, p2 ), advantage, False )

            else if p1 == 15 then
                ( ( 30, p2 ), advantage, False )

            else if p1 == 0 then
                ( ( 15, p2 ), advantage, False )

            else
                ( ( p1, p2 ), advantage, False )

        Player2 ->
            if p2 == 40 && p1 < 40 then
                ( ( 0, 0 ), Nothing, True )

            else if isDeuce ( p1, p2 ) then
                if advantage == Just Player2 then
                    ( ( 0, 0 ), Nothing, True )

                else if advantage == Just Player1 then
                    ( ( 40, 40 ), Nothing, False )

                else
                    ( ( 40, 40 ), Just Player2, False )

            else if p2 == 30 then
                ( ( p1, 40 ), advantage, False )

            else if p2 == 15 then
                ( ( p1, 30 ), advantage, False )

            else if p2 == 0 then
                ( ( p1, 15 ), advantage, False )

            else
                ( ( p1, p2 ), advantage, False )


isDeuce : ( Int, Int ) -> Bool
isDeuce ( p1, p2 ) =
    p1 == 40 && p2 == 40


updateSet : Player -> ( Int, Int ) -> ( Int, Int )
updateSet player ( s1, s2 ) =
    case player of
        Player1 ->
            ( s1 + 1, s2 )

        Player2 ->
            ( s1, s2 + 1 )


switchServer : Player -> Player
switchServer player =
    case player of
        Player1 ->
            Player2

        Player2 ->
            Player1



-- VIEW


view : Model -> Html Msg
view model =
    viewMatch
        model


viewMatch : Model -> Html Msg
viewMatch model =
    div [ class "mx-auto max-w-md p-4 bg-blue-900" ]
        [ div [ class "grid grid-cols-2 gap-4" ]
            [ div [ class "text-center text-xl font-bold uppercase text-white" ] [ text (toName (Tuple.first model.players) model.isServing) ]
            , div [ class "text-center text-xl font-bold uppercase text-white" ] [ text (toName (Tuple.second model.players) model.isServing) ]
            , viewScorePlayer1 model
            , viewScorePlayer2 model
            , div [ class "flex mx-auto border mb-8" ]
                [ div [ class "p-2" ] [ text (String.fromInt (Tuple.first model.firstSet)) ]
                , div [ class "p-2 border-l border-gray-300 border-dotted" ] [ text (String.fromInt (Tuple.first model.secondSet)) ]
                , div [ class "p-2 border-l border-gray-300 border-dotted" ] [ text (String.fromInt (Tuple.first model.matchTieBreak)) ]
                ]
            , div [ class "flex mx-auto border mb-8" ]
                [ div [ class "p-2" ] [ text (String.fromInt (Tuple.second model.firstSet)) ]
                , div [ class "p-2 border-l border-gray-300 border-dotted" ] [ text (String.fromInt (Tuple.second model.secondSet)) ]
                , div [ class "p-2 border-l border-gray-300 border-dotted" ] [ text (String.fromInt (Tuple.second model.matchTieBreak)) ]
                ]
            , viewButton "Winner" Player1Scored
            , viewButton "Winner" Player2Scored
            , viewButton "Fault" Player1Fault
            , viewButton "Fault" Player2Fault
            , viewButton "Unforced Error" Player1UnforcedError
            , viewButton "Unforced Error" Player2UnforcedError
            ]
        , button [ onClick Undo, class "mt-4 w-full cursor-pointer py-2 px-4 border text-center text-black bg-blue-500" ] [ text "Undo" ]
        , div [ class "mt-4 w-full text-center" ]
            [ viewSummary model "Set 1 Summary"
            , viewSummary model "Set 2 Summary"
            , viewSummary model "Match Summary"
            ]
        ]


viewScorePlayer1 : Model -> Html Msg
viewScorePlayer1 model =
    if isDeuce model.score && model.advantage == Just Player1 then
        div [ class "text-center text-xl font-bold" ] [ text "AD" ]

    else if isDeuce model.score && model.advantage == Just Player2 then
        div [ class "text-center text-xl font-bold" ] [ text "" ]

    else
        div [ class "text-center text-xl font-bold" ]
            [ text (String.fromInt (Tuple.first model.score))
            ]


viewScorePlayer2 : Model -> Html Msg
viewScorePlayer2 model =
    if isDeuce model.score && model.advantage == Just Player1 then
        div [ class "text-center text-xl font-bold" ] [ text "" ]

    else if isDeuce model.score && model.advantage == Just Player2 then
        div [ class "text-center text-xl font-bold" ] [ text "AD" ]

    else
        div [ class "text-center text-xl font-bold" ]
            [ text (String.fromInt (Tuple.second model.score))
            ]


viewButton : String -> Msg -> Html Msg
viewButton label msg =
    button [ onClick msg, class "cursor-pointer py-2 px-4 border text-center" ] [ text label ]


viewSummary : Model -> String -> Html Msg
viewSummary model label =
    details [ class "w-full", attribute "open" "open" ]
        [ summary [ class "text-left p-4 border-b border-gray-300 cursor-pointer" ] [ text label ]
        , table [ class "w-full border border-t-0" ]
            [ viewSummaryRow "1st Serve %" (String.fromInt (Tuple.first model.score)) (String.fromInt (Tuple.second model.score))
            , viewSummaryRow "1st Serve Pts Won %" (String.fromInt (Tuple.first model.score)) (String.fromInt (Tuple.second model.score))
            , viewSummaryRow "2nd Serve Pts Won %" (String.fromInt (Tuple.first model.score)) (String.fromInt (Tuple.second model.score))
            , viewSummaryRow "Winners" (String.fromInt (Tuple.first model.score)) (String.fromInt (Tuple.second model.score))
            , viewSummaryRow "Unforced Errors" (String.fromInt (Tuple.first model.score)) (String.fromInt (Tuple.second model.score))
            , viewSummaryRow "Break Points Won" (String.fromInt (Tuple.first model.score)) (String.fromInt (Tuple.second model.score))
            ]
        ]


viewSummaryRow : String -> String -> String -> Html Msg
viewSummaryRow label value1 value2 =
    tr [ class "border-b border-dotted last:border-none border-gray-300" ]
        [ td [ class "p-2" ] [ text value1 ]
        , td [ class "p-2" ] [ text label ]
        , td [ class "p-2" ] [ text value2 ]
        ]


toName : Player -> Player -> String
toName player isServingPlayer =
    case player of
        Player1 ->
            if isServingPlayer == Player1 then
                "Player 1 🎾"

            else
                "Player 1"

        Player2 ->
            if isServingPlayer == Player2 then
                "Player 2 🎾"

            else
                "Player 2"
