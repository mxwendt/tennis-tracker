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


type MatchStatus
    = InProgress
    | Finished Player


type alias Match =
    { players : ( Player, Player )
    , score : ( Int, Int )
    , firstSet : ( Int, Int )
    , secondSet : ( Int, Int )
    , thirdSet : ( Int, Int )
    , isDeuce : Bool
    , isServing : Player
    , advantage : Maybe Player
    , matchStatus : MatchStatus
    , currentSet : Int
    }


type alias Model =
    Match


init : Model
init =
    { players = ( Player1, Player2 )
    , score = ( 0, 0 )
    , firstSet = ( 0, 0 )
    , secondSet = ( 0, 0 )
    , thirdSet = ( 0, 0 )
    , isDeuce = False
    , isServing = Player1
    , advantage = Nothing
    , matchStatus = InProgress
    , currentSet = 1
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
                , thirdSet = ( 0, 0 )
                , isDeuce = False
                , isServing = Player1
                , advantage = Nothing
                , matchStatus = InProgress
                , currentSet = 1
            }

        Player1Scored ->
            updatePlayerScored Player1 model

        Player2Scored ->
            updatePlayerScored Player2 model

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


updatePlayerScored : Player -> Model -> Model
updatePlayerScored player model =
    let
        ( p1, p2 ) =
            model.score

        ( newScore, newAdvantage, isGameWon ) =
            updateScore player ( p1, p2 ) model.advantage
    in
    if isGameWon then
        let
            newSetScore =
                case model.currentSet of
                    1 ->
                        updateSet player model.firstSet

                    2 ->
                        updateSet player model.secondSet

                    3 ->
                        updateSet player model.thirdSet

                    _ ->
                        ( 0, 0 )

            isSetWon =
                checkSetWon newSetScore

            setsWon =
                getSetsWon model player

            updatedModel =
                case model.currentSet of
                    1 ->
                        { model | firstSet = newSetScore }

                    2 ->
                        { model | secondSet = newSetScore }

                    3 ->
                        { model | thirdSet = newSetScore }

                    _ ->
                        model
        in
        if isSetWon then
            if setsWon + 1 >= 2 then
                -- Match won
                { updatedModel
                    | score = ( 0, 0 )
                    , isDeuce = False
                    , isServing = switchServer model.isServing
                    , advantage = Nothing
                    , matchStatus = Finished player
                }

            else
                -- Set won, move to next set
                { updatedModel
                    | score = ( 0, 0 )
                    , isDeuce = False
                    , isServing = switchServer model.isServing
                    , advantage = Nothing
                    , currentSet = model.currentSet + 1
                }

        else
            -- Game won, continue in current set
            { updatedModel
                | score = ( 0, 0 )
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


getSetsWon : Model -> Player -> Int
getSetsWon model player =
    let
        set1Won =
            if checkSetWon model.firstSet then
                case player of
                    Player1 ->
                        if Tuple.first model.firstSet > Tuple.second model.firstSet then
                            1

                        else
                            0

                    Player2 ->
                        if Tuple.second model.firstSet > Tuple.first model.firstSet then
                            1

                        else
                            0

            else
                0

        set2Won =
            if checkSetWon model.secondSet then
                case player of
                    Player1 ->
                        if Tuple.first model.secondSet > Tuple.second model.secondSet then
                            1

                        else
                            0

                    Player2 ->
                        if Tuple.second model.secondSet > Tuple.first model.secondSet then
                            1

                        else
                            0

            else
                0

        set3Won =
            if checkSetWon model.thirdSet then
                case player of
                    Player1 ->
                        if Tuple.first model.thirdSet > Tuple.second model.thirdSet then
                            1

                        else
                            0

                    Player2 ->
                        if Tuple.second model.thirdSet > Tuple.first model.thirdSet then
                            1

                        else
                            0

            else
                0
    in
    set1Won + set2Won + set3Won


checkSetWon : ( Int, Int ) -> Bool
checkSetWon ( s1, s2 ) =
    -- A set is won when a player reaches 6 games with at least 2 games lead
    -- or wins a tiebreak at 7-6
    (s1 >= 6 && s1 - s2 >= 2)
        || (s2 >= 6 && s2 - s1 >= 2)
        || (s1 == 7 && s2 == 6)
        || (s1 == 6 && s2 == 7)


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
        [ case model.matchStatus of
            Finished winner ->
                div [ class "mb-4 p-4 bg-green-600 text-white text-center text-xl font-bold" ]
                    [ text ("🏆 " ++ toName winner model.isServing ++ " wins the match! 🏆") ]

            InProgress ->
                text ""
        , div [ class "grid grid-cols-2 gap-4" ]
            [ div [ class "text-center text-xl font-bold uppercase text-white" ] [ text (toName (Tuple.first model.players) model.isServing) ]
            , div [ class "text-center text-xl font-bold uppercase text-white" ] [ text (toName (Tuple.second model.players) model.isServing) ]
            , viewScorePlayer1 model
            , viewScorePlayer2 model
            , div [ class "flex mx-auto border mb-8" ]
                [ div [ class "p-2" ] [ text (String.fromInt (Tuple.first model.firstSet)) ]
                , div [ class "p-2 border-l border-gray-300 border-dotted" ] [ text (String.fromInt (Tuple.first model.secondSet)) ]
                , div [ class "p-2 border-l border-gray-300 border-dotted" ] [ text (String.fromInt (Tuple.first model.thirdSet)) ]
                ]
            , div [ class "flex mx-auto border mb-8" ]
                [ div [ class "p-2" ] [ text (String.fromInt (Tuple.second model.firstSet)) ]
                , div [ class "p-2 border-l border-gray-300 border-dotted" ] [ text (String.fromInt (Tuple.second model.secondSet)) ]
                , div [ class "p-2 border-l border-gray-300 border-dotted" ] [ text (String.fromInt (Tuple.second model.thirdSet)) ]
                ]
            , viewButton "Winner" Player1Scored (model.matchStatus == InProgress)
            , viewButton "Winner" Player2Scored (model.matchStatus == InProgress)
            , viewButton "Fault" Player1Fault (model.matchStatus == InProgress)
            , viewButton "Fault" Player2Fault (model.matchStatus == InProgress)
            , viewButton "Unforced Error" Player1UnforcedError (model.matchStatus == InProgress)
            , viewButton "Unforced Error" Player2UnforcedError (model.matchStatus == InProgress)
            ]
        , button [ onClick Undo, class "mt-4 w-full cursor-pointer py-2 px-4 border text-center text-black bg-blue-500" ] [ text "Undo" ]
        , button [ onClick StartMatch, class "mt-2 w-full cursor-pointer py-2 px-4 border text-center text-white bg-green-600" ] [ text "New Match" ]
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


viewButton : String -> Msg -> Bool -> Html Msg
viewButton label msg enabled =
    button
        [ onClick msg
        , class "cursor-pointer py-2 px-4 border text-center"
        , class
            (if enabled then
                "opacity-100"

             else
                "opacity-50 cursor-not-allowed"
            )
        ]
        [ text label ]


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
