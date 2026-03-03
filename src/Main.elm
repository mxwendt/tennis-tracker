module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, details, div, summary, table, td, text, tr)
import Html.Attributes exposing (attribute, class, disabled)
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
    { match : Match
    , history : List Match
    }


initialMatch : Match
initialMatch =
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


init : Model
init =
    { match = initialMatch
    , history = []
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


{-| Push the current match onto the history stack and apply a new match state.
-}
withHistory : Model -> Match -> Model
withHistory model newMatch =
    { match = newMatch
    , history = model.match :: model.history
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        StartMatch ->
            init

        Player1Scored ->
            withHistory model (updatePlayerScored Player1 model.match)

        Player2Scored ->
            withHistory model (updatePlayerScored Player2 model.match)

        Player1Fault ->
            withHistory model (updatePlayerScored Player2 model.match)

        Player2Fault ->
            withHistory model (updatePlayerScored Player1 model.match)

        Player1UnforcedError ->
            withHistory model (updatePlayerScored Player2 model.match)

        Player2UnforcedError ->
            withHistory model (updatePlayerScored Player1 model.match)

        Undo ->
            case model.history of
                [] ->
                    model

                previous :: rest ->
                    { match = previous, history = rest }


updatePlayerScored : Player -> Match -> Match
updatePlayerScored player match =
    let
        ( p1, p2 ) =
            match.score

        ( newScore, newAdvantage, isGameWon ) =
            updateScore player ( p1, p2 ) match.advantage
    in
    if isGameWon then
        let
            newSetScore =
                case match.currentSet of
                    1 ->
                        updateSet player match.firstSet

                    2 ->
                        updateSet player match.secondSet

                    3 ->
                        updateSet player match.thirdSet

                    _ ->
                        ( 0, 0 )

            isSetWon =
                checkSetWon newSetScore

            setsWon =
                getSetsWon match player

            updatedMatch =
                case match.currentSet of
                    1 ->
                        { match | firstSet = newSetScore }

                    2 ->
                        { match | secondSet = newSetScore }

                    3 ->
                        { match | thirdSet = newSetScore }

                    _ ->
                        match
        in
        if isSetWon then
            if setsWon + 1 >= 2 then
                -- Match won
                { updatedMatch
                    | score = ( 0, 0 )
                    , isDeuce = False
                    , isServing = switchServer match.isServing
                    , advantage = Nothing
                    , matchStatus = Finished player
                }

            else
                -- Set won, move to next set
                { updatedMatch
                    | score = ( 0, 0 )
                    , isDeuce = False
                    , isServing = switchServer match.isServing
                    , advantage = Nothing
                    , currentSet = match.currentSet + 1
                }

        else
            -- Game won, continue in current set
            { updatedMatch
                | score = ( 0, 0 )
                , isDeuce = False
                , isServing = switchServer match.isServing
                , advantage = Nothing
            }

    else
        { match
            | score = newScore
            , advantage = newAdvantage
            , isDeuce = isDeuce newScore
        }


getSetsWon : Match -> Player -> Int
getSetsWon match player =
    let
        countSet setScore =
            if checkSetWon setScore then
                case player of
                    Player1 ->
                        if Tuple.first setScore > Tuple.second setScore then
                            1

                        else
                            0

                    Player2 ->
                        if Tuple.second setScore > Tuple.first setScore then
                            1

                        else
                            0

            else
                0
    in
    countSet match.firstSet + countSet match.secondSet + countSet match.thirdSet


checkSetWon : ( Int, Int ) -> Bool
checkSetWon ( s1, s2 ) =
    -- A set is won when a player reaches 6 games with at least 2 games lead,
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

            else
                ( ( 15, p2 ), advantage, False )

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

            else
                ( ( p1, 15 ), advantage, False )


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
    viewMatch model


viewMatch : Model -> Html Msg
viewMatch model =
    let
        match =
            model.match

        canUndo =
            not (List.isEmpty model.history)

        isInProgress =
            match.matchStatus == InProgress
    in
    div [ class "mx-auto max-w-md p-4 bg-blue-900" ]
        [ case match.matchStatus of
            Finished winner ->
                div [ class "mb-4 p-4 bg-green-600 text-white text-center text-xl font-bold" ]
                    [ text ("🏆 " ++ toName winner match.isServing ++ " wins the match! 🏆") ]

            InProgress ->
                text ""
        , div [ class "grid grid-cols-2 gap-4" ]
            [ div [ class "text-center text-xl font-bold uppercase text-white" ] [ text (toName (Tuple.first match.players) match.isServing) ]
            , div [ class "text-center text-xl font-bold uppercase text-white" ] [ text (toName (Tuple.second match.players) match.isServing) ]
            , viewScorePlayer1 match
            , viewScorePlayer2 match
            , div [ class "flex mx-auto border mb-8" ]
                [ div [ class "p-2" ] [ text (String.fromInt (Tuple.first match.firstSet)) ]
                , div [ class "p-2 border-l border-gray-300 border-dotted" ] [ text (String.fromInt (Tuple.first match.secondSet)) ]
                , div [ class "p-2 border-l border-gray-300 border-dotted" ] [ text (String.fromInt (Tuple.first match.thirdSet)) ]
                ]
            , div [ class "flex mx-auto border mb-8" ]
                [ div [ class "p-2" ] [ text (String.fromInt (Tuple.second match.firstSet)) ]
                , div [ class "p-2 border-l border-gray-300 border-dotted" ] [ text (String.fromInt (Tuple.second match.secondSet)) ]
                , div [ class "p-2 border-l border-gray-300 border-dotted" ] [ text (String.fromInt (Tuple.second match.thirdSet)) ]
                ]
            , viewButton "Winner" Player1Scored isInProgress
            , viewButton "Winner" Player2Scored isInProgress
            , viewButton "Fault" Player1Fault isInProgress
            , viewButton "Fault" Player2Fault isInProgress
            , viewButton "Unforced Error" Player1UnforcedError isInProgress
            , viewButton "Unforced Error" Player2UnforcedError isInProgress
            ]
        , button
            [ onClick Undo
            , disabled (not canUndo)
            , class "mt-4 w-full py-2 px-4 border text-center text-black bg-blue-500"
            , class
                (if canUndo then
                    "cursor-pointer opacity-100"

                 else
                    "cursor-not-allowed opacity-40"
                )
            ]
            [ text
                ("↩ Undo"
                    ++ (if canUndo then
                            " (" ++ String.fromInt (List.length model.history) ++ ")"

                        else
                            ""
                       )
                )
            ]
        , button [ onClick StartMatch, class "mt-2 w-full cursor-pointer py-2 px-4 border text-center text-white bg-green-600" ] [ text "New Match" ]
        , div [ class "mt-4 w-full text-center" ]
            [ viewSummary match "Set 1 Summary"
            , viewSummary match "Set 2 Summary"
            , viewSummary match "Match Summary"
            ]
        ]


viewScorePlayer1 : Match -> Html Msg
viewScorePlayer1 match =
    if isDeuce match.score && match.advantage == Just Player1 then
        div [ class "text-center text-xl font-bold" ] [ text "AD" ]

    else if isDeuce match.score && match.advantage == Just Player2 then
        div [ class "text-center text-xl font-bold" ] [ text "" ]

    else
        div [ class "text-center text-xl font-bold" ]
            [ text (String.fromInt (Tuple.first match.score)) ]


viewScorePlayer2 : Match -> Html Msg
viewScorePlayer2 match =
    if isDeuce match.score && match.advantage == Just Player1 then
        div [ class "text-center text-xl font-bold" ] [ text "" ]

    else if isDeuce match.score && match.advantage == Just Player2 then
        div [ class "text-center text-xl font-bold" ] [ text "AD" ]

    else
        div [ class "text-center text-xl font-bold" ]
            [ text (String.fromInt (Tuple.second match.score)) ]


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


viewSummary : Match -> String -> Html Msg
viewSummary match label =
    details [ class "w-full", attribute "open" "open" ]
        [ summary [ class "text-left p-4 border-b border-gray-300 cursor-pointer" ] [ text label ]
        , table [ class "w-full border border-t-0" ]
            [ viewSummaryRow "1st Serve %" (String.fromInt (Tuple.first match.score)) (String.fromInt (Tuple.second match.score))
            , viewSummaryRow "1st Serve Pts Won %" (String.fromInt (Tuple.first match.score)) (String.fromInt (Tuple.second match.score))
            , viewSummaryRow "2nd Serve Pts Won %" (String.fromInt (Tuple.first match.score)) (String.fromInt (Tuple.second match.score))
            , viewSummaryRow "Winners" (String.fromInt (Tuple.first match.score)) (String.fromInt (Tuple.second match.score))
            , viewSummaryRow "Unforced Errors" (String.fromInt (Tuple.first match.score)) (String.fromInt (Tuple.second match.score))
            , viewSummaryRow "Break Points Won" (String.fromInt (Tuple.first match.score)) (String.fromInt (Tuple.second match.score))
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
