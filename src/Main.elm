module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, details, div, p, summary, table, td, text, tr)
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


type alias SetStats =
    { winners : ( Int, Int )
    , unforcedErrors : ( Int, Int )
    , breakPointsWon : ( Int, Int )
    , breakPointsPlayed : ( Int, Int )
    }


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
    , set1Stats : SetStats
    , set2Stats : SetStats
    , set3Stats : SetStats
    }


type alias Model =
    { match : Match
    , history : List Match
    , confirmingNewMatch : Bool
    }


initialSetStats : SetStats
initialSetStats =
    { winners = ( 0, 0 )
    , unforcedErrors = ( 0, 0 )
    , breakPointsWon = ( 0, 0 )
    , breakPointsPlayed = ( 0, 0 )
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
    , set1Stats = initialSetStats
    , set2Stats = initialSetStats
    , set3Stats = initialSetStats
    }


init : Model
init =
    { match = initialMatch
    , history = []
    , confirmingNewMatch = False
    }



-- UPDATE


type Msg
    = RequestNewMatch
    | ConfirmNewMatch
    | CancelNewMatch
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
    , confirmingNewMatch = False
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        RequestNewMatch ->
            { model | confirmingNewMatch = True }

        ConfirmNewMatch ->
            init

        CancelNewMatch ->
            { model | confirmingNewMatch = False }

        Player1Scored ->
            let
                currentSet =
                    model.match.currentSet

                originalMatch =
                    model.match

                newMatch =
                    originalMatch
                        |> updatePlayerScored Player1
                        |> addWinner Player1 currentSet
                        |> trackBreakPoints Player1 originalMatch
            in
            withHistory model newMatch

        Player2Scored ->
            let
                currentSet =
                    model.match.currentSet

                originalMatch =
                    model.match

                newMatch =
                    originalMatch
                        |> updatePlayerScored Player2
                        |> addWinner Player2 currentSet
                        |> trackBreakPoints Player2 originalMatch
            in
            withHistory model newMatch

        Player1Fault ->
            let
                originalMatch =
                    model.match

                newMatch =
                    originalMatch
                        |> updatePlayerScored Player2
                        |> trackBreakPoints Player2 originalMatch
            in
            withHistory model newMatch

        Player2Fault ->
            let
                originalMatch =
                    model.match

                newMatch =
                    originalMatch
                        |> updatePlayerScored Player1
                        |> trackBreakPoints Player1 originalMatch
            in
            withHistory model newMatch

        Player1UnforcedError ->
            let
                currentSet =
                    model.match.currentSet

                originalMatch =
                    model.match

                newMatch =
                    originalMatch
                        |> updatePlayerScored Player2
                        |> addUnforcedError Player1 currentSet
                        |> trackBreakPoints Player2 originalMatch
            in
            withHistory model newMatch

        Player2UnforcedError ->
            let
                currentSet =
                    model.match.currentSet

                originalMatch =
                    model.match

                newMatch =
                    originalMatch
                        |> updatePlayerScored Player1
                        |> addUnforcedError Player2 currentSet
                        |> trackBreakPoints Player1 originalMatch
            in
            withHistory model newMatch

        Undo ->
            case model.history of
                [] ->
                    model

                previous :: rest ->
                    { match = previous, history = rest, confirmingNewMatch = False }


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


addWinnerToSetStats : Player -> SetStats -> SetStats
addWinnerToSetStats player stats =
    case player of
        Player1 ->
            { stats | winners = ( Tuple.first stats.winners + 1, Tuple.second stats.winners ) }

        Player2 ->
            { stats | winners = ( Tuple.first stats.winners, Tuple.second stats.winners + 1 ) }


addUnforcedErrorToSetStats : Player -> SetStats -> SetStats
addUnforcedErrorToSetStats player stats =
    case player of
        Player1 ->
            { stats | unforcedErrors = ( Tuple.first stats.unforcedErrors + 1, Tuple.second stats.unforcedErrors ) }

        Player2 ->
            { stats | unforcedErrors = ( Tuple.first stats.unforcedErrors, Tuple.second stats.unforcedErrors + 1 ) }


addBreakPointToSetStats : Player -> Bool -> SetStats -> SetStats
addBreakPointToSetStats player won stats =
    let
        updatedPlayed =
            case player of
                Player1 ->
                    ( Tuple.first stats.breakPointsPlayed + 1, Tuple.second stats.breakPointsPlayed )

                Player2 ->
                    ( Tuple.first stats.breakPointsPlayed, Tuple.second stats.breakPointsPlayed + 1 )

        updatedWon =
            if won then
                case player of
                    Player1 ->
                        ( Tuple.first stats.breakPointsWon + 1, Tuple.second stats.breakPointsWon )

                    Player2 ->
                        ( Tuple.first stats.breakPointsWon, Tuple.second stats.breakPointsWon + 1 )

            else
                stats.breakPointsWon
    in
    { stats | breakPointsPlayed = updatedPlayed, breakPointsWon = updatedWon }


addWinner : Player -> Int -> Match -> Match
addWinner player currentSet match =
    case currentSet of
        1 ->
            { match | set1Stats = addWinnerToSetStats player match.set1Stats }

        2 ->
            { match | set2Stats = addWinnerToSetStats player match.set2Stats }

        3 ->
            { match | set3Stats = addWinnerToSetStats player match.set3Stats }

        _ ->
            match


addUnforcedError : Player -> Int -> Match -> Match
addUnforcedError player currentSet match =
    case currentSet of
        1 ->
            { match | set1Stats = addUnforcedErrorToSetStats player match.set1Stats }

        2 ->
            { match | set2Stats = addUnforcedErrorToSetStats player match.set2Stats }

        3 ->
            { match | set3Stats = addUnforcedErrorToSetStats player match.set3Stats }

        _ ->
            match


{-| Given the player who scored, the match state _before_ the point was played,
and the match state _after_, detect any break-point situations and update the
appropriate set's stats.

Two cases are handled per point:

1.  The scoring player was in a break-point position (returner at game point)
    → they converted: increment their opportunities **and** wins.
2.  The _other_ player was in a break-point position (which they just lost)
    → increment only their opportunities (break point saved).

-}
trackBreakPoints : Player -> Match -> Match -> Match
trackBreakPoints scoringPlayer originalMatch resultMatch =
    let
        ( p1, p2 ) =
            originalMatch.score

        isBreakPointFor returner =
            case returner of
                Player1 ->
                    originalMatch.isServing
                        == Player2
                        && ((p1 == 40 && p2 < 40) || (isDeuce ( p1, p2 ) && originalMatch.advantage == Just Player1))

                Player2 ->
                    originalMatch.isServing
                        == Player1
                        && ((p2 == 40 && p1 < 40) || (isDeuce ( p1, p2 ) && originalMatch.advantage == Just Player2))

        otherPlayer =
            case scoringPlayer of
                Player1 ->
                    Player2

                Player2 ->
                    Player1

        hasBPForScoring =
            isBreakPointFor scoringPlayer

        hasBPForOther =
            isBreakPointFor otherPlayer

        currentSet =
            originalMatch.currentSet

        applyToCurrentSet updater match =
            case currentSet of
                1 ->
                    { match | set1Stats = updater match.set1Stats }

                2 ->
                    { match | set2Stats = updater match.set2Stats }

                3 ->
                    { match | set3Stats = updater match.set3Stats }

                _ ->
                    match
    in
    resultMatch
        |> (if hasBPForScoring then
                applyToCurrentSet (addBreakPointToSetStats scoringPlayer True)

            else
                identity
           )
        |> (if hasBPForOther then
                applyToCurrentSet (addBreakPointToSetStats otherPlayer False)

            else
                identity
           )


matchTotalStats : Match -> SetStats
matchTotalStats match =
    let
        sumTuples ( a1, b1 ) ( a2, b2 ) =
            ( a1 + a2, b1 + b2 )
    in
    { winners =
        sumTuples match.set1Stats.winners
            (sumTuples match.set2Stats.winners match.set3Stats.winners)
    , unforcedErrors =
        sumTuples match.set1Stats.unforcedErrors
            (sumTuples match.set2Stats.unforcedErrors match.set3Stats.unforcedErrors)
    , breakPointsWon =
        sumTuples match.set1Stats.breakPointsWon
            (sumTuples match.set2Stats.breakPointsWon match.set3Stats.breakPointsWon)
    , breakPointsPlayed =
        sumTuples match.set1Stats.breakPointsPlayed
            (sumTuples match.set2Stats.breakPointsPlayed match.set3Stats.breakPointsPlayed)
    }



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
        , button [ onClick RequestNewMatch, class "mt-2 w-full cursor-pointer py-2 px-4 border text-center text-white bg-green-600" ] [ text "New Match" ]
        , viewNewMatchDialog model.confirmingNewMatch
        , div [ class "mt-4 w-full text-center" ]
            [ viewSummary "Set 1 Summary" match.set1Stats False
            , viewSummary "Set 2 Summary" match.set2Stats False
            , viewSummary "Set 3 Summary" match.set3Stats False
            , viewSummary "Match Summary" (matchTotalStats match) True
            ]
        ]


viewNewMatchDialog : Bool -> Html Msg
viewNewMatchDialog isOpen =
    if not isOpen then
        text ""

    else
        div []
            [ {- Backdrop: clicking it dismisses the dialog -}
              div
                [ class "fixed inset-0 bg-black/60 z-10"
                , onClick CancelNewMatch
                ]
                []
            , Html.node "dialog"
                [ attribute "open" ""
                , class "fixed top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 z-20 m-0 w-80 rounded-lg border-none bg-white p-6 text-gray-900 shadow-2xl"
                ]
                [ p [ class "mb-1 text-base font-semibold" ]
                    [ text "Start a new match?" ]
                , p [ class "mb-6 text-sm text-gray-500" ]
                    [ text "All current progress and history will be lost." ]
                , div [ class "flex gap-3 justify-end" ]
                    [ button
                        [ onClick CancelNewMatch
                        , class "cursor-pointer rounded px-4 py-2 text-sm font-medium border border-gray-300 hover:bg-gray-100"
                        ]
                        [ text "Cancel" ]
                    , button
                        [ onClick ConfirmNewMatch
                        , class "cursor-pointer rounded px-4 py-2 text-sm font-medium bg-green-600 text-white hover:bg-green-700"
                        ]
                        [ text "Start New Match" ]
                    ]
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


viewSummary : String -> SetStats -> Bool -> Html Msg
viewSummary label stats open =
    let
        ( winners1, winners2 ) =
            stats.winners

        ( errors1, errors2 ) =
            stats.unforcedErrors

        ( bpWon1, bpWon2 ) =
            stats.breakPointsWon

        ( bpPlayed1, bpPlayed2 ) =
            stats.breakPointsPlayed

        formatBP won played =
            String.fromInt won ++ "/" ++ String.fromInt played
    in
    details
        (if open then
            [ class "w-full", attribute "open" "open" ]

         else
            [ class "w-full" ]
        )
        [ summary [ class "text-left p-4 border-b border-gray-300 cursor-pointer" ] [ text label ]
        , table [ class "w-full border border-t-0" ]
            [ viewSummaryRow "1st Serve %" "--" "--"
            , viewSummaryRow "1st Serve Pts Won %" "--" "--"
            , viewSummaryRow "2nd Serve Pts Won %" "--" "--"
            , viewSummaryRow "Winners" (String.fromInt winners1) (String.fromInt winners2)
            , viewSummaryRow "Unforced Errors" (String.fromInt errors1) (String.fromInt errors2)
            , viewSummaryRow "Break Points Won" (formatBP bpWon1 bpPlayed1) (formatBP bpWon2 bpPlayed2)
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
