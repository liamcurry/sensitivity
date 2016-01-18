module Sample where

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (type')
import Html.Events exposing (onClick)
import Utils


type alias Model =
    { startingInput : String
    , starting : Float
    , high : Float
    , low : Float
    , hasStarted : Bool
    }


type Action
    = NoOp
    | SetStartingInput String
    | Start
    | ChooseHigh
    | ChooseLow


initialModel : Model
initialModel =
    { startingInput = ""
    , starting = 0.0
    , high = 0.0
    , low = 0.0
    , hasStarted = False
    }


init : (Model, Effects Action)
init =
    ( initialModel
    , Effects.none
    )


update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        NoOp ->
            ( model, Effects.none )
        SetStartingInput startingInput ->
            ( { model | startingInput = startingInput }, Effects.none )
        Start ->
            let
                starting = Utils.parseFloat model.startingInput
                modifier = starting * 0.4
            in
                ( { model
                  | hasStarted = True
                  , starting = starting
                  , high = starting + modifier |> Utils.roundPlaces 2
                  , low = starting - modifier |> Utils.roundPlaces 2
                  }
                , Effects.none
                )
        ChooseHigh ->
            let
                diff = (model.high - model.low) * 0.75
            in
                ( { model | low = model.high - diff |> Utils.roundPlaces 2 }
                , Effects.none
                )
        ChooseLow ->
            let
                diff = (model.high - model.low) * 0.75
            in
                ( { model | high = model.low + diff |> Utils.roundPlaces 2 }
                , Effects.none
                )


startingForm : Signal.Address Action -> Model -> Html
startingForm address model =
    div
        [ ]
        [ label
            [ ]
            [ text "Choose your starting sensitivity: "
            , input
                [ type' "number"
                , Utils.onInput address SetStartingInput
                ]
                [ ]
            ]
        , button [ onClick address Start ] [ text "Start" ]
        ]


startedView : Signal.Address Action -> Model -> Html
startedView address model =
    div
        [ ]
        [ p [ ] [ text "Set your sensitivity to each value and choose which one feels better" ]
        , div
            [ ]
            [ h2 [ ] [ text "Low" ]
            , kbd [ ] [ model.low |> toString |> text ]
            , button [ onClick address ChooseLow ] [ text "Choose" ]
            ]
        , div
            [ ]
            [ h2 [ ] [ text "High" ]
            , kbd [ ] [ model.high |> toString |> text ]
            , button [ onClick address ChooseHigh ] [ text "Choose" ]
            ]
        ]


view : Signal.Address Action -> Model -> Html
view address model =
    let
        pageView = if model.hasStarted then startedView else startingForm
    in
        div []
            [ h1 [ ] [ text "Calibrate your sensitivity" ]
            , pageView address model
            ]
