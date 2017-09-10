port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


type Msg
    = Click Int
    | Start
    | ToggleStrict
    | AddSound Int


type alias Model =
    { sounds : List Int
    , userSounds : List Int
    , isStrict : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] [] False, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "circle" ]
        [ div [ class "circle-inside" ]
            [ div [ class "top-left btn-1", onClick (Click 1) ] []
            , div [ class "top-right btn-2", onClick (Click 2) ] []
            , div [ class "bottom-left btn-3", onClick (Click 3) ] []
            , div [ class "bottom-right btn-4", onClick (Click 4) ] []
            , div [ class "x-line" ] []
            , div [ class "y-line" ] []
            , div [ class "circle-controls" ]
                [ div [ class "simon-text" ] [ text "Simon" ]
                , div [ class "score" ]
                    [ text (toString (List.length model.sounds)) ]
                , div
                    [ class "start-btn"
                    , onClick Start
                    ]
                    [ text "Start" ]
                , div
                    [ class "strict-btn"
                    , onClick ToggleStrict
                    ]
                    [ text "Strict" ]
                , if model.isStrict then
                    div [ class "strict-light" ] []
                  else
                    div [] []
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | sounds = [], userSounds = [] }, randomSound )

        ToggleStrict ->
            ( { model | isStrict = not model.isStrict }, Cmd.none )

        Click soundInt ->
            if model.sounds == [] then
                ( model, playSound soundInt )
            else
                let
                    userSounds =
                        model.userSounds ++ [ soundInt ]

                    expectedSounds =
                        List.take (List.length userSounds) model.sounds
                in
                    if userSounds == expectedSounds then
                        if userSounds == model.sounds then
                            ( { model | userSounds = [] }
                            , playSoundThenRandomInt soundInt
                            )
                        else
                            ( { model | userSounds = userSounds }
                            , playSound soundInt
                            )
                    else if model.isStrict then
                        ( { model | sounds = [], userSounds = [] }
                        , playErrorThenRandomInt ()
                        )
                    else
                        ( { model | userSounds = [] }
                        , playErrorThenSeries model.sounds
                        )

        AddSound int ->
            let
                sounds =
                    model.sounds ++ [ int ]
            in
                ( { model | sounds = sounds }, playSeries sounds )


subscriptions : Model -> Sub Msg
subscriptions model =
    getRandomSoundInt AddSound


randomSound : Cmd Msg
randomSound =
    Random.generate AddSound (Random.int 1 4)


port playSound : Int -> Cmd msg


port playSeries : List Int -> Cmd msg


port playErrorThenSeries : List Int -> Cmd msg


port playErrorThenRandomInt : () -> Cmd msg


port playSoundThenRandomInt : Int -> Cmd msg


port getRandomSoundInt : (Int -> msg) -> Sub msg


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
