port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    { count : Int }


type alias Flags =
    { count : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { count = flags.count }, Cmd.none )


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            let
                newCount =
                    model.count + 1
            in
            ( { model | count = newCount }, cache newCount )

        Decrement ->
            let
                newCount =
                    model.count - 1
            in
            ( { model | count = newCount }, cache newCount )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+1" ]
        , div [] [ text <| String.fromInt model.count ]
        , button [ onClick Decrement ] [ text "-1" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


port cache : Int -> Cmd msg


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
