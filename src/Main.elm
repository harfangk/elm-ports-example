port module Main exposing (main)

import Browser
import Html exposing (Html, br, button, div, form, input, label, p, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Json.Encode as JE


type alias Model =
    { cookies : String
    , keyForSetItem : String
    , keyForGetItem : String
    , keyForRemoveItem : String
    , value : String
    , receivedItem : String
    }


type alias Flags =
    String


type alias KV =
    { key : String, value : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { cookies = ""
      , keyForSetItem = ""
      , keyForGetItem = ""
      , keyForRemoveItem = ""
      , value = ""
      , receivedItem = ""
      }
    , Cmd.none
    )


type Msg
    = EnteredSetItemKey String
    | EnteredGetItemKey String
    | EnteredRemoveItemKey String
    | EnteredValue String
    | SetLocalStorageItem
    | GetLocalStorageItem
    | RemoveLocalStorageItem
    | ClearLocalStorage
    | SetCookie
    | GetCookies
    | GotCookies String
    | GotLocalStorageItem KV
    | GotPortMsg (Result JD.Error Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredSetItemKey key ->
            ( { model | keyForSetItem = key }, Cmd.none )

        EnteredGetItemKey key ->
            ( { model | keyForGetItem = key }, Cmd.none )

        EnteredRemoveItemKey key ->
            ( { model | keyForRemoveItem = key }, Cmd.none )

        EnteredValue value ->
            ( { model | value = value }, Cmd.none )

        SetLocalStorageItem ->
            let
                payload =
                    JE.object
                        [ ( "key", JE.string model.keyForSetItem )
                        , ( "value", JE.string model.value )
                        ]

                portMsg =
                    JE.object
                        [ ( "method", JE.string "setLocalStorageItem" )
                        , ( "payload", payload )
                        ]
            in
            ( model, sendPortMsg portMsg )

        GetLocalStorageItem ->
            let
                portMsg =
                    JE.object
                        [ ( "method", JE.string "getLocalStorageItem" )
                        , ( "payload", JE.string model.keyForGetItem )
                        ]
            in
            ( model, sendPortMsg portMsg )

        ClearLocalStorage ->
            let
                portMsg =
                    JE.object
                        [ ( "method", JE.string "clearLocalStorage" )
                        ]
            in
            ( model, sendPortMsg portMsg )

        RemoveLocalStorageItem ->
            let
                portMsg =
                    JE.object
                        [ ( "method", JE.string "removeLocalStorageItem" )
                        , ( "payload", JE.string model.keyForRemoveItem )
                        ]
            in
            ( model, sendPortMsg portMsg )

        GetCookies ->
            let
                portMsg =
                    JE.object
                        [ ( "method", JE.string "getCookies" ) ]
            in
            ( model, sendPortMsg portMsg )

        SetCookie ->
            let
                payload =
                    JE.object
                        [ ( "key", JE.string model.keyForSetItem )
                        , ( "value", JE.string model.value )
                        ]

                portMsg =
                    JE.object
                        [ ( "method", JE.string "setCookie" )
                        , ( "payload", payload )
                        ]
            in
            ( model, sendPortMsg portMsg )

        GotLocalStorageItem { key, value } ->
            ( { model | receivedItem = key ++ "=" ++ value }, Cmd.none )

        GotCookies cookies ->
            ( { model | cookies = cookies }, Cmd.none )

        GotPortMsg result ->
            case result of
                Ok portMsg ->
                    update portMsg model

                Err jdError ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ form []
                [ label [] [ text "Key: ", input [ onInput EnteredSetItemKey, value model.keyForSetItem ] [] ]
                , label [] [ text "Value: ", input [ onInput EnteredValue, value model.value ] [] ]
                , button [ type_ "button", onClick SetLocalStorageItem ] [ text "Set Item" ]
                , button [ type_ "button", onClick SetCookie ] [ text "Set Cookie" ]
                ]
            ]
        , div []
            [ form []
                [ label [] [ text "Key: ", input [ onInput EnteredGetItemKey, value model.keyForGetItem ] [] ]
                , text <| "LocalStorage Item: " ++ model.receivedItem
                , br [] []
                , button [ type_ "button", onClick GetLocalStorageItem ] [ text "Get Item" ]
                ]
            ]
        , div []
            [ text <| "Cookies: " ++ model.cookies
            , br [] []
            , button [ type_ "button", onClick GetCookies ] [ text "Get Cookies" ]
            ]
        , div []
            [ form []
                [ label [] [ text "Key: ", input [ onInput EnteredRemoveItemKey, value model.keyForRemoveItem ] [] ]
                , button [ type_ "button", onClick RemoveLocalStorageItem ] [ text "Remove Item" ]
                , button [ type_ "button", onClick ClearLocalStorage ] [ text "Clear LocalStorage" ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    gotPortMsg (JD.decodeValue portMsgDecoder >> GotPortMsg)



-- Ports


port sendPortMsg : JE.Value -> Cmd msg


port gotPortMsg : (JE.Value -> msg) -> Sub msg



-- Port Decoders


portMsgDecoder : JD.Decoder Msg
portMsgDecoder =
    JD.field "method" JD.string
        |> JD.andThen portMsgDecoder_


portMsgDecoder_ : String -> JD.Decoder Msg
portMsgDecoder_ method =
    case method of
        "gotLocalStorageItem" ->
            JD.map GotLocalStorageItem (JD.field "payload" kvDecoder)

        "gotCookies" ->
            JD.map GotCookies (JD.field "payload" (JD.field "cookies" JD.string))

        _ ->
            JD.fail "Got unregistered method for port message"


kvDecoder : JD.Decoder KV
kvDecoder =
    JD.map2 KV
        (JD.field "key" JD.string)
        (JD.field "value" JD.string)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
