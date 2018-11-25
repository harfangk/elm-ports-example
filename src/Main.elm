module Main exposing (main)

import Browser
import Html exposing (Html, br, button, div, form, input, label, p, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Json.Encode as JE
import Port



-- Model


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



-- Init


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



-- Update


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
    | GotLocalStorageItem { key : String, value : Maybe String }
    | GotPortMsg (Result JD.Error Port.InboundMethod)


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
                portMsg =
                    Port.SetLocalStorageItem { key = model.keyForSetItem, value = model.value }
                        |> Port.encode
            in
            ( model, Port.sendPortMsg portMsg )

        GetLocalStorageItem ->
            let
                portMsg =
                    Port.GetLocalStorageItem { key = model.keyForGetItem }
                        |> Port.encode
            in
            ( model, Port.sendPortMsg portMsg )

        ClearLocalStorage ->
            let
                portMsg =
                    Port.ClearLocalStorage |> Port.encode
            in
            ( model, Port.sendPortMsg portMsg )

        RemoveLocalStorageItem ->
            let
                portMsg =
                    Port.RemoveLocalStorageItem { key = model.keyForRemoveItem }
                        |> Port.encode
            in
            ( model, Port.sendPortMsg portMsg )

        GetCookies ->
            ( model, Port.sendPortMsg (Port.encode Port.GetCookies) )

        SetCookie ->
            let
                portMsg =
                    Port.SetCookie { key = model.keyForSetItem, value = model.value }
                        |> Port.encode
            in
            ( model, Port.sendPortMsg portMsg )

        GotLocalStorageItem { key, value } ->
            ( { model | receivedItem = key ++ "=" ++ Maybe.withDefault "null" value }, Cmd.none )

        GotCookies cookies ->
            ( { model | cookies = cookies }, Cmd.none )

        GotPortMsg result ->
            case result of
                Ok inboundMethod ->
                    update (inboundMethodToMsg inboundMethod) model

                Err jdError ->
                    ( model, Cmd.none )


inboundMethodToMsg : Port.InboundMethod -> Msg
inboundMethodToMsg inboundMethod =
    case inboundMethod of
        Port.GotLocalStorageItem kv ->
            GotLocalStorageItem kv

        Port.GotCookies cookies ->
            GotCookies cookies



-- View


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



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Port.gotPortMsg (JD.decodeValue Port.decoder >> GotPortMsg)



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
