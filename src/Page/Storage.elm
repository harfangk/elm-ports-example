module Page.Storage exposing (Model, Msg(..), initModel, update, view)

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



-- Init


initModel : Model
initModel =
    { cookies = ""
    , keyForSetItem = ""
    , keyForGetItem = ""
    , keyForRemoveItem = ""
    , value = ""
    , receivedItem = ""
    }



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
    | GotLocalStorageItem { key : String, value : String }


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
            ( model, Port.sendPortMsg portMsg )

        GetLocalStorageItem ->
            let
                portMsg =
                    JE.object
                        [ ( "method", JE.string "getLocalStorageItem" )
                        , ( "payload", JE.string model.keyForGetItem )
                        ]
            in
            ( model, Port.sendPortMsg portMsg )

        ClearLocalStorage ->
            let
                portMsg =
                    JE.object
                        [ ( "method", JE.string "clearLocalStorage" )
                        ]
            in
            ( model, Port.sendPortMsg portMsg )

        RemoveLocalStorageItem ->
            let
                portMsg =
                    JE.object
                        [ ( "method", JE.string "removeLocalStorageItem" )
                        , ( "payload", JE.string model.keyForRemoveItem )
                        ]
            in
            ( model, Port.sendPortMsg portMsg )

        GetCookies ->
            let
                portMsg =
                    JE.object
                        [ ( "method", JE.string "getCookies" ) ]
            in
            ( model, Port.sendPortMsg portMsg )

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
            ( model, Port.sendPortMsg portMsg )

        GotLocalStorageItem { key, value } ->
            ( { model | receivedItem = key ++ "=" ++ value }, Cmd.none )

        GotCookies cookies ->
            ( { model | cookies = cookies }, Cmd.none )



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
