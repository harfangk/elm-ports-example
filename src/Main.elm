port module Main exposing (main)

import Browser
import Html exposing (Html, br, button, div, form, input, label, p, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)


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
            ( model, setLocalStorageItem { key = model.keyForSetItem, value = model.value } )

        GetLocalStorageItem ->
            ( model, getLocalStorageItem model.keyForGetItem )

        ClearLocalStorage ->
            ( model, clearLocalStorage () )

        RemoveLocalStorageItem ->
            ( model, removeLocalStorageItem model.keyForRemoveItem )

        GetCookies ->
            ( model, getCookies () )

        SetCookie ->
            ( model, setCookie { key = model.keyForSetItem, value = model.value } )

        GotLocalStorageItem { key, value } ->
            ( { model | receivedItem = key ++ "=" ++ value }, Cmd.none )

        GotCookies cookies ->
            ( { model | cookies = cookies }, Cmd.none )


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
    Sub.batch
        [ gotLocalStorageItem GotLocalStorageItem
        , gotCookies GotCookies
        ]


port setLocalStorageItem : { key : String, value : String } -> Cmd msg


port getLocalStorageItem : String -> Cmd msg


port gotLocalStorageItem : ({ key : String, value : String } -> msg) -> Sub msg


port removeLocalStorageItem : String -> Cmd msg


port clearLocalStorage : () -> Cmd msg


port getCookies : () -> Cmd msg


port gotCookies : (String -> msg) -> Sub msg


port setCookie : { key : String, value : String } -> Cmd msg


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
