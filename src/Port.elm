port module Port exposing (InboundMethod(..), OutboundMethod(..), decoder, encode, gotPortMsg, sendPortMsg)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE



-- Port Functions


port sendPortMsg : JE.Value -> Cmd msg


port gotPortMsg : (JE.Value -> msg) -> Sub msg



-- Outbound Ports


type OutboundMethod
    = SetLocalStorageItem { key : String, value : String }
    | GetLocalStorageItem { key : String }
    | ClearLocalStorage
    | RemoveLocalStorageItem { key : String }
    | GetCookies
    | SetCookie { key : String, value : String }
    | ConnectServer


encode : OutboundMethod -> JE.Value
encode method =
    case method of
        SetLocalStorageItem { key, value } ->
            let
                payload =
                    JE.object
                        [ ( "key", JE.string key )
                        , ( "value", JE.string value )
                        ]
            in
            JE.object
                [ ( "method", JE.string "setLocalStorageItem" )
                , ( "payload", payload )
                ]

        GetLocalStorageItem { key } ->
            JE.object
                [ ( "method", JE.string "getLocalStorageItem" )
                , ( "payload", JE.string key )
                ]

        ClearLocalStorage ->
            JE.object
                [ ( "method", JE.string "clearLocalStorage" )
                ]

        RemoveLocalStorageItem { key } ->
            JE.object
                [ ( "method", JE.string "removeLocalStorageItem" )
                , ( "payload", JE.string key )
                ]

        GetCookies ->
            JE.object
                [ ( "method", JE.string "getCookies" ) ]

        SetCookie { key, value } ->
            let
                payload =
                    JE.object
                        [ ( "key", JE.string key )
                        , ( "value", JE.string value )
                        ]
            in
            JE.object
                [ ( "method", JE.string "setCookie" )
                , ( "payload", payload )
                ]

        ConnectServer ->
            JE.object
                [ ( "method", JE.string "connectToServer" ) ]



-- Inbound Ports


type InboundMethod
    = GotLocalStorageItem { key : String, value : Maybe String }
    | GotCookies String
    | ConnectedToServer


decoder : Decoder InboundMethod
decoder =
    JD.field "method" JD.string
        |> JD.andThen decoder_


decoder_ : String -> JD.Decoder InboundMethod
decoder_ method =
    case method of
        "gotLocalStorageItem" ->
            JD.map GotLocalStorageItem (JD.field "payload" kvDecoder)

        "gotCookies" ->
            JD.map GotCookies (JD.at [ "payload", "cookies" ] JD.string)

        "connectedToServer" ->
            JD.succeed ConnectedToServer

        _ ->
            JD.fail "Got unregistered inbound port method"


kvConstructor : String -> Maybe String -> { key : String, value : Maybe String }
kvConstructor key mbValue =
    { key = key, value = mbValue }


kvDecoder : JD.Decoder { key : String, value : Maybe String }
kvDecoder =
    JD.map2 kvConstructor
        (JD.field "key" JD.string)
        (JD.field "value" (JD.nullable JD.string))
