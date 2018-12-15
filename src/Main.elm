module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Json.Decode as JD
import Json.Encode as JE
import Page.Storage
import Port



-- Model


type Model
    = Loading
    | Ready Page
    | Unavailable


type Page
    = Landing
    | Storage Page.Storage.Model
    | FAQ
    | NotFound


type alias Flags =
    String


type alias KV =
    { key : String, value : String }



-- Init


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Loading, initCmd )


initCmd : Cmd Msg
initCmd =
    let
        portMsg =
            JE.object
                [ ( "method", JE.string "connectToServer" ) ]
    in
    Port.sendPortMsg portMsg


initPage : Page
initPage =
    Storage Page.Storage.initModel



-- Update


type Msg
    = ConnectedToServer
    | GotPortMsg (Result JD.Error Msg)
    | GotStorageMsg Page.Storage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPortMsg result ->
            case result of
                Ok portMsg ->
                    update portMsg model

                Err jdError ->
                    ( model, Cmd.none )

        otherMsg ->
            case model of
                Loading ->
                    case otherMsg of
                        ConnectedToServer ->
                            ( Ready initPage, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Unavailable ->
                    ( model, Cmd.none )

                Ready page ->
                    case ( otherMsg, page ) of
                        ( GotStorageMsg storageMsg, Storage storageModel ) ->
                            Page.Storage.update storageMsg storageModel
                                |> Tuple.mapBoth (Storage >> Ready) (Cmd.map GotStorageMsg)

                        _ ->
                            ( model, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            text "Loading..."

        Unavailable ->
            text "Unavailable..."

        Ready page ->
            viewPage page


viewPage : Page -> Html Msg
viewPage page =
    case page of
        Landing ->
            text "Landing Page"

        FAQ ->
            text "FAQ Page"

        NotFound ->
            text "Page Not Found"

        Storage storageModel ->
            Page.Storage.view storageModel
                |> Html.map GotStorageMsg



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Port.gotPortMsg (JD.decodeValue portMsgDecoder >> GotPortMsg)



-- Port Decoders


portMsgDecoder : JD.Decoder Msg
portMsgDecoder =
    JD.field "method" JD.string
        |> JD.andThen portMsgDecoder_


portMsgDecoder_ : String -> JD.Decoder Msg
portMsgDecoder_ method =
    case method of
        "gotLocalStorageItem" ->
            JD.map (Page.Storage.GotLocalStorageItem >> GotStorageMsg) (JD.field "payload" kvDecoder)

        "gotCookies" ->
            JD.map (Page.Storage.GotCookies >> GotStorageMsg) (JD.field "payload" (JD.field "cookies" JD.string))

        "connectedToServer" ->
            JD.succeed ConnectedToServer

        _ ->
            JD.fail "Got unregistered method for port message"


kvDecoder : JD.Decoder KV
kvDecoder =
    JD.map2 KV
        (JD.field "key" JD.string)
        (JD.field "value" JD.string)



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
