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



-- Init


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Loading, initCmd )


initCmd : Cmd Msg
initCmd =
    Port.sendPortMsg (Port.encode Port.ConnectServer)


initPage : Page
initPage =
    Storage Page.Storage.initModel



-- Update


type Msg
    = ConnectedToServer
    | GotPortMsg (Result JD.Error Port.InboundMethod)
    | GotStorageMsg Page.Storage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPortMsg result ->
            case result of
                Ok inboundMethod ->
                    update (inboundMethodToMsg inboundMethod) model

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


inboundMethodToMsg : Port.InboundMethod -> Msg
inboundMethodToMsg inboundMethod =
    case inboundMethod of
        Port.GotLocalStorageItem kv ->
            kv |> Page.Storage.GotLocalStorageItem >> GotStorageMsg

        Port.GotCookies cookies ->
            cookies |> Page.Storage.GotCookies >> GotStorageMsg

        Port.ConnectedToServer ->
            ConnectedToServer



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
