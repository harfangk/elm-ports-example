module Main exposing (main)

import Browser
import Html exposing (Html, text)
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
    ( Loading, Port.connectToServer () )


initPage : Page
initPage =
    Storage Page.Storage.initModel



-- Update


type Msg
    = ConnectedToServer ()
    | GotStorageMsg Page.Storage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading ->
            case msg of
                ConnectedToServer () ->
                    ( Ready initPage, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Unavailable ->
            ( model, Cmd.none )

        Ready page ->
            case ( msg, page ) of
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
    case model of
        Loading ->
            Port.connectedToServer ConnectedToServer

        Unavailable ->
            Port.connectedToServer ConnectedToServer

        Ready page ->
            case page of
                Storage storageModel ->
                    Sub.map GotStorageMsg (Page.Storage.subscriptions storageModel)

                _ ->
                    Sub.none



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
