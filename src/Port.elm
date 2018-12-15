port module Port exposing (clearLocalStorage, connectToServer, connectedToServer, getCookies, getLocalStorageItem, gotCookies, gotLocalStorageItem, removeLocalStorageItem, setCookie, setLocalStorageItem)

import Json.Encode as JE



-- Local Storage


port setLocalStorageItem : { key : String, value : String } -> Cmd msg


port getLocalStorageItem : String -> Cmd msg


port gotLocalStorageItem : ({ key : String, value : Maybe String } -> msg) -> Sub msg


port removeLocalStorageItem : String -> Cmd msg


port clearLocalStorage : () -> Cmd msg



-- Cookies


port getCookies : () -> Cmd msg


port gotCookies : (String -> msg) -> Sub msg


port setCookie : { key : String, value : String } -> Cmd msg



-- Server Connection


port connectToServer : () -> Cmd msg


port connectedToServer : (() -> msg) -> Sub msg
