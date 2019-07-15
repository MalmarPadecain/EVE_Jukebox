port module Main exposing (control, init, loadJson, main, songDecoder, update)

import Array exposing (Array)
import Browser
import Core exposing (..)
import Http
import Json.Decode exposing (Decoder, array, field, map2, map3, string)
import Random exposing (generate)
import Random.Array exposing (shuffle)
import View exposing (..)


port control : String -> Cmd msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( Success
        { name = "Empty"
        , songs = Array.empty
        , index = 0
        , volume = 100
        }
    , loadJson "./playlists/EVE_Soundtrack.json"
    )


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Success pl ->
            case msg of
                Next ->
                    update Play (next model)

                Previous ->
                    update Play (previous model)

                Shuffle ->
                    ( model, generate Shuffled (shuffle pl.songs) )

                Shuffled shuffledList ->
                    ( Success { pl | songs = shuffledList, index = 0 }, Cmd.batch [] )

                Load link ->
                    ( model, loadJson link )

                Loaded result ->
                    case result of
                        Ok playlist ->
                            ( Success playlist, Cmd.none )

                        Err err ->
                            ( Error <| errorToString err, Cmd.none )

                ChooseSong songIndex ->
                    update Play <| Success { pl | index = songIndex }

                ChangeVolume volume ->
                    ( Success { pl | volume = volume }, control ("volume " ++ String.fromFloat volume) )

                Play ->
                    ( model, control ("play " ++ (currentSong model).link) )

                TogglePause ->
                    ( model, control "togglePause" )

        Error _ ->
            ( model, Cmd.none )


loadJson : String -> Cmd Msg
loadJson link =
    Http.get
        { url = link
        , expect = Http.expectJson Loaded songDecoder
        }


songDecoder : Decoder Playlist
songDecoder =
    map2 (Playlist 0 100)
        (field "name" string)
        (field "songs"
            (array
                (map3 Song
                    (field "name" string)
                    (field "duration" string)
                    (field "link" string)
                )
            )
        )


errorToString : Http.Error -> String
errorToString e =
    case e of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out."

        Http.NetworkError ->
            "A Network Error occurred"

        Http.BadStatus status ->
            String.fromInt status ++ " returned"

        Http.BadBody body ->
            "Bad Body: " ++ body
