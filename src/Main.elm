port module Main exposing (control, main, progress, update)

import Array exposing (Array)
import Browser
import Core exposing (..)
import Draggable
import Http
import Json.Decode exposing (Decoder, array, field, map2, map3, string)
import Platform.Sub as Sub
import Random exposing (generate)
import Random.Array exposing (shuffle)
import View exposing (..)


port control : String -> Cmd msg


port progress : (Float -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Success { dragState } ->
            Sub.batch
                [ Draggable.subscriptions DragMsg dragState.drag
                , progress Progress
                ]

        Error _ ->
            Sub.none


dragConfig : Draggable.Config String Msg
dragConfig =
    Draggable.basicConfig OnDragBy


init : () -> ( Model, Cmd Msg )
init _ =
    ( Success
        { playlist =
            { index = 0
            , progress = 0
            , shuffled = False
            , name = "Empty"
            , songs = Array.empty
            }
        , volume = 100
        , dragState =
            { position = ( 0, 0 )
            , drag = Draggable.init
            }
        }
    , loadJson "./playlists/EVE_Soundtrack.json"
    )


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Success { playlist, volume, dragState } ->
            case msg of
                Next ->
                    update Play (next model)

                Previous ->
                    update Play (previous model)

                Shuffle ->
                    ( model, generate Shuffled (shuffle playlist.songs) )

                Shuffled shuffledList ->
                    ( Success
                        { playlist = { playlist | songs = shuffledList, index = 0, shuffled = True }
                        , volume = volume
                        , dragState = dragState
                        }
                    , Cmd.batch []
                    )

                Load link ->
                    ( model, loadJson link )

                Loaded result ->
                    case result of
                        Ok pl ->
                            ( Success { playlist = pl, volume = volume, dragState = dragState }, Cmd.none )

                        Err err ->
                            ( Error <| errorToString err, Cmd.none )

                ChooseSong songIndex ->
                    update Play <|
                        Success
                            { playlist = { playlist | index = songIndex }
                            , volume = volume
                            , dragState = dragState
                            }

                ChangeVolume vol ->
                    ( Success { playlist = playlist, volume = vol, dragState = dragState }
                    , control ("volume " ++ String.fromFloat vol)
                    )

                Play ->
                    ( model, control ("play " ++ (currentSong model).link) )

                TogglePause ->
                    ( model, control "togglePause" )

                OnDragBy ( dx, dy ) ->
                    ( Success
                        { playlist = playlist
                        , volume = volume
                        , dragState =
                            { dragState
                                | position =
                                    ( round (toFloat (Tuple.first dragState.position) + dx)
                                    , round (toFloat (Tuple.second dragState.position) + dy)
                                    )
                            }
                        }
                    , Cmd.none
                    )

                DragMsg dragMsg ->
                    let
                        ( newDragState, m ) =
                            Draggable.update dragConfig dragMsg dragState
                    in
                    ( Success
                        { playlist = playlist
                        , volume = volume
                        , dragState = { position = dragState.position, drag = newDragState.drag }
                        }
                    , m
                    )

                Progress val ->
                    ( Success
                        { playlist = { playlist | progress = val }
                        , volume = volume
                        , dragState = dragState
                        }
                    , Cmd.none
                    )

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
    map2 (Playlist 0 0 False)
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
