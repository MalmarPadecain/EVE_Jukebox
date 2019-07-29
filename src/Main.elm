port module Main exposing (control, main, progress, update)

import Array exposing (Array)
import Browser
import Core exposing (..)
import Draggable
import Http
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
    Success
        { playlist =
            { index = 0
            , progress = 0
            , shuffledSongs = Nothing
            , tmpCurrentSong = Nothing
            , playing = False
            , name = "Empty"
            , songs = Array.empty
            }
        , volume = 100
        , dragState =
            { position = ( 0, 0 )
            , drag = Draggable.init
            }
        }
        |> update (Load "./playlists/EVE_Soundtrack.json")


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model_ =
    case model_ of
        Success ({ playlist, dragState } as model) ->
            case msg of
                Next ->
                    update Play (Success { model | playlist = next model.playlist })

                Previous ->
                    if playlist.progress < 5 then
                        update Play (Success { model | playlist = previous model.playlist })

                    else
                        update Play model_

                Shuffle ->
                    if playlist.shuffledSongs == Nothing then
                        ( Success model, generate Shuffled (shuffle playlist.songs) )

                    else
                        ( Success { model | playlist = { playlist | shuffledSongs = Nothing } }, Cmd.none )

                Shuffled shuffledList ->
                    ( Success
                        { model
                            | playlist =
                                { playlist
                                    | index = 0
                                    , shuffledSongs = Just shuffledList
                                    , tmpCurrentSong =
                                        if playlist.tmpCurrentSong == Nothing then
                                            Just <| currentSong playlist

                                        else
                                            playlist.tmpCurrentSong
                                }
                        }
                    , Cmd.none
                    )

                Load link ->
                    ( Success model, loadJson link )

                Loaded result ->
                    case result of
                        Ok pl ->
                            ( Success { model | playlist = pl }
                            , control ("load " ++ (currentSong pl).link)
                            )

                        Err err ->
                            ( Error <| errorToString err, Cmd.none )

                ChooseSong songIndex ->
                    update Play <| Success { model | playlist = chooseSong model.playlist songIndex }

                ChangeVolume vol ->
                    ( Success { model | volume = vol }
                    , control ("volume " ++ String.fromFloat vol)
                    )

                Play ->
                    ( Success { model | playlist = { playlist | playing = True } }
                    , control ("play " ++ (currentSong playlist).link)
                    )

                TogglePause ->
                    ( Success { model | playlist = { playlist | playing = not playlist.playing } }
                    , control "togglePause"
                    )

                OnDragBy ( dx, dy ) ->
                    ( Success
                        { model
                            | dragState =
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
                            Draggable.update dragConfig dragMsg model.dragState
                    in
                    ( Success
                        { model
                            | dragState = { position = model.dragState.position, drag = newDragState.drag }
                        }
                    , m
                    )

                Progress val ->
                    ( Success
                        { model | playlist = { playlist | progress = val } }
                    , Cmd.none
                    )

        Error _ ->
            ( model_, Cmd.none )


loadJson : String -> Cmd Msg
loadJson link =
    Http.get
        { url = link
        , expect = Http.expectJson Loaded playlistDecoder
        }


errorToString : Http.Error -> String
errorToString e =
    --    not sure if this is actually needed.
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
