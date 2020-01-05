port module Main exposing (control, main, progress, update)

import Browser
import Core exposing (..)
import Draggable
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Platform.Sub as Sub
import Playlist exposing (..)
import Random exposing (generate)
import Random.List exposing (shuffle)
import Task exposing (Task)
import View exposing (..)


port control : Encode.Value -> Cmd msg


port progress : (Float -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Ok { dragState } ->
            Sub.batch
                [ Draggable.subscriptions DragMsg dragState.drag
                , progress Progress
                ]

        Err _ ->
            Sub.none


dragConfig : Draggable.Config String Msg
dragConfig =
    Draggable.basicConfig OnDragBy


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialBackground =
            "./video/gallenteStill.png"
    in
    ( Ok
        { playlist =
            { progress = 0
            , playing = False
            , core =
                { name = "Empty"
                , link = ""
                }
            , order =
                { current = { index = 0, name = "", duration = "", link = "" }
                , previous = []
                , next = []
                }
            , displayOrder = [ { index = 0, name = "", duration = "", link = "" } ]
            , orderedBy = ( Number, Asc )
            }
        , volume = 100
        , dragState =
            { position = ( 200, 100 )
            , drag = Draggable.init
            }
        , shuffled = False
        , playlistList = []
        , selectedBackground = initialBackground
        , appliedBackground = initialBackground
        }
    , Cmd.batch
        [ Task.attempt PlaylistsLoaded loadPlaylistList
        , control <|
            Encode.object
                [ ( "message", Encode.string "background" )
                , ( "payload", Encode.string initialBackground )
                ]
        ]
    )


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
        Ok ({ playlist, dragState } as model) ->
            case msg of
                PlaylistsLoaded result ->
                    case result of
                        Ok ((x :: _) as list) ->
                            ( Ok { model | playlistList = list }, Task.attempt PlaylistLoaded (loadPlaylist x) )

                        Ok [] ->
                            ( Err "No playlists found.", Cmd.none )

                        Err err ->
                            ( Err <| errorToString err, Cmd.none )

                Next ->
                    let
                        newPlaylist =
                            next model.playlist

                        message =
                            if newPlaylist.playing then
                                "play"

                            else
                                "load"
                    in
                    ( Ok
                        { model
                            | playlist = newPlaylist
                        }
                    , control <|
                        Encode.object
                            [ ( "message", Encode.string message )
                            , ( "payload", Encode.string newPlaylist.order.current.link )
                            ]
                    )

                Previous ->
                    let
                        message =
                            if playlist.playing then
                                "play"

                            else
                                "load"
                    in
                    if playlist.progress < 5 then
                        let
                            newPlaylist =
                                previous model.playlist
                        in
                        ( Ok
                            { model
                                | playlist = newPlaylist
                            }
                        , control <|
                            Encode.object
                                [ ( "message", Encode.string message )
                                , ( "payload", Encode.string newPlaylist.order.current.link )
                                ]
                        )

                    else
                        ( model_
                        , control <|
                            Encode.object
                                [ ( "message", Encode.string message )
                                , ( "payload", Encode.string model.playlist.order.current.link )
                                ]
                        )

                Shuffle ->
                    if model.shuffled then
                        ( Ok { model | playlist = unshuffle playlist, shuffled = False }, Cmd.none )

                    else
                        ( Ok model, generate Shuffled (shuffle playlist.order.next) )

                Shuffled shuffledList ->
                    ( Ok
                        { model
                            | playlist =
                                { playlist
                                    | order =
                                        { current = playlist.order.current
                                        , next = shuffledList
                                        , previous = List.reverse shuffledList
                                        }
                                }
                            , shuffled = True
                        }
                    , Cmd.none
                    )

                Order field ->
                    if field == Tuple.first playlist.orderedBy then
                        ( Ok { model | playlist = flipDisplayOrder playlist }, Cmd.none )

                    else
                        case field of
                            Number ->
                                ( Ok
                                    { model
                                        | playlist =
                                            { playlist
                                                | displayOrder = List.sortBy .index playlist.displayOrder
                                                , orderedBy = ( field, Asc )
                                            }
                                    }
                                , Cmd.none
                                )

                            Title ->
                                ( Ok
                                    { model
                                        | playlist =
                                            { playlist
                                                | displayOrder = List.sortBy .name playlist.displayOrder
                                                , orderedBy = ( field, Asc )
                                            }
                                    }
                                , Cmd.none
                                )

                            Duration ->
                                ( Ok
                                    { model
                                        | playlist =
                                            { playlist
                                                | displayOrder = List.sortBy .duration playlist.displayOrder
                                                , orderedBy = ( field, Asc )
                                            }
                                    }
                                , Cmd.none
                                )

                LoadPlaylist core ->
                    ( Ok model, Task.attempt PlaylistLoaded (loadPlaylist core) )

                PlaylistLoaded result ->
                    case result of
                        Ok pl ->
                            ( Ok { model | playlist = pl, shuffled = False }
                            , control <|
                                Encode.object
                                    [ ( "message", Encode.string "load" )
                                    , ( "payload", Encode.string playlist.order.current.link )
                                    ]
                            )

                        Err err ->
                            ( Err <| errorToString err, Cmd.none )

                ChooseSong songIndex ->
                    let
                        newPlaylist =
                            chooseSong model.playlist songIndex
                                |> (\pl -> { pl | playing = True })
                    in
                    ( Ok
                        { model
                            | playlist = newPlaylist
                        }
                    , control <|
                        Encode.object
                            [ ( "message", Encode.string "play" )
                            , ( "payload", Encode.string newPlaylist.order.current.link )
                            ]
                    )

                ChangeVolume vol ->
                    ( Ok { model | volume = vol }
                    , control <|
                        Encode.object
                            [ ( "message", Encode.string "volume" )
                            , ( "payload", Encode.float vol )
                            ]
                    )

                TogglePause ->
                    ( Ok { model | playlist = { playlist | playing = not playlist.playing } }
                    , control <|
                        Encode.object
                            [ ( "message", Encode.string "togglePause" )
                            , ( "payload", Encode.null )
                            ]
                    )

                SelectBackground background ->
                    ( Ok { model | selectedBackground = background }, Cmd.none )

                ApplyBackground ->
                    ( Ok { model | appliedBackground = model.selectedBackground }
                    , control <|
                        Encode.object
                            [ ( "message", Encode.string "background" )
                            , ( "payload", Encode.string model.selectedBackground )
                            ]
                    )

                OnDragBy ( dx, dy ) ->
                    let
                        ( x, y ) =
                            model.dragState.position

                        newX =
                            toFloat x + dx

                        newY =
                            if toFloat y + dy < 0 then
                                0

                            else
                                toFloat y + dy
                    in
                    ( Ok
                        { model
                            | dragState =
                                { dragState
                                    | position =
                                        ( round newX
                                        , round newY
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
                    ( Ok
                        { model
                            | dragState = { position = model.dragState.position, drag = newDragState.drag }
                        }
                    , m
                    )

                Progress val ->
                    ( Ok
                        { model | playlist = { playlist | progress = val } }
                    , Cmd.none
                    )

        Err _ ->
            ( model_, Cmd.none )


loadPlaylist : PlaylistCore -> Task Http.Error Playlist
loadPlaylist core =
    Http.task
        { method = "GET"
        , headers = []
        , url = "playlists/" ++ core.link
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| playlistDecoder core.name core.link
        , timeout = Nothing
        }


loadPlaylistList : Task Http.Error (List PlaylistCore)
loadPlaylistList =
    Http.task
        { method = "GET"
        , headers = []
        , url = "playlists/playlists.json"
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| playlistListDecoder
        , timeout = Nothing
        }


handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    -- from https://korban.net/posts/elm/2019-02-15-combining-http-requests-with-task-in-elm/
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result


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
