port module Main exposing (control, main, progress, update)

import Browser
import Core exposing (..)
import Draggable
import Http
import Platform.Sub as Sub
import Playlist exposing (..)
import Random exposing (generate)
import Random.List exposing (shuffle)
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
        , selectedBackground = Still Minmatar
        , appliedBackground = Still Minmatar
        }
        |> update Init


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
                Init ->
                    ( Success model, Cmd.batch [ loadPlaylistList, control "background ./video/minmatarStill.png" ] )

                PlaylistsLoaded result ->
                    case result of
                        Ok ((x :: _) as list) ->
                            update (Load x.link) (Success { model | playlistList = list })

                        Ok [] ->
                            ( Error "No playlists found.", Cmd.none )

                        Err err ->
                            ( Error <| errorToString err, Cmd.none )

                Next ->
                    update Play (Success { model | playlist = next model.playlist })

                Previous ->
                    if playlist.progress < 5 then
                        update Play (Success { model | playlist = previous model.playlist })

                    else
                        update Play model_

                Shuffle ->
                    if model.shuffled then
                        ( Success { model | playlist = unshuffle playlist, shuffled = False }, Cmd.none )

                    else
                        ( Success model, generate Shuffled (shuffle playlist.order.next) )

                Shuffled shuffledList ->
                    ( Success
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
                        ( Success { model | playlist = flipDisplayOrder playlist }, Cmd.none )

                    else
                        case field of
                            Number ->
                                ( Success
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
                                ( Success
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
                                ( Success
                                    { model
                                        | playlist =
                                            { playlist
                                                | displayOrder = List.sortBy .duration playlist.displayOrder
                                                , orderedBy = ( field, Asc )
                                            }
                                    }
                                , Cmd.none
                                )

                Load link ->
                    ( Success model, loadPlaylist link playlist.core.name )

                Loaded result ->
                    case result of
                        Ok pl ->
                            ( Success { model | playlist = pl }
                            , control ("load " ++ playlist.order.current.link)
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
                    , control ("play " ++ playlist.order.current.link)
                    )

                TogglePause ->
                    ( Success { model | playlist = { playlist | playing = not playlist.playing } }
                    , control "togglePause"
                    )

                SelectBackground background ->
                    ( Success { model | selectedBackground = background }, Cmd.none )

                ApplyBackground ->
                    ( Success { model | appliedBackground = model.selectedBackground }
                    , control ("background " ++ resolveBackground model.selectedBackground)
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
                    ( Success
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


loadPlaylist : String -> String -> Cmd Msg
loadPlaylist link name =
    Http.get
        { url = "playlists/" ++ link
        , expect = Http.expectJson Loaded <| playlistDecoder link name
        }


loadPlaylistList : Cmd Msg
loadPlaylistList =
    Http.get
        { url = "playlists/playlists.json"
        , expect = Http.expectJson PlaylistsLoaded <| playlistListDecoder
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
