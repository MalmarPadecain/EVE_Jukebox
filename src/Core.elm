module Core exposing (Model(..), Msg(..), Playlist, Song, currentSong, next, previous, secondsToString)

import Array exposing (Array)
import Draggable
import Http


type alias Song =
    { name : String
    , duration : String
    , link : String
    }


type alias Playlist =
    { index : Int
    , progress : Float
    , name : String
    , songs : Array Song
    }


type alias DragState =
    { position : ( Int, Int )
    , drag : Draggable.State String
    }


type Model
    = Success
        { playlist : Playlist
        , volume : Float
        , dragState : DragState
        }
    | Error String


type Msg
    = Next
    | Previous
    | Shuffle
    | Shuffled (Array Song)
    | Load String
    | Loaded (Result Http.Error Playlist)
    | ChooseSong Int
    | ChangeVolume Float
    | Play
    | TogglePause
    | OnDragBy Draggable.Delta
    | DragMsg (Draggable.Msg String)
    | Progress Float


next : Model -> Model
next model =
    case model of
        Success { playlist, volume, dragState } ->
            if playlist.index == Array.length playlist.songs - 1 then
                Success { playlist = { playlist | index = 0 }, volume = volume, dragState = dragState }

            else
                Success { playlist = { playlist | index = playlist.index + 1 }, volume = volume, dragState = dragState }

        Error _ ->
            model


previous : Model -> Model
previous model =
    case model of
        Success { playlist, volume, dragState } ->
            if playlist.index == 0 then
                Success
                    { playlist = { playlist | index = Array.length playlist.songs - 1 }
                    , volume = volume
                    , dragState = dragState
                    }

            else
                Success { playlist = { playlist | index = playlist.index - 1 }, volume = volume, dragState = dragState }

        Error _ ->
            model


currentSong : Model -> Song
currentSong model =
    case model of
        Success { playlist } ->
            Maybe.withDefault { name = "", link = "", duration = "" } <| Array.get playlist.index playlist.songs

        Error _ ->
            { name = "", link = "", duration = "" }


secondsToString : Float -> String
secondsToString seconds =
    let
        sec =
            remainderBy 60 (floor seconds)

        min =
            floor seconds // 60
    in
    String.fromInt min
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt sec)
