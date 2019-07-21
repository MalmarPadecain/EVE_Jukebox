module Core exposing (Model(..), Msg(..), Playlist, Song, currentSong, next, previous)

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
