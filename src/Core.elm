module Core exposing (Model(..), Msg(..), Playlist, Song, chooseSong, currentSong, next, previous, secondsToString)

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
    , shuffled : Bool
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


next : Playlist -> Playlist
next playlist =
    if playlist.index == Array.length playlist.songs - 1 then
        { playlist | index = 0 }

    else
        { playlist | index = playlist.index + 1 }


previous : Playlist -> Playlist
previous playlist =
    if playlist.index == 0 then
        { playlist | index = Array.length playlist.songs - 1 }

    else
        { playlist | index = playlist.index - 1 }


currentSong : Playlist -> Song
currentSong playlist =
    Maybe.withDefault { name = "", link = "", duration = "" } <| Array.get playlist.index playlist.songs


chooseSong : Playlist -> Int -> Playlist
chooseSong pl songIndex =
    { pl | index = songIndex }


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
