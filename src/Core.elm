module Core exposing (Model(..), Msg(..))

import Draggable
import Http
import Playlist exposing (..)


type alias DragState =
    { position : ( Int, Int )
    , drag : Draggable.State String
    }


type Model
    = Success
        { playlist : Playlist
        , volume : Float
        , dragState : DragState
        , shuffled : Bool
        , playlistList : List PlaylistCore
        }
    | Error String


type Msg
    = Init
    | PlaylistsLoaded (Result Http.Error (List PlaylistCore))
    | Next
    | Previous
    | Shuffle
    | Shuffled (List Song)
    | Order OrderBy
    | Load String
    | Loaded (Result Http.Error Playlist)
    | ChooseSong Song
    | ChangeVolume Float
    | Play
    | TogglePause
    | OnDragBy Draggable.Delta
    | DragMsg (Draggable.Msg String)
    | Progress Float
