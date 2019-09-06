module Core exposing (Background(..), Faction(..), Model(..), Msg(..), resolveBackground)

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
        , selectedBackground : Background
        , appliedBackground : Background
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
    | SelectBackground Background
    | ApplyBackground
    | OnDragBy Draggable.Delta
    | DragMsg (Draggable.Msg String)
    | Progress Float


type Faction
    = Gallente
    | Minmatar
    | Amarr
    | Caldari


type Background
    = Still Faction
    | Balcony Faction
    | Hangar Faction
    | Room Faction


resolveBackground : Background -> String
resolveBackground bg =
    let
        path =
            "./video/"
    in
    case bg of
        Still Gallente ->
            path ++ "gallenteStill.png"

        Still Minmatar ->
            path ++ "minmatarStill.png"

        Still Amarr ->
            path ++ "amarrStill.png"

        Still Caldari ->
            path ++ "caldariStill.png"

        Balcony Gallente ->
            path ++ "gallenteBalcony.mp4"

        Balcony Minmatar ->
            path ++ "minmatarBalcony.mp4"

        Balcony Amarr ->
            path ++ "amarrBalcony.mp4"

        Balcony Caldari ->
            path ++ "caldariBalcony.mp4"

        Hangar Gallente ->
            path ++ "gallenteHangar.mp4"

        Hangar Minmatar ->
            path ++ "minmatarHangar.mp4"

        Hangar Amarr ->
            path ++ "amarrHangar.mp4"

        Hangar Caldari ->
            path ++ "caldariHangar.mp4"

        Room Gallente ->
            path ++ "gallenteRoom.mp4"

        Room Minmatar ->
            path ++ "minmatarRoom.mp4"

        Room Amarr ->
            path ++ "amarrRoom.mp4"

        Room Caldari ->
            path ++ "caldariRoom.mp4"
