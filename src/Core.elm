module Core exposing (Model(..), Msg(..), Playlist, Song, chooseSong, currentSong, next, playlistDecoder, previous)

import Array exposing (Array)
import Draggable
import Http
import Json.Decode exposing (Decoder, array, field, map2, map3, string)


type alias Song =
    { name : String
    , duration : String
    , link : String
    }


type alias Playlist =
    { index : Int
    , progress : Float
    , shuffledSongs : Maybe (Array Song)

    -- after shuffling or unshffling the currentSong changes immediately but the playing song stays the same
    , currentSong : Maybe Song
    , playing : Bool
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


{-| Moves the index of the playlist one step further. If it hits the end it loops around.
Also sets tmpCurrentSong to Nothing
-}
next : Playlist -> Playlist
next playlist =
    let
        newIndex =
            if playlist.index == Array.length playlist.songs - 1 then
                0

            else
                playlist.index + 1
    in
    case playlist.shuffledSongs of
        Nothing ->
            { playlist | index = newIndex, currentSong = Just <| songAt playlist.songs newIndex }

        Just songs ->
            { playlist | index = newIndex, currentSong = Just <| songAt songs newIndex }


{-| Moves the index of the playlist one step back. If it hits the beginning it loops around
-}
previous : Playlist -> Playlist
previous playlist =
    let
        newIndex =
            if playlist.index == 0 then
                Array.length playlist.songs - 1

            else
                playlist.index - 1
    in
    case playlist.shuffledSongs of
        Nothing ->
            { playlist | index = newIndex, currentSong = Just <| songAt playlist.songs newIndex }

        Just songs ->
            { playlist | index = newIndex, currentSong = Just <| songAt songs newIndex }


{-| Returns the Song at the current position.
If an illegal int is given it returns { name = "", link = "", duration = "" }
-}
currentSong : Playlist -> Song
currentSong playlist =
    let
        songs =
            if playlist.shuffledSongs == Nothing then
                playlist.songs

            else
                Maybe.withDefault Array.empty playlist.shuffledSongs
    in
    Maybe.withDefault { name = "", link = "", duration = "" } <| Array.get playlist.index songs


songAt : Array Song -> Int -> Song
songAt songs i =
    Maybe.withDefault { name = "", link = "", duration = "" } <| Array.get i songs


{-| Sets the index to the given value. No checks are performed.
-}
chooseSong : Playlist -> Int -> Playlist
chooseSong playlist songIndex =
    case playlist.shuffledSongs of
        Nothing ->
            { playlist | index = songIndex, currentSong = Just <| songAt playlist.songs songIndex }

        Just songs ->
            { playlist | index = songIndex, currentSong = Just <| songAt songs songIndex }


playlistDecoder : Decoder Playlist
playlistDecoder =
    map2 (Playlist 0 0 Nothing Nothing False)
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
