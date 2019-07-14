module Core exposing (Model(..), Msg(..), Playlist, Song, currentSong, previous, next)

import Array exposing (Array)
import Http


type alias Song =
    { name : String
    , duration : String
    , link : String
    }


type alias Playlist =
    { index : Int
    , volume : Float
    , name : String
    , songs : Array Song
    }


type Model
    = Success Playlist
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


next : Model -> Model
next model =
    case model of
        Success pl ->
            if pl.index == Array.length pl.songs - 1 then
                Success { pl | index = 0 }

            else
                Success { pl | index = pl.index + 1 }

        Error _ ->
            model


previous : Model -> Model
previous model =
    case model of
        Success pl ->
            if pl.index == 0 then
                Success { pl | index = Array.length pl.songs - 1 }

            else
                Success { pl | index = pl.index - 1 }

        Error _ ->
            model


currentSong : Model -> Song
currentSong model =
    case model of
        Success pl ->
            Maybe.withDefault { name = "", link = "", duration = "" } <| Array.get pl.index pl.songs

        Error _ ->
            { name = "", link = "", duration = "" }
