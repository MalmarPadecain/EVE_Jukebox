port module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, audio, button, div, li, text, ul)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, string, array, map2, map3)
import Maybe
import Random exposing (generate)
import Random.Array exposing (shuffle)


type alias Song =
    { name : String
    , duration : String
    , link : String
    }


type alias Playlist =
    { name : String
    , songs : Array Song}


type Model =
    Success { playlist : Playlist
    , index : Int
    } | Error String


type Msg
    = Next
    | Previous
    | Shuffle
    | Shuffled (Array Song)
    | Load String
    | Loaded (Result Http.Error Playlist)
    | ChooseSong Int
    | Play
    | TogglePause


port control : String -> Cmd msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( Success { playlist = { name = "Empty", songs = Array.empty }
      , index = 0
      }
    , Cmd.none
    )


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


next : Model -> Model
next model =
    case model of
        Success pl ->
            if pl.index == Array.length pl.playlist.songs - 1
            then Success { playlist = pl.playlist, index = 0 }
            else Success { playlist = pl.playlist, index = pl.index + 1 }
        Error _ ->
            model


previous : Model -> Model
previous model =
    case model of
        Success pl ->
            if pl.index == 0
            then Success { playlist = pl.playlist, index = 0 }
            else Success { playlist = pl.playlist, index = pl.index + 1 }
        Error _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next ->
            update Play (next model)
        Previous ->
            update Play (previous model)
        Shuffle ->
            case model of
                Success pl ->
                    ( model, generate Shuffled (shuffle pl.playlist.songs) )
                Error _ ->
                    (model, Cmd.batch [])
        Shuffled shuffledList ->
            ( Success{ playlist = { name = "", songs = shuffledList } , index = 0 }, Cmd.batch [] )
        Load link ->
            ( model, loadJson link)
        Loaded result ->
            case result of
                Ok pl ->
                    (Success { playlist = pl , index = 0}, Cmd.none)
                Err err ->
                    case err of
                        Http.BadUrl url ->
                            (Error ("Bad URL: " ++ url), Cmd.none)
                        Http.Timeout ->
                            (Error ("Request timed out."), Cmd.none)
                        Http.NetworkError ->
                            (Error ("A Network Error occurred"), Cmd.none)
                        Http.BadStatus status ->
                            (Error (String.fromInt status ++ " returned"), Cmd.none)
                        Http.BadBody body ->
                            (Error ("Bad Body: " ++ body ), Cmd.none)
        ChooseSong songIndex ->
            case model of
                Success pl->
                    update Play <| Success {playlist = pl.playlist, index = songIndex}
                Error _ ->
                    update Play model
        Play ->
            (model, control ("play " ++ (currentSong model).link))
        TogglePause ->
            (model, control "togglePause")


renderList : Model -> Html Msg
renderList model =
    case model of
        Success pl ->
            pl.playlist.songs
                |> Array.indexedMap (\index song -> li [ onClick (ChooseSong index) ] [ text song.name ])
                |> Array.toList
                |> ul []
        Error err ->
            div [] [ text err ]


currentSong : Model -> Song
currentSong model =
    case model of
        Success pl ->
            Maybe.withDefault {name="", link="", duration=""} <| Array.get pl.index pl.playlist.songs
        Error _ ->
            {name="", link="", duration=""}


view : Model -> Browser.Document Msg
view model =
    case model of
        Success pl->
            { title = "Jukebox"
            , body =
                [ button [ onClick Shuffle ] [ text "Shuffle" ]
                , button [ onClick Previous ] [ text "-" ]
                , button [ onClick Next ] [ text "+" ]
                , button [ onClick TogglePause ] [ text "Pause" ]
                , renderList model
                , div [] [ text (((currentSong model).name) ++ " " ++ String.fromInt pl.index) ]
                , button [ onClick (Load "playlists/EVE_Soundtrack.json") ] [text "Load"]
                , audio
                    [ id "audio" ] []
                ]
            }
        Error msg ->
            { title = "Jukebox"
            , body = [ text msg ] }


loadJson : String -> Cmd Msg
loadJson link =
    Http.get
        { url = link
        , expect = Http.expectJson Loaded songDecoder}


songDecoder : Decoder Playlist
songDecoder =
    map2 Playlist
        (field "name" string)
        (field "songs"
            (array (map3 Song
                 (field "name" string)
                 (field "duration" string)
                 (field "link" string))))
