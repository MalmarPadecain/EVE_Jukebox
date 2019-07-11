port module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, audio, button, div, table, tbody, thead, td, th, tr, text)
import Html.Attributes exposing (class, id)
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
    case model of
        Success pl ->
            case msg of
                Next ->
                    update Play (next model)
                Previous ->
                    update Play (previous model)
                Shuffle ->
                    ( model, generate Shuffled (shuffle pl.playlist.songs) )
                Shuffled shuffledList ->
                    ( Success{ playlist = { name = "", songs = shuffledList } , index = 0 }, Cmd.batch [] )
                Load link ->
                    ( model, loadJson link)
                Loaded result ->
                    case result of
                        Ok playlist ->
                            (Success { playlist = playlist , index = 0}, Cmd.none)
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
                    update Play <| Success {playlist = pl.playlist, index = songIndex}
                Play ->
                    (model, control ("play " ++ (currentSong model).link))
                TogglePause ->
                    (model, control "togglePause")
        Error _ ->
            (model, Cmd.none)


renderTable : Playlist -> Html Msg
renderTable pl =
    -- index comes from the map not from the model. will this lead to problems?
    table [ class "Tracklist" ] [ thead [] [ tr [] [ th [] []
                                           , th [] [ text "Number" ]
                                           , th [] [ text "Title" ]
                                           , th [] [ text "Duration" ]
                                           , th [] []]]
    , pl.songs
        |> Array.indexedMap (\index song -> tr [ onClick (ChooseSong index) ] [ td [] []
                                                                              , td [] [ text (String.fromInt index) ]
                                                                              , td [] [ text song.name ]
                                                                              , td [] [ text song.duration]])
        |> Array.toList
        |> tbody []]


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
                [ div [ class "jukeboxMain" ] [ text "1" ]
                , div [ class "jukeboxMain" ] [ text "2" ]
                , div [ class "jukeboxMain" ] [ text "3" ]
                , div [ class "jukeboxMain" ] [ text "4" ]
                , div [ class "jukeboxMain"
                      , id "listContainer" ] [ div [ class "PlaylistContainer" ] [ text "5.1" ]
                                                   , div [ class "TracklistContainer"] [ renderTable pl.playlist ]]
                , div [ class "jukeboxMain" ] [ text "6" ]
                , button [ onClick Shuffle ] [ text "Shuffle" ]
                , button [ onClick Previous ] [ text "-" ]
                , button [ onClick Next ] [ text "+" ]
                , button [ onClick TogglePause ] [ text "Pause" ]
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
