module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, li, text, ul)
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
    , playlist : Array Song}


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


init : () -> ( Model, Cmd Msg )
init _ =
    ( Success { playlist = { name = "Empty", playlist = Array.empty }
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next ->
            case model of
                Success playlist ->
                    if playlist.index == Array.length playlist.playlist.playlist - 1
                    then ( Success { playlist = playlist.playlist, index = 0 }, Cmd.batch [] )
                    else( Success { playlist = playlist.playlist, index = playlist.index + 1 }, Cmd.batch [] )
                Error _ ->
                    (model, Cmd.batch [])
        Previous ->
            case model of
                Success playlist ->
                    if playlist.index == 0 then
                        ( Success{ playlist = playlist.playlist, index = Array.length playlist.playlist.playlist - 1 }, Cmd.batch [] )

                    else
                        ( Success{ playlist = playlist.playlist, index = playlist.index - 1 }, Cmd.batch [] )
                Error _ ->
                    (model, Cmd.batch [])
        Shuffle ->
            case model of
                Success playlist ->
                    ( model, generate Shuffled (shuffle playlist.playlist.playlist) )
                Error _ ->
                    (model, Cmd.batch [])
        Shuffled shuffledList ->
            ( Success{ playlist = { name = "", playlist = shuffledList } , index = 0 }, Cmd.batch [] )
        Load link ->
            ( model, loadJson link)
        Loaded result ->
            case result of
                Ok lst ->
                    (Success { playlist = lst , index = 0}, Cmd.none)
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


renderList : Model -> Html msg
renderList model =
    case model of
        Success playlist ->
            playlist.playlist.playlist
                |> Array.toList
                |> List.map (\l -> li [] [ text l.name ])
                |> ul []
        Error err ->
            div [] [ text err ]


currentlyPlaying : Model -> Song
currentlyPlaying model =
    case model of
        Success playlist ->
            case Array.get playlist.index playlist.playlist.playlist of
                Nothing ->
                    {name="", link="", duration=""}
                Just song ->
                    song
        Error _ ->
            {name="", link="", duration=""}


view : Model -> Browser.Document Msg
view model =
    { title = "Jukebox"
    , body =
        [ button [ onClick Shuffle ] [ text "Shuffle" ]
        , button [ onClick Previous ] [ text "-" ]
        , renderList model
        , div [] [ text ((currentlyPlaying model).name) ]
        , button [ onClick Next ] [ text "+" ]
        , button [ onClick (Load "playlists/EVE_Soundtrack.json") ] [text "Load"]
        ]
    }


--viewPlaylist : Model -> Html Msg
--viewPlaylist model =


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
                 (field "link" string)
                 (field "duration" string))))
