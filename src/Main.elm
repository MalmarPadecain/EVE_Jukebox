port module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, audio, button, div, hr, img, input, table, tbody, td, th, tr, text)
import Html.Attributes exposing (class, id, max, min, src, type_, value)
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
            then Success { playlist = pl.playlist, index = Array.length pl.playlist.songs - 1 }
            else Success { playlist = pl.playlist, index = pl.index - 1 }
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
    div [ class "TracklistContainer" ]
        [ table [ class "TracklistHeader" ]
            [ tr []
                [ th [ id "col1head" ]
                    [ div [ class "thBorderRight" ]
                        [ text " " ]
                    ]
                , th [ class "thBorderParent", id "col2head" ]
                    [ text "Number                        "
                    , div [ class "thBorderRight" ]
                        [ text " " ]
                    ]
                , th [ id "col3head" ]
                    [ text "Title                        "
                    , div [ class "thBorderRight" ]
                        [ text " " ]
                    ]
                , th [ id "col4head" ]
                    [ text "Duration                        "
                    , div [ class "thBorderRight" ]
                        [ text " " ]
                    ]
                , th [ id "col5head" ]
                    []
                ]
            ]
    , table [ class "Tracklist" ] [ pl.songs
        |> Array.indexedMap (\index song -> tr [ onClick (ChooseSong index) ] [ td [] []
                                                                              , td [] [ text (String.fromInt index) ]
                                                                              , td [] [ text song.name ]
                                                                              , td [] [ text song.duration]
                                                                              , td [] []])
        |> Array.toList
        |> (\l -> l ++ [ tr [] [ td [ id "col1" ] []
                       , td [ id "col2" ] []
                       , td [ id "col3" ] []
                       , td [ id "col4" ] []
                       , td [ id "col5" ] []]
        ])
        |> tbody [ class "scrollContent" ]]]


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
            , body = [
                div [ class "jukeboxWrapper" ]
                    [ div [ class "jukeboxMain", id "TitleBox" ]
                        [ div [ class "MainTextPos" ]
                            [ text "Jukebox        " ]
                        ]
                    , div [ class "jukeboxMain", id "UpperSeparator" ]
                        [ hr []
                            []
                        ]
                    , div [ class "jukeboxMain", id "NowPlayingContainerContainer" ]
                        [ div [ id "NowPlayingContainer" ]
                            [ div [ class "NowPlaying", id "TimeElapsed" ]
                                [ text "03:49            " ]
                            , div [ class "NowPlaying", id "SongName" ]
                                [ text (currentSong model).name ]
                            ]
                        ]
                    , div [ class "jukeboxMain", id "buttonList" ]
                        [ div [ class "controlBtnContainer" ]
                            [ img [ class "controlBtn", src "images/btnBack.jpg", onClick Previous ]
                                []
                            , img [ class "controlBtn", src "images/btnPlay.jpg", onClick TogglePause ]
                                []
                            , img [ class "controlBtn", src "images/btnFwd.jpg" , onClick Next]
                                []
                            , img [ class "controlBtn", id "btnShuffle", src "images/btnShuffleOff.jpg", onClick Shuffle ]
                                []
                            ]
                        , div [ class "volumeContainer" ]
                            [ div [ class "volumeSymbol" ]
                                [ img [ src "images/lowVolume.png" ]
                                    []
                                ]
                            , div []
                                [ input [ class "slider", id "myRange", max "100", min "1", type_ "range", value "50" ]
                                    []
                                ]
                            , div [ class "volumeSymbol" ]
                                [ img [ src "images/highVolume.png" ]
                                    []
                                ]
                            ]
                        ]
                    , div [ class "jukeboxMain", id "LowerSeparator" ]
                        [ hr []
                            []
                        , div []
                            [ img [ id "doubleChevron", src "images/doubleChevron.png" ]
                                []
                            ]
                        ]
                    , div [ class "jukeboxMain", id "ListContainer" ]
                        [ div [ class "PlaylistContainer" ]
                            [ div [ class "Playlist" , onClick (Load "./playlists/EVE_Soundtrack.json")]
                                [ text "EVE Soundtrack" ]
                            , div [ class "Playlist" ]
                                [ text "Login Screens" ]
                            , div [ class "Playlist" ]
                                [ text "EVE Valkyrie" ]
                            , div [ class "Playlist" ]
                                [ text "Peace Logs" ]
                            , div [ class "Playlist" ]
                                [ text "War Logs" ]
                            , div [ class "Playlist" ]
                                [ text "Dead Logs" ]
                            ]
                        , renderTable pl.playlist
                            ]
                    , div [ class "jukeboxMain", id "BtnContainer" ]
                        [ div [ class "NewBtnContainer" ]
                            [ div [ id "newBtn" ]
                                [ button [ class "EVEButton" ]
                                    [ text "New" ]
                                ]
                            ]
                        , div [ class "AddBtnContainer" ]
                            [ button [ class "EVEButton" ]
                                [ text "Add Tracks" ]
                            ]
                        ]
                    , audio [ id "audio" ] []
                    ]
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
