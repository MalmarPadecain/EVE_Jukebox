module View exposing (renderTable, view)

import Array
import Browser
import Core exposing (..)
import Draggable
import Html exposing (Html, a, audio, br, button, div, h1, hr, img, input, p, table, tbody, td, text, th, tr)
import Html.Attributes exposing (class, href, id, max, min, src, step, style, title, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Html.Lazy exposing (lazy)
import Json.Decode as Decode


view : Model -> Browser.Document Msg
view model =
    case model of
        Success { playlist, volume, dragState } ->
            { title = "Jukebox"
            , body =
                [ div
                    [ class "jukeboxWrapper"
                    , style "transform" <|
                        "translate("
                            ++ String.fromInt (Tuple.first dragState.position)
                            ++ "px, "
                            ++ String.fromInt (Tuple.second dragState.position)
                            ++ "px)"
                    ]
                    [ div
                        [ class "jukeboxMain"
                        , id "TitleBox"
                        , Draggable.mouseTrigger "jukebox" DragMsg
                        ]
                        [ div [ class "MainTextPos" ]
                            [ text "Jukebox" ]
                        ]
                    , div [ class "jukeboxMain", id "UpperSeparator" ]
                        [ hr [] []
                        ]
                    , div [ class "jukeboxMain", id "NowPlayingContainerContainer" ]
                        [ div [ id "NowPlayingContainer" ]
                            [ div [ class "NowPlaying", id "TimeElapsed" ]
                                [ text <| secondsToString playlist.progress ]
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
                            , img [ class "controlBtn", src "images/btnFwd.jpg", onClick Next ]
                                []
                            , img
                                [ class "controlBtn"
                                , id "btnShuffle"
                                , if playlist.shuffled then
                                    src "images/btnShuffleOn.jpg"

                                  else
                                    src "images/btnShuffleOff.jpg"
                                , onClick Shuffle
                                ]
                                []
                            ]
                        , div [ class "volumeContainer" ]
                            [ div
                                [ class "volumeSymbol"
                                , onClick <| ChangeVolume 0
                                ]
                                [ img [ src "images/lowVolume.png" ] []
                                ]
                            , div []
                                [ input
                                    [ class "slider"
                                    , id "myRange"
                                    , max "100"
                                    , min "0"
                                    , step "1"
                                    , type_ "range"
                                    , value <| String.fromFloat volume
                                    , onInput (\vol -> ChangeVolume <| Maybe.withDefault 100 <| String.toFloat vol)
                                    ]
                                    []
                                ]
                            , div
                                [ class "volumeSymbol"
                                , onClick <| ChangeVolume 100
                                ]
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
                            [ div [ class "Playlist", onClick (Load "./playlists/EVE_Soundtrack.json") ]
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
                        , lazy renderTable playlist
                        ]
                    , div [ class "jukeboxMain", id "BtnContainer" ]
                        [ div [ class "NewBtnContainer" ]
                            [ div [ id "newBtn" ]
                                [ button [ class "EVEButton" ]
                                    [ text "New" ]
                                ]
                            ]
                        , div [ class "AddBtnContainer" ]
                            [ a [ class "EVEButton", href "#open-modal" ]
                                [ text "About" ]
                            ]
                        ]
                    , aboutWindow
                    , audio
                        [ id "audio"
                        , on "ended" (Decode.succeed Next)
                        ]
                        []
                    ]
                ]
            }

        Error msg ->
            { title = "Jukebox"
            , body = [ text msg ]
            }


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
        , table [ class "Tracklist" ]
            [ pl.songs
                |> Array.indexedMap
                    (\index song ->
                        tr [ onClick (ChooseSong index) ]
                            [ td [] []
                            , td [] [ text (String.fromInt index) ]
                            , td [] [ text song.name ]
                            , td [] [ text song.duration ]
                            , td [] []
                            ]
                    )
                |> Array.toList
                |> (\l -> l ++ finalRow pl)
                |> tbody [ class "scrollContent" ]
            ]
        ]


finalRow : Playlist -> List (Html msg)
finalRow pl =
    let
        stl =
            style "height" <|
                if 313 - (Array.length pl.songs * 16) > 0 then
                    (String.fromInt <| 313 - (Array.length pl.songs * 16)) ++ "px"

                else
                    "0"
    in
    [ tr []
        [ td [ id "col1", class "finalRow", stl ] []
        , td [ id "col2", class "finalRow", stl ] []
        , td [ id "col3", class "finalRow", stl ] []
        , td [ id "col4", class "finalRow", stl ] []
        , td [ id "col5", class "finalRow", stl ] []
        ]
    ]


aboutWindow =
    div [ class "modal-window", id "open-modal" ]
        [ div []
            [ a [ class "modal-close", href "#modal-close", title "Close" ]
                [ text "CLOSE" ]
            , h1 [ class "modalText" ]
                [ text "The EVE Online Jukebox Project" ]
            , div [ class "modalText" ]
                [ hr [] []
                , p []
                    [ text "Bring back the good old times of EVE with this recreation of the EVE Jukebox using CSS." ]
                , br [] []
                , p [ id "smallText" ]
                    [ text """EVE Online and the EVE logo are the registered trademarks of CCP hf.
                               All rights are reserved worldwide. All other trademarks are the property of their
                               respective owners. EVE Online, the EVE logo, EVE and all associated logos and designs
                               are the intellectual property of CCP hf. All artwork, screenshots, characters, vehicles,
                               storylines, world facts or other recognizable features of the intellectual property
                               relating to these trademarks are likewise the intellectual property of CCP hf.
                               CCP is in no way responsible for the content on or functioning of this website, nor can
                               it be liable for any damage arising from the use of this website.""" ]
                , br [] []
                , p [ id "stylingBy" ]
                    [ a [ href "https://ashyin.space/", id "stylingBy" ]
                        [ text "Styling by Ashley Traynor" ]
                    , br []
                        []
                    , a [ href "https://evewho.com/pilot/Malhia+Fehrnah", id "stylingBy" ]
                        [ text "Backend by Malmar Padecain" ]
                    , br []
                        []
                    , a [ href "https://github.com/MalmarPadecain/EVE_Jukebox", id "gitLink" ]
                        [ text "This is an open source project!" ]
                    ]
                ]
            ]
        ]
