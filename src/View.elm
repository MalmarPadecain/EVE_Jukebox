module View exposing (renderTable, view)

import Array
import Browser
import Core exposing (..)
import Draggable
import Html exposing (Html, a, audio, b, br, button, div, h1, hr, img, input, p, source, table, tbody, td, text, th, tr, ul, video)
import Html.Attributes exposing (autoplay, class, href, id, loop, max, min, src, step, style, title, type_, value)
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
                                [ text <| .name <| Maybe.withDefault (currentSong playlist) playlist.currentSong ]
                            ]
                        ]
                    , div [ class "jukeboxMain", id "buttonList" ]
                        [ div [ class "controlBtnContainer" ]
                            [ div [ class "controlBtn", id "btnBack", onClick Previous ]
                                []
                            , div
                                [ class "controlBtn"
                                , if playlist.playing then
                                    id "btnPause"

                                  else
                                    id "btnPlay"
                                , onClick TogglePause
                                ]
                                []
                            , div [ class "controlBtn", id "btnFwd", onClick Next ]
                                []
                            , div
                                [ class "controlBtn"
                                , if playlist.shuffledSongs == Nothing then
                                    id "btnShuffleOff"

                                  else
                                    id "btnShuffleOn"
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
                            , div [ class "Playlist", onClick (Load "./playlists/Mission.json") ]
                                [ text "Mission Music" ]
                            , div [ class "Playlist", onClick (Load "./playlists/Login.json") ]
                                [ text "Login Themes" ]
                            , div [ class "Playlist", onClick (Load "./playlists/Cinematic.json") ]
                                [ text "Cinematic Themes" ]
                            , div [ class "Playlist", onClick (Load "./playlists/Permaband.json") ]
                                [ text "Permaband" ]
                            , div [ class "Playlist", onClick (Load "./playlists/Miscellaneous.json") ]
                                [ text "Misc Tracks" ]
                            , div [ class "Playlist", onClick (Load "./playlists/Upbeat.json") ]
                                [ text "Upbeat Music" ]
                            , div [ class "Playlist", onClick (Load "./playlists/Peace_Logs.json") ]
                                [ text "Peace Logs" ]
                            , div [ class "Playlist", onClick (Load "./playlists/War_Logs.json") ]
                                [ text "War Logs" ]
                            , div [ class "Playlist", onClick (Load "./playlists/Dead_Logs.json") ]
                                [ text "Dead Logs" ]
                            ]
                        , lazy renderTable playlist
                        ]
                    , div [ class "jukeboxMain", id "BtnContainer" ]
                        [ div [ class "NewBtnContainer" ]
                            [ div [ id "newBtn" ]
                                [ button [ class "EVEButton" ]
                                    [ text "Opt" ]
                                ]
                            ]
                        , div [ class "AddBtnContainer" ]
                            [ a [ class "EVEButton", href "#open-modal" ]
                                [ button [ class "EVEButton" ]
                                    [ text "About" ]
                                ]
                            ]
                        ]
                    , aboutWindow
                    , audio
                        [ id "audio"
                        , on "ended" (Decode.succeed Next)
                        ]
                        []
                    ]
                , videoDiv
                ]
            }

        Error msg ->
            { title = "Jukebox"
            , body = [ text msg ]
            }


{-| Turns a list into a div with a header table and a content table.
-}
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
                            [ if song == Maybe.withDefault { name = "", duration = "", link = "" } pl.currentSong then
                                td [] [ text "►" ]

                              else
                                td [] []
                            , td [] [ text (String.fromInt (index + 1)) ]
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


{-| The final row of the content table.
This is needed to set the width of all cells
and to make sure it fills the entirety of the remaining space in small playlists
-}
finalRow : Playlist -> List (Html msg)
finalRow pl =
    let
        stl =
            style "height" <|
                if 313 - (Array.length pl.songs * 16) > 0 then
                    (String.fromInt <| 303 - (Array.length pl.songs * 16)) ++ "px"

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
                [ text "CLOSE ×" ]
            , h1 [ class "modalText" ]
                [ text "The EVE Online Jukebox Project" ]
            , div [ class "modalText" ]
                [ hr []
                    []
                , p []
                    [ text "Bring back the good old times of EVE with this recreation of the EVE Jukebox using CSS. If you find any problems please let us know on Discord at Laura#1479 or Malmar Padecain#0659        " ]
                , div [ class "playlistDescriptionContainer" ]
                    [ div [ class "playlistDescription" ]
                        [ ul []
                            [ b [ class "orangeText" ]
                                [ text "EVE Soundtrack:" ]
                            , text "The original soundtrack as it was in the Crucible jukebox in 2011."
                            ]
                        , ul []
                            [ b [ class "orangeText" ]
                                [ text "Mission Music:" ]
                            , text "All of the music found in missions across New Eden. Mostly rock music."
                            ]
                        , ul []
                            [ b [ class "orangeText" ]
                                [ text "Login Themes:" ]
                            , text "A chronological list of expansion themes, or \"login music\"."
                            ]
                        , ul []
                            [ b [ class "orangeText" ]
                                [ text "Cinematic Themes:" ]
                            , text "Music from trailers."
                            ]
                        , ul []
                            [ b [ class "orangeText" ]
                                [ text "Permaband:" ]
                            , text "Only the best band this side of Jita."
                            ]
                        ]
                    , div [ class "playlistDescription" ]
                        [ ul []
                            [ b [ class "orangeText" ]
                                [ text "Misc Tracks:" ]
                            , text "Various tracks that didn't fit anywhere else."
                            ]
                        , ul []
                            [ b [ class "orangeText" ]
                                [ text "Upbeat Music:" ]
                            , text "Our own selection of fast-paced music for combat and such."
                            ]
                        , ul []
                            [ b [ class "orangeText" ]
                                [ text "Peace Logs:" ]
                            , text "Fan-made ambient and downtempo music."
                            ]
                        , ul []
                            [ b [ class "orangeText" ]
                                [ text "War Logs:" ]
                            , text "Fan made drum and bass or dubstep."
                            ]
                        , ul []
                            [ b [ class "orangeText" ]
                                [ text "Dead Logs:" ]
                            , text "Fan made dark ambient/drone music."
                            ]
                        ]
                    ]
                , hr []
                    []
                , br []
                    []
                , p [ class "smallText" ]
                    [ text "EVE Online and the EVE logo are the registered trademarks of CCP hf. All rights are reserved worldwide. All other trademarks are the property of their respective owners. EVE Online, the EVE logo, EVE and all associated logos and designs are the intellectual          property of CCP hf. All artwork, screenshots, characters, vehicles, storylines, world facts or other recognizable features of the intellectual property relating to these trademarks are likewise the intellectual property of CCP hf. CCP is in no way          responsible for the content on or functioning of this website, nor can it be liable for any damage arising from the use of this website.        " ]
                , p [ class "smallText" ]
                    [ text "Most of the music here is property of CCP Games and can be better accessed in a more modern interface at their "
                    , a [ class "orangeText", href "https://soundcloud.com/ccpgames" ]
                        [ text "Soundcloud" ]
                    ]
                , br []
                    []
                , p [ id "stylingBy" ]
                    [ a [ href "https://ashyin.space/", id "stylingBy" ]
                        [ text "Styling by Ashley Traynor" ]
                    , br []
                        []
                    , a [ href "https://evewho.com/pilot/Malhia+Fehrnah", id "stylingBy" ]
                        [ text "Backend by Malmar Padecain" ]
                    , br []
                        []
                    , a [ class "orangeText", href "https://github.com/MalmarPadecain/EVE_Jukebox" ]
                        [ text "This is an open source project!" ]
                    ]
                ]
            ]
        ]


videoDiv : Html msg
videoDiv =
    div [ id "videoWrapper" ]
        [ video [ id "film", style "z-index" "8", autoplay True, loop True ]
            [ source [ src "video/minmatarStation.mp4", type_ "video/mp4" ] []
            ]
        ]


{-| Transforms a time value as received from audio.currentTime to a human readable string

    secondsToString 2.0 == "0:02"

    secondsToString 60.1 == "1:00"

    secondsToString 100.0 == "1:40"

-}
secondsToString : Float -> String
secondsToString seconds =
    let
        sec =
            remainderBy 60 (round seconds)

        min =
            floor seconds // 60
    in
    String.fromInt min
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt sec)
