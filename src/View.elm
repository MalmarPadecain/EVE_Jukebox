module View exposing (renderTable, view)

import Browser
import Core exposing (..)
import Draggable
import Html exposing (Html, a, audio, b, br, button, div, h1, h3, hr, img, input, p, source, table, tbody, td, text, th, tr, ul, video)
import Html.Attributes exposing (attribute, autoplay, class, href, id, loop, max, min, name, src, step, style, title, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Html.Lazy exposing (lazy)
import Json.Decode as Decode
import Playlist exposing (..)


view : Model -> Browser.Document Msg
view model =
    case model of
        Success { playlist, volume, dragState, shuffled, playlistList } ->
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
                                [ text playlist.order.current.name ]
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
                                , if shuffled then
                                    id "btnShuffleOn"

                                  else
                                    id "btnShuffleOff"
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
                            (renderPlaylistList playlistList)
                        , lazy renderTable playlist
                        ]
                    , div [ class "jukeboxMain", id "BtnContainer" ]
                        [ div [ class "NewBtnContainer" ]
                            [ div [ id "newBtn" ]
                                [ a [ href "#open-opt" ]
                                    [ button
                                        [ class "EVEButton" ]
                                        [ text "Opt" ]
                                    ]
                                ]
                            ]
                        , div [ class "AddBtnContainer" ]
                            [ div [ id "about" ]
                                [ a [ href "#open-about" ]
                                    [ button [ class "EVEButton" ]
                                        [ text "About" ]
                                    ]
                                ]
                            ]
                        ]
                    , aboutWindow
                    , optionWindow
                    , audio
                        [ id "audio"
                        , on "ended" (Decode.succeed Next)
                        ]
                        []
                    ]
                , videoDiv
                , div [ id "overviewWrapper" ] []
                ]
            }

        Error msg ->
            { title = "Jukebox"
            , body = [ text msg ]
            }


renderPlaylistList : List PlaylistCore -> List (Html Msg)
renderPlaylistList list =
    List.map (\plc -> div [ class "PlaylistTest", onClick (Load plc.link) ] [ text plc.name ]) list


{-| Turns a list into a div with a header table and a content table.
-}
renderTable : Playlist -> Html Msg
renderTable playlist =
    let
        ( orderedBy, direction ) =
            playlist.orderedBy

        orderSign =
            case direction of
                Asc ->
                    "🞁"

                Desc ->
                    "🞃"

        numberText =
            if orderedBy == Number then
                "Number " ++ orderSign

            else
                "Number"

        titleText =
            if orderedBy == Title then
                "Title " ++ orderSign

            else
                "Title"

        durationText =
            if orderedBy == Duration then
                "Duration " ++ orderSign

            else
                "Duration"
    in
    div [ class "TracklistContainer" ]
        [ table [ class "TracklistHeader" ]
            [ tr []
                [ th [ id "col1head" ]
                    [ div [ class "thBorderRight" ]
                        [ text " " ]
                    ]
                , th [ class "thBorderParent", id "col2head", onClick <| Order Number ]
                    [ text numberText
                    , div [ class "thBorderRight" ]
                        [ text " " ]
                    ]
                , th [ id "col3head", onClick <| Order Title ]
                    [ text titleText
                    , div [ class "thBorderRight" ]
                        [ text " " ]
                    ]
                , th [ id "col4head", onClick <| Order Duration ]
                    [ text durationText
                    , div [ class "thBorderRight" ]
                        [ text " " ]
                    ]
                , th [ id "col5head" ]
                    []
                ]
            ]
        , table [ class "Tracklist" ]
            [ playlist.displayOrder
                |> List.map
                    (\song ->
                        tr [ onClick (ChooseSong song) ]
                            [ if song == playlist.order.current then
                                td [] [ text "►" ]

                              else
                                td [] []
                            , td [] [ text (String.fromInt (song.index + 1)) ]
                            , td [] [ text song.name ]
                            , td [] [ text song.duration ]
                            , td [] []
                            ]
                    )
                |> (\l -> l ++ finalRow playlist)
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
                if 313 - (List.length pl.displayOrder * 16) > 0 then
                    (String.fromInt <| 303 - (List.length pl.displayOrder * 16)) ++ "px"

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
    div [ class "modal-window", id "open-about" ]
        [ div [ class "modalInner" ]
            [ a [ class "modal-close", href "#modal-close", title "Close" ]
                [ text "CLOSE ×" ]
            , h1 [ class "modalText" ]
                [ text "The EVE Online Jukebox Project" ]
            , div [ class "modalText" ]
                [ hr []
                    []
                , p []
                    [ text "Bring back the good old times of EVE with this recreation of the EVE Jukebox using CSS. If you find any problems please let us know on Discord at Laura#1479 or Malmar Padecain#0659        " ]
                , ul []
                    [ b [ class "orangeText" ]
                        [ text "EVE Soundtrack: " ]
                    , text "The original soundtrack as it was in the Crucible jukebox in 2011."
                    ]
                , ul []
                    [ b [ class "orangeText" ]
                        [ text "Mission Music: " ]
                    , text "All of the music found in missions across New Eden. Mostly rock music."
                    ]
                , ul []
                    [ b [ class "orangeText" ]
                        [ text "Login Themes: " ]
                    , text "A chronological list of expansion themes, or \"login music\"."
                    ]
                , ul []
                    [ b [ class "orangeText" ]
                        [ text "Cinematic Themes: " ]
                    , text "Music from trailers."
                    ]
                , ul []
                    [ b [ class "orangeText" ]
                        [ text "Permaband: " ]
                    , text "Only the best band this side of Jita."
                    ]
                , ul []
                    [ b [ class "orangeText" ]
                        [ text "Orchestral: " ]
                    , text "The 10-year anniversary music from Fanfest 2013"
                    ]
                , div [ class "modalFlex" ]
                    [ ul []
                        [ b [ class "orangeText" ]
                            [ text "Misc Tracks: " ]
                        , text "Various tracks that didn't fit anywhere else."
                        ]
                    , ul []
                        [ b [ class "orangeText" ]
                            [ text "Upbeat Music: " ]
                        , text "Our own selection of fast-paced music for combat and such."
                        ]
                    , ul []
                        [ b [ class "orangeText" ]
                            [ text "Peace Logs: " ]
                        , text "Fan-made ambient and downtempo music."
                        ]
                    , ul []
                        [ b [ class "orangeText" ]
                            [ text "War Logs: " ]
                        , text "Fan made drum and bass or dubstep."
                        ]
                    , ul []
                        [ b [ class "orangeText" ]
                            [ text "Dead Logs: " ]
                        , text "Fan made dark ambient/drone music."
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


optionWindow =
    div [ class "modal-window", id "open-opt" ]
        [ div [ class "modalInner" ]
            [ a [ class "modal-close", href "#modal-close", title "Close" ]
                [ text "CLOSE ×" ]
            , h1 [ class "modalText" ]
                [ text "Jukebox Options" ]
            , div [ class "modalText" ]
                [ hr []
                    []
                , p []
                    [ text "Here you can select the background that you see behind the jukebox as captured 2011 style. We recommend a still image if you're on a limited-data connection.        " ]
                , hr []
                    []
                , h3 []
                    [ text "Video Backgrounds" ]
                , div [ class "modalFlexContainer" ]
                    [ div [ class "modalFlex" ]
                        [ div [ class "radioButtonTitle" ]
                            [ text "Minmatar" ]
                        , ul [ class "" ]
                            [ input [ name "backgroundSelect", type_ "radio", value "minmatarBalcony" ]
                                []
                            , text "Balcony"
                            ]
                        , ul [ class "" ]
                            [ input [ name "backgroundSelect", type_ "radio", value "minmatarHangar" ]
                                []
                            , text "Hangar [WIP]"
                            ]
                        , ul [ class "" ]
                            [ input [ name "backgroundSelect", type_ "radio", value "minmatarHangar" ]
                                []
                            , text "Room [WIP]"
                            ]
                        , ul [ class "" ]
                            [ input [ name "backgroundSelect", type_ "radio", value "minmatarBalconyStill" ]
                                []
                            , text "Still Image"
                            ]
                        ]
                    , div [ class "modalFlex" ]
                        [ div [ class "radioButtonTitle" ]
                            [ text "Gallente" ]
                        , ul [ class "orangeText" ]
                            [ input [ name "backgroundSelect", type_ "radio", value "gallenteBalcony" ]
                                []
                            , text "Balcony"
                            ]
                        , ul [ class "" ]
                            [ input [ name "backgroundSelect", type_ "radio", value "gallenteHangar" ]
                                []
                            , text "Hangar [WIP]"
                            ]
                        , ul [ class "" ]
                            [ input [ name "backgroundSelect", type_ "radio", value "gallenteHangar" ]
                                []
                            , text "Room [WIP]"
                            ]
                        , ul [ class "" ]
                            [ input [ name "backgroundSelect", type_ "radio", value "gallenteBalconyStill" ]
                                []
                            , text "Still Image"
                            ]
                        ]
                    , div [ class "modalFlex" ]
                        [ div [ class "radioButtonTitle" ]
                            [ text "Amarr" ]
                        , ul [ class "" ]
                            [ input [ name "backgroundSelect", type_ "radio", value "amarrBalcony" ]
                                []
                            , text "Balcony [WIP]"
                            ]
                        , ul [ class "" ]
                            [ input [ name "backgroundSelect", type_ "radio", value "amarrHangar" ]
                                []
                            , text "Hangar [WIP]"
                            ]
                        , ul [ class "" ]
                            [ input [ name "backgroundSelect", type_ "radio", value "amarrHangar" ]
                                []
                            , text "Room [WIP]"
                            ]
                        , ul [ class "" ]
                            [ input [ name "backgroundSelect", type_ "radio", value "amarrBalconyStill" ]
                                []
                            , text "Still Image"
                            ]
                        ]
                    ]
                , div [ class "modalFlexContainer" ]
                    [ div [ class "modalFlex" ]
                        [ div [ class "radioButtonTitle" ]
                            [ text "Caldari" ]
                        , ul [ class "" ]
                            [ input [ name "backgroundSelect", type_ "radio", value "caldariBalcony" ]
                                []
                            , text "Balcony [WIP]"
                            ]
                        , ul [ class "" ]
                            [ input [ name "backgroundSelect", type_ "radio", value "caldariHangar" ]
                                []
                            , text "Hangar [WIP]"
                            ]
                        , ul [ class "" ]
                            [ input [ name "backgroundSelect", type_ "radio", value "caldariHangar" ]
                                []
                            , text "Room [WIP]"
                            ]
                        , ul [ class "" ]
                            [ input [ name "backgroundSelect", type_ "radio", value "amarrBalconyStill" ]
                                []
                            , text "Still Image"
                            ]
                        ]
                    , div [ class "modalFlex", id "flexFinal" ]
                        [ text "More backgrounds soon!          " ]
                    ]
                , hr []
                    []
                , div [ id "ApplyBtnContainer" ]
                    [ a [ href "#modal-close" ]
                        [ button [ class "ApplyButton" ]
                            [ text "Apply" ]
                        ]
                    ]
                ]
            ]
        ]


videoDiv : Html msg
videoDiv =
    div [ id "videoWrapper" ]
        [ video [ id "film", style "z-index" "8", autoplay True, loop True, attribute "muted" "" ]
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
