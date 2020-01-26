module View exposing (renderTable, view)

import Browser
import Core exposing (..)
import Draggable
import Html exposing (Html, a, audio, b, br, button, div, h1, h2, h3, hr, img, input, li, p, table, tbody, td, text, th, tr, ul, video)
import Html.Attributes exposing (attribute, autoplay, checked, class, href, id, loop, name, src, step, style, title, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Html.Lazy exposing (lazy)
import Json.Decode as Decode
import Playlist exposing (..)


view : Model -> Browser.Document Msg
view model =
    case model of
        Ok { playlist, volume, dragState, shuffled, playlistList, selectedBackground, appliedBackground } ->
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
                                    , Html.Attributes.max "100"
                                    , Html.Attributes.min "0"
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
                        [ ul [ class "PlaylistContainer" ]
                            (renderPlaylistList playlistList playlist)
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
                    , optionWindow selectedBackground
                    , audio
                        [ id "audio"
                        , on "ended" (Decode.succeed Next)
                        ]
                        []
                    ]
                , videoDiv appliedBackground
                , div [ class "overviewWrapper", id "overviewWrapperNeocomUpper" ] []
                , div [ class "overviewWrapper", id "overviewWrapperNeocomLower" ] []
                , div [ class "overviewWrapper", id "overviewWrapperNeocomBackground" ] []
                , div [ class "overviewWrapper", id "overviewWrapperLocal" ] []
                , div [ class "overviewWrapper", id "overviewWrapperStation" ] []
                ]
            }

        Err msg ->
            { title = "Jukebox"
            , body = [ text msg ]
            }


renderPlaylistList : List PlaylistCore -> Playlist -> List (Html Msg)
renderPlaylistList list pl =
    List.map
        (\plc ->
            if plc.name == pl.core.name then
                li [ class "Playlist", id "selectedPlaylist", onClick (LoadPlaylist plc) ] [ text plc.name ]

            else
                li [ class "Playlist", onClick (LoadPlaylist plc) ] [ text plc.name ]
        )
        list


{-| Turns a list into a div with a header table and a content table.
-}
renderTable : Playlist -> Html Msg
renderTable playlist =
    let
        order =
            playlist.orderedBy

        orderedBy =
            getOrderBy order

        orderSign =
            case order of
                Asc _ ->
                    "🞃"

                Desc _ ->
                    "🞁"

        numberText =
            if orderedBy == Number then
                "Number\u{00A0}" ++ orderSign

            else
                "Number"

        titleText =
            if orderedBy == Title then
                "Title\u{00A0}" ++ orderSign

            else
                "Title"

        durationText =
            if orderedBy == Duration then
                "Duration " ++ orderSign

            else
                "Duration"

        -- The final row of the content table.
        -- This is needed to set the width of all cells
        -- and to make sure it fills the entirety of the remaining space in small playlists
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
                        if song == playlist.order.current then
                            tr [ onClick (ChooseSong song), id "selectedSong" ]
                                [ td [] [ text "►" ]
                                , td [] [ text (String.fromInt (song.index + 1)) ]
                                , td [] [ text song.name ]
                                , td [] [ text song.duration ]
                                , td [] []
                                ]

                        else
                            tr [ onClick (ChooseSong song) ]
                                [ td [] []
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


optionWindow : String -> Html Msg
optionWindow backgroundLink =
    let
        viewRadio : { radioName : String, link : String } -> Html Msg
        viewRadio { radioName, link } =
            li [ class "" ]
                [ input
                    [ name "backgroundSelect"
                    , type_ "radio"
                    , onInput (\_ -> SelectBackground <| link)
                    , if backgroundLink == link then
                        checked True

                      else
                        checked False
                    ]
                    []
                , text radioName
                ]

        viewRadioSet : ( String, List { radioName : String, link : String } ) -> Html Msg
        viewRadioSet ( radioName, backgrounds ) =
            div [ class "modalFlex" ]
                [ h3 [ class "radioButtonTitle" ]
                    [ text radioName ]
                , ul [] <| List.map viewRadio backgrounds
                ]
    in
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
                    [ text "Here you can select the background that you see behind the jukebox as captured 2011 style. We recommend a still image if you're on a limited-data connection." ]
                , hr []
                    []
                , h2 []
                    [ text "Video Backgrounds" ]
                , div [ class "modalFlexContainer" ] <|
                    List.map viewRadioSet Core.possibleBackgrounds
                , hr []
                    []
                , div [ id "ApplyBtnContainer" ]
                    [ a [ href "#modal-close" ]
                        [ button [ class "ApplyButton", onClick ApplyBackground ]
                            [ text "Apply" ]
                        ]
                    ]
                ]
            ]
        ]


videoDiv : String -> Html msg
videoDiv link =
    if String.contains ".png" link then
        div [ id "videoWrapper" ]
            [ img [ id "still", style "z-index" "8" ] [] ]

    else
        div [ id "videoWrapper" ]
            [ video [ id "video", style "z-index" "8", autoplay True, loop True, attribute "muted" "true" ] [] ]


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
