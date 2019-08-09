module PlaylistTest exposing (afterOneNext, afterTwoNext, emptyPlaylist, initOrder, initPlaylist, song0, song1, song2, song3, song4, song5, song6, songList, testDecoder, testNext)

import Expect
import Json.Decode as Decode
import Playlist exposing (..)
import Test exposing (Test, describe, test)


song0 =
    { index = 0, name = "Song 0", duration = "0", link = "" }


song1 =
    { index = 1, name = "Song 1", duration = "1", link = "" }


song2 =
    { index = 2, name = "Song2", duration = "2", link = "" }


song3 =
    { index = 3, name = "Song3", duration = "3", link = "" }


song4 =
    { index = 4, name = "Song4", duration = "4", link = "" }


song5 =
    { index = 5, name = "Song5", duration = "5", link = "" }


song6 =
    { index = 6, name = "Song6", duration = "6", link = "" }


songList : List Song
songList =
    [ song0
    , song1
    , song2
    , song3
    , song4
    , song5
    , song6
    ]


initOrder : PlayOrder
initOrder =
    { current = song0
    , previous = [ song6, song5, song4, song3, song2, song1 ]
    , next = [ song1, song2, song3, song4, song5, song6 ]
    }


afterOneNext : PlayOrder
afterOneNext =
    { current = song1
    , previous = [ song0, song6, song5, song4, song3, song2 ]
    , next = [ song2, song3, song4, song5, song6, song0 ]
    }


afterTwoNext : PlayOrder
afterTwoNext =
    { current = song2
    , previous = [ song1, song0, song6, song5, song4, song3 ]
    , next = [ song3, song4, song5, song6, song0, song1 ]
    }


emptyPlaylist : Playlist
emptyPlaylist =
    { progress = 0
    , playing = False
    , core = { name = "emptyPlaylist", link = "" }
    , order = { current = song0, previous = [], next = [] }
    , displayOrder = [ song0 ]
    }


initPlaylist : Playlist
initPlaylist =
    { progress = 0
    , playing = False
    , core = { name = "initPlaylist", link = "" }
    , order = initOrder
    , displayOrder = songList
    }


testNext : Test
testNext =
    describe "The PlaylistTest Module"
        [ describe "PlaylistTest.next"
            [ test "PlaylistTest with only one Element" <|
                \_ ->
                    emptyPlaylist
                        |> next
                        |> Expect.equal emptyPlaylist
            , test "after one next" <|
                \_ ->
                    initPlaylist
                        |> next
                        |> Expect.equal { initPlaylist | order = afterOneNext }
            , test "after two next" <|
                \_ ->
                    initPlaylist
                        |> next
                        |> next
                        |> Expect.equal { initPlaylist | order = afterTwoNext }
            ]
        ]


testDecoder : Test
testDecoder =
    let
        emptyJSON =
            """{"name": "empty", "songs": []}"""

        wellformedJSON =
            """{
                              "name": "Cinematic Themes",
                              "songs": [
                                {
                                  "name": "The End of Peace (Revelations)",
                                  "link": "https://www.modenstudios.com/EVE/music/The%20End%20of%20Peace.mp3",
                                  "duration": "1:37"
                                },
                                {
                                  "name": "Rebirth (Trinity)",
                                  "link": "https://www.modenstudios.com/EVE/music/Rebirth.mp3",
                                  "duration": "2:14"
                                }]}"""
    in
    describe "The Playlist Decoder"
        [ test "Given an empty List" <|
            \_ ->
                Decode.decodeString (playlistDecoder "empty" "link") emptyJSON
                    |> Expect.err
        , test "Given a well formed JSON" <|
            \_ ->
                Decode.decodeString (playlistDecoder "Cinematic Themes" "link") wellformedJSON
                    |> Expect.equal
                        (Ok
                            { progress = 0
                            , playing = False
                            , core =
                                { name = "Cinematic Themes"
                                , link = "link"
                                }
                            , order =
                                { current =
                                    { index = 0
                                    , name = "The End of Peace (Revelations)"
                                    , duration = "1:37"
                                    , link = "https://www.modenstudios.com/EVE/music/The%20End%20of%20Peace.mp3"
                                    }
                                , previous =
                                    [ { index = 1
                                      , name = "Rebirth (Trinity)"
                                      , duration = "2:14"
                                      , link = "https://www.modenstudios.com/EVE/music/Rebirth.mp3"
                                      }
                                    ]
                                , next =
                                    [ { index = 1
                                      , name = "Rebirth (Trinity)"
                                      , duration = "2:14"
                                      , link = "https://www.modenstudios.com/EVE/music/Rebirth.mp3"
                                      }
                                    ]
                                }
                            , displayOrder =
                                [ { index = 0
                                  , name = "The End of Peace (Revelations)"
                                  , duration = "1:37"
                                  , link = "https://www.modenstudios.com/EVE/music/The%20End%20of%20Peace.mp3"
                                  }
                                , { index = 1
                                  , name = "Rebirth (Trinity)"
                                  , duration = "2:14"
                                  , link = "https://www.modenstudios.com/EVE/music/Rebirth.mp3"
                                  }
                                ]
                            }
                        )
        ]
