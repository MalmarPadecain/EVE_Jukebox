module Playlist exposing (Order(..), OrderBy(..), PlayOrder, Playlist, PlaylistCore, Song, chooseSong, flipDisplayOrder, getOrderBy, next, orderBy, playlistDecoder, playlistListDecoder, previous, unshuffle)

import Json.Decode exposing (Decoder, andThen, fail, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import List exposing (indexedMap, reverse, sortBy)


type alias ProtoSong =
    { name : String
    , link : String
    , duration : String
    }


type alias Song =
    { index : Int
    , name : String
    , duration : String
    , link : String
    }


type alias PlayOrder =
    { current : Song
    , previous : List Song
    , next : List Song
    }


type alias PlaylistCore =
    { name : String
    , link : String
    }


type alias Playlist =
    { progress : Float
    , playing : Bool
    , core : PlaylistCore
    , order : PlayOrder
    , displayOrder : List Song
    , orderedBy : Order
    }


type OrderBy
    = Number
    | Title
    | Duration


type Order
    = Asc OrderBy
    | Desc OrderBy


flip : Order -> Order
flip order =
    case order of
        Asc a ->
            Desc a

        Desc a ->
            Asc a


getOrderBy : Order -> OrderBy
getOrderBy order =
    case order of
        Asc a ->
            a

        Desc a ->
            a


orderBy : Playlist -> OrderBy -> Playlist
orderBy playlist by =
    if by == getOrderBy playlist.orderedBy then
        flipDisplayOrder playlist

    else
        case by of
            Number ->
                { playlist
                    | displayOrder = List.sortBy .index playlist.displayOrder
                    , orderedBy = Asc by
                }

            Title ->
                { playlist
                    | displayOrder = List.sortBy .name playlist.displayOrder
                    , orderedBy = Asc by
                }

            Duration ->
                { playlist
                    | displayOrder = List.sortBy .duration playlist.displayOrder
                    , orderedBy = Asc by
                }


{-| Helper to create Playlists without the need to create PlayOrder manually
-}
makePlaylist : String -> String -> List ProtoSong -> Result String Playlist
makePlaylist pName pLink protoSongs =
    let
        songs =
            indexedMap (\i { name, link, duration } -> Song i name duration link) protoSongs
    in
    case songs of
        [] ->
            Err "songs must not be empty."

        s :: ss ->
            Ok
                { progress = 0
                , playing = False
                , core =
                    { name = pName
                    , link = pLink
                    }
                , order =
                    { current = s
                    , previous = reverse ss
                    , next = ss
                    }
                , displayOrder = indexedMap (\i { name, link, duration } -> Song i name duration link) protoSongs
                , orderedBy = Asc Number
                }


{-| Orders songs by index
-}
unshuffle : Playlist -> Playlist
unshuffle ({ order } as playlist) =
    let
        sortedSongList =
            sortBy .index (order.current :: order.next)

        newPL =
            case sortedSongList of
                [] ->
                    playlist

                x :: xs ->
                    { playlist
                        | order =
                            { current = x
                            , next = xs
                            , previous = reverse xs
                            }
                    }
    in
    chooseSong newPL order.current


{-| Moves the current song to the head of the next list.
If order.next is empty it returns the playlist unaltered
-}
next : Playlist -> Playlist
next playlist =
    case playlist.order.next of
        x :: xs ->
            { playlist
                | order =
                    { current = x
                    , previous = playlist.order.current :: reverse xs
                    , next = xs ++ [ playlist.order.current ]
                    }
            }

        [] ->
            playlist


{-| Moves the current song to the head of the previous list.
If order.previous is empty it returns the playlist unaltered
-}
previous : Playlist -> Playlist
previous playlist =
    case playlist.order.previous of
        x :: xs ->
            { playlist
                | order =
                    { current = x
                    , previous = xs ++ [ playlist.order.current ]
                    , next = playlist.order.current :: reverse xs
                    }
            }

        [] ->
            playlist


{-| Sets current to song.
If song is not in the list this will result in infinite recursion!
-}
chooseSong : Playlist -> Song -> Playlist
chooseSong playlist song =
    if playlist.order.current == song then
        playlist

    else
        chooseSong (next playlist) song


{-| Reverse displayed order
-}
flipDisplayOrder : Playlist -> Playlist
flipDisplayOrder playlist =
    { playlist | orderedBy = flip playlist.orderedBy, displayOrder = reverse playlist.displayOrder }


protoSongDecoder : Decoder ProtoSong
protoSongDecoder =
    succeed ProtoSong
        |> required "name" string
        |> required "link" string
        |> required "duration" string


{-| Decoder takes playlist name and link.
-}
playlistDecoder : String -> String -> Decoder Playlist
playlistDecoder name link =
    succeed makePlaylist
        |> hardcoded name
        |> hardcoded link
        |> required "songs" (list protoSongDecoder)
        |> andThen
            (\result ->
                case result of
                    Err s ->
                        fail s

                    Ok pl ->
                        succeed pl
            )


playlistListDecoder : Decoder (List PlaylistCore)
playlistListDecoder =
    list
        (succeed PlaylistCore
            |> required "name" string
            |> required "file" string
        )
