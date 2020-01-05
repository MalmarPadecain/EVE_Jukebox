module Core exposing (Model, Msg(..), possibleBackgrounds)

import Draggable
import Http
import Playlist exposing (..)


type alias DragState =
    { position : ( Int, Int )
    , drag : Draggable.State String
    }


type alias Model =
    Result String
        { playlist : Playlist
        , volume : Float
        , dragState : DragState
        , shuffled : Bool
        , playlistList : List PlaylistCore
        , selectedBackground : String
        , appliedBackground : String
        }


type Msg
    = PlaylistsLoaded (Result Http.Error (List PlaylistCore))
    | Next
    | Previous
    | Shuffle
    | Shuffled (List Song)
    | Order OrderBy
    | LoadPlaylist PlaylistCore
    | PlaylistLoaded (Result Http.Error Playlist)
    | ChooseSong Song
    | ChangeVolume Float
    | TogglePause
    | SelectBackground String
    | ApplyBackground
    | OnDragBy Draggable.Delta
    | DragMsg (Draggable.Msg String)
    | Progress Float


possibleBackgrounds :
    List
        ( String
        , List
            { radioName : String
            , link : String
            }
        )
possibleBackgrounds =
    let
        path =
            "./video/"
    in
    [ ( "Minmatar"
      , [ { radioName = "Balcony"
          , link = path ++ "minmatarBalcony.mp4"
          }
        , { radioName = "Hangar [WIP]"
          , link = path ++ "minmatarHangar.mp4"
          }
        , { radioName = "Room [WIP]"
          , link = path ++ "minmatarRoom.mp4"
          }
        , { radioName = "Still Image"
          , link = path ++ "minmatarStill.png"
          }
        ]
      )
    , ( "Gallente"
      , [ { radioName = "Balcony"
          , link = path ++ "gallenteBalcony.mp4"
          }
        , { radioName = "Hangar [WIP]"
          , link = path ++ "gallenteHangar.mp4"
          }
        , { radioName = "Room [WIP]"
          , link = path ++ "gallenteRoom.mp4"
          }
        , { radioName = "Still Image"
          , link = path ++ "gallenteStill.png"
          }
        ]
      )
    , ( "Amarr"
      , [ { radioName = "Balcony [WIP]"
          , link = path ++ "amarrBalcony.mp4"
          }
        , { radioName = "Hangar [WIP]"
          , link = path ++ "amarrHangar.mp4"
          }
        , { radioName = "Room [WIP]"
          , link = path ++ "amarrRoom.mp4"
          }
        , { radioName = "Still Image"
          , link = path ++ "amarrStill.png"
          }
        ]
      )
    , ( "Caldari"
      , [ { radioName = "Balcony [WIP]"
          , link = path ++ "caldariBalcony.mp4"
          }
        , { radioName = "Hangar [WIP]"
          , link = path ++ "caldariHangar.mp4"
          }
        , { radioName = "Room [WIP]"
          , link = path ++ "caldariRoom.mp4"
          }
        , { radioName = "Still Image"
          , link = path ++ "caldariStill.png"
          }
        ]
      )
    ]
