module Main exposing (main)

import Array
import Browser
import Html exposing (Html, button, div, li, text, ul)
import Html.Events exposing (onClick)
import Maybe
import Random exposing (generate)
import Random.Array exposing (shuffle)


type alias Song =
    { name : String
    , duration : String
    , link : String
    }


type alias Playlist = Array.Array Song


type alias Model =
    { playlist : Playlist
    , index : Int
    }


type Msg
    = Next
    | Previous
    | Shuffle
    | Shuffled (Array.Array Song)


songList : Playlist
songList =
    Array.fromList [ { name = "Unidentified Phenomenon"
    , duration = "4:30"
    , link = "https://www.modenstudios.com/EVE/music/Unidentified%20Phenomenon.mp3"}
    ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { playlist = songList
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
            if model.index == Array.length model.playlist - 1 then
                ( { playlist = model.playlist, index = 0 }, Cmd.batch [] )

            else
                ( { playlist = model.playlist, index = model.index + 1 }, Cmd.batch [] )

        Previous ->
            if model.index == 0 then
                ( { playlist = model.playlist, index = Array.length model.playlist - 1 }, Cmd.batch [] )

            else
                ( { playlist = model.playlist, index = model.index - 1 }, Cmd.batch [] )

        Shuffle ->
            ( model, generate Shuffled (shuffle model.playlist) )

        Shuffled shuffledList ->
            ( { playlist = shuffledList, index = 0 }, Cmd.batch [] )


renderList : Model -> Html msg
renderList model =
    model.playlist
        |> Array.toList
        |> List.map (\l -> li [] [ text l.name ])
        |> ul []


currentlyPlaying : Model -> Song
currentlyPlaying model =
    case Array.get model.index model.playlist of
        Nothing ->
            {name="", link="", duration=""}
        Just song ->
            song

view : Model -> Browser.Document Msg
view model =
    { title = "Jukebox"
    , body =
        [ button [ onClick Shuffle ] [ text "Shuffle" ]
        , button [ onClick Previous ] [ text "-" ]
        , renderList model
        , div [] [ text ((currentlyPlaying model).name ++ " " ++ String.fromInt model.index) ]
        , button [ onClick Next ] [ text "+" ]
        ]
    }
