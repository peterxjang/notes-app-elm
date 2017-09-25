module Notes exposing (..)

import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Time


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Note =
    { id : Int, body : String, timestamp : Float }


type alias Model =
    { notes : List Note, selectedNoteId : Int }


init : ( Model, Cmd Msg )
init =
    ( { notes =
            [ { id = 1, body = "First note...", timestamp = 0 }
            , { id = 2, body = "Second note...", timestamp = 0 }
            , { id = 3, body = "Third note...", timestamp = 0 }
            ]
      , selectedNoteId = 1
      }
    , Task.perform InitializeTimestamps Time.now
    )



-- UPDATE


type Msg
    = InitializeTimestamps Time.Time
    | SelectNote Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitializeTimestamps time ->
            ( { model
                | notes = List.map (\note -> { note | timestamp = time }) model.notes
              }
            , Cmd.none
            )

        SelectNote id ->
            ( { model | selectedNoteId = id }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "app" ]
        [ div [ class "toolbar" ]
            [ button [ class "toolbar-button" ] [ text "New" ]
            , button [ class "toolbar-button" ] [ text "Delete" ]
            , input [ class "toolbar-search", type_ "text", placeholder "Search..." ] []
            ]
        , div [ class "note-container" ]
            [ viewNoteSelectors model
            , viewNoteEditor model
            ]
        ]


viewNoteSelectors : Model -> Html Msg
viewNoteSelectors model =
    div [ class "note-selectors" ]
        (model.notes
            |> List.sortBy .timestamp
            |> List.reverse
            |> List.map (\note -> viewNoteSelector note model.selectedNoteId)
        )


viewNoteSelector : Note -> Int -> Html Msg
viewNoteSelector note selectedNoteId =
    div [ classList [ ( "note-selector", True ), ( "active", note.id == selectedNoteId ) ], onClick (SelectNote note.id) ]
        [ p [ class "note-selector-title" ] [ text (formatTitle note.body) ]
        , p [ class "note-selector-timestamp" ] [ text (formatTimestamp note.timestamp) ]
        ]


viewNoteEditor : Model -> Html Msg
viewNoteEditor model =
    case model.notes |> List.filter (\note -> note.id == model.selectedNoteId) |> List.head of
        Nothing ->
            div [ class "note-editor" ] []

        Just selectedNote ->
            div [ class "note-editor" ]
                [ p [ class "note-editor-info" ] [ text (formatTimestamp selectedNote.timestamp) ]
                , textarea [ class "note-editor-input" ] [ text selectedNote.body ]
                ]


formatTitle : String -> String
formatTitle body =
    let
        maxLength =
            20

        length =
            String.length body
    in
    if length > maxLength then
        String.left (maxLength - 3) body ++ "..."
    else if length == 0 then
        "New note"
    else
        body


formatTimestamp : Float -> String
formatTimestamp timestamp =
    toString (Date.fromTime timestamp)
        |> String.slice 1 -16
