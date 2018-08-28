module Notes exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Time


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Note =
    { id : Int, body : String, timestamp : Int }


type alias Model =
    { notes : List Note, selectedNoteId : Int, searchNoteText : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { notes =
            [ { id = 1, body = "First note...", timestamp = 0 }
            , { id = 2, body = "Second note...", timestamp = 0 }
            , { id = 3, body = "Third note...", timestamp = 0 }
            ]
      , selectedNoteId = 1
      , searchNoteText = ""
      }
    , Task.perform InitializeNotesTimestamps Time.now
    )



-- UPDATE


type Msg
    = InitializeNotesTimestamps Time.Posix
    | SelectNote Int
    | UpdateSelectedNoteBody String
    | UpdateSelectedNoteTimestamp Time.Posix
    | ClickNew
    | CreateNote Time.Posix
    | ClickDelete
    | InputSearch String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitializeNotesTimestamps time ->
            ( { model
                | notes = List.map (\note -> { note | timestamp = Time.posixToMillis time }) model.notes
              }
            , Cmd.none
            )

        SelectNote id ->
            ( { model | selectedNoteId = id }, Cmd.none )

        UpdateSelectedNoteBody newText ->
            case getSelectedNote model of
                Nothing ->
                    ( model, Cmd.none )

                Just selectedNote ->
                    let
                        updateSelectedNote note =
                            if note.id == model.selectedNoteId then
                                { note | body = newText }

                            else
                                note

                        newNotes =
                            List.map updateSelectedNote model.notes
                    in
                    ( { model | notes = newNotes }
                    , Task.perform UpdateSelectedNoteTimestamp Time.now
                    )

        UpdateSelectedNoteTimestamp newTime ->
            case getSelectedNote model of
                Nothing ->
                    ( model, Cmd.none )

                Just selectedNote ->
                    let
                        updateSelectedNote note =
                            if note.id == model.selectedNoteId then
                                { note | timestamp = Time.posixToMillis newTime }

                            else
                                note

                        newNotes =
                            List.map updateSelectedNote model.notes
                    in
                    ( { model | notes = newNotes }, Cmd.none )

        ClickNew ->
            ( model, Task.perform CreateNote Time.now )

        CreateNote newTime ->
            let
                newTimestamp =
                    Time.posixToMillis newTime

                newId =
                    newTimestamp
            in
            ( { model
                | notes = [ { id = newId, body = "", timestamp = newTimestamp } ] ++ model.notes
                , selectedNoteId = newId
              }
            , Cmd.none
            )

        ClickDelete ->
            let
                newNotes =
                    List.filter (\note -> note.id /= model.selectedNoteId) model.notes

                firstVisibleNote =
                    getFirstVisibleNote newNotes model.searchNoteText
            in
            case firstVisibleNote of
                Nothing ->
                    ( { model | notes = newNotes }, Cmd.none )

                Just availableNote ->
                    ( { model | notes = newNotes, selectedNoteId = availableNote.id }, Cmd.none )

        InputSearch searchNoteText ->
            let
                firstVisibleNote =
                    getFirstVisibleNote model.notes searchNoteText
            in
            case firstVisibleNote of
                Nothing ->
                    ( { model | searchNoteText = searchNoteText, selectedNoteId = -1 }, Cmd.none )

                Just availableNote ->
                    ( { model | searchNoteText = searchNoteText, selectedNoteId = availableNote.id }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "app" ]
        [ div [ class "toolbar" ]
            [ button [ class "toolbar-button", onClick ClickNew ] [ text "New" ]
            , button [ class "toolbar-button", onClick ClickDelete ] [ text "Delete" ]
            , input [ class "toolbar-search", type_ "text", placeholder "Search...", onInput InputSearch ] []
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
            |> transformNotes model.searchNoteText
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
    case getSelectedNote model of
        Nothing ->
            div [ class "note-editor" ] []

        Just selectedNote ->
            div [ class "note-editor" ]
                [ p [ class "note-editor-info" ] [ text (formatTimestamp selectedNote.timestamp) ]
                , textarea [ class "note-editor-input", onInput UpdateSelectedNoteBody, value selectedNote.body ] []
                ]



-- HELPERS


getFirstVisibleNote : List Note -> String -> Maybe Note
getFirstVisibleNote notes searchText =
    notes
        |> transformNotes searchText
        |> List.head


transformNotes : String -> List Note -> List Note
transformNotes searchNoteText notes =
    notes
        |> List.filter (\note -> String.contains (String.toLower searchNoteText) (String.toLower note.body))
        |> List.sortBy .timestamp
        |> List.reverse


getSelectedNote : Model -> Maybe Note
getSelectedNote model =
    model.notes
        |> transformNotes model.searchNoteText
        |> List.filter (\note -> note.id == model.selectedNoteId)
        |> List.head


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


formatTimestamp : Int -> String
formatTimestamp timestamp =
    let
        time =
            Time.millisToPosix timestamp

        hour =
            String.fromInt (Time.toHour Time.utc time)

        minute =
            String.fromInt (Time.toMinute Time.utc time)

        second =
            String.fromInt (Time.toSecond Time.utc time)
    in
    hour ++ ":" ++ minute ++ ":" ++ second ++ " UTC"
