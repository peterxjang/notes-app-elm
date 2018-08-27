module Notes exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)


main =
    div [ id "app" ]
        [ div [ class "toolbar" ]
            [ button [ class "toolbar-button" ] [ text "New" ]
            , button [ class "toolbar-button" ] [ text "Delete" ]
            , input [ class "toolbar-search", type_ "text", placeholder "Search..." ] []
            ]
        , div [ class "note-container" ]
            [ div [ class "note-selectors" ]
                [ div [ class "note-selector active" ]
                    [ p [ class "note-selector-title" ] [ text "First note..." ]
                    , p [ class "note-selector-timestamp" ] [ text "Timestamp here..." ]
                    ]
                , div [ class "note-selector" ]
                    [ p [ class "note-selector-title" ] [ text "Second note..." ]
                    , p [ class "note-selector-timestamp" ] [ text "Timestamp here..." ]
                    ]
                , div [ class "note-selector" ]
                    [ p [ class "note-selector-title" ] [ text "Third note..." ]
                    , p [ class "note-selector-timestamp" ] [ text "Timestamp here..." ]
                    ]
                ]
            , div [ class "note-editor" ]
                [ p [ class "note-editor-info" ] [ text "Timestamp here..." ]
                , textarea [ class "note-editor-input" ] [ text "First note..." ]
                ]
            ]
        ]
