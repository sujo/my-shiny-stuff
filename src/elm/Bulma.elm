module Bulma exposing (viewTextField, viewButton, viewErrors, hero)

import Html exposing (Html, div, h2, text, input, button, img, p)
import Html.Attributes exposing (class, src, value)
import Html.Events exposing (onClick, onInput)

-- View functions using bulma.io classes and structures.

{-| Render dismissable errors. We use this all over the place!
-}
viewErrors : msg -> List String -> Html msg
viewErrors dismissErrors errors =
    if List.isEmpty errors then
        div [] []

    else
        div
            [ class "message is-danger"
            -- , style "position" "fixed"
            -- , style "top" "0"
            ] [
               div [ class "message-header" ] [
                  text "Error",
                  button [ class "delete", onClick dismissErrors ] []
               ],
               div [ class "message-body" ]
               <|
                  List.map (\error -> p [] [ text error ]) errors
            ]


viewTextField : String -> String -> (String -> msg) -> Html msg
viewTextField label init msg =
    div [ class "field" ]
        [ Html.label [ class "label" ] [ text label ]
        , div [ class "control" ]
            [ input [ class "input", value init, onInput msg ] [] ]
        ]

viewButton : String -> String -> msg -> Html msg
viewButton label cls msg =
    div [ class "field" ]
        [ div [ class "control" ]
            [ button [ class ("button " ++ cls), onClick msg ] [ text label ] ] ]


hero : String -> Html msg
hero title =
   div [ class "hero" ]
      [ div [ class "hero-body" ]
         [ div [ class "level" ]
            [ img [ class "image is-128x128", src "https://render.guildwars2.com/file/A1BD345AD9192C3A585BE2F6CB0617C5A797A1E2/619317.png" ] []
            , h2 [ class "title is-2" ] [ text title ]
            , img [ class "image is-128x128", src "https://static.staticwars.com/quaggans/quaggan.jpg" ] []
            ]
         ]
      ]
