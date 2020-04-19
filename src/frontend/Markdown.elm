-- Content of this module is taken from https://ellie-app.com/8kHgbSLfhfha1
--


module Markdown exposing (elmUiRenderer)

import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Element.Region
import Html
import Html.Attributes
import Markdown.Block
import Markdown.Html
import Markdown.Renderer
import SyntaxHighlight
    exposing
        ( css
        , elm
        , gitHub
        , javascript
        , json
        , python
        , sql
        , toInlineHtml
        , useTheme
        , xml
        )


elmUiRenderer : Markdown.Renderer.Renderer (Element.Element msg)
elmUiRenderer =
    { heading = heading
    , paragraph =
        Element.paragraph
            [ Element.spacing 15 ]
    , thematicBreak = Element.none
    , text = Element.text
    , strong = \content -> Element.row [ Element.Font.bold ] content
    , emphasis = \content -> Element.row [ Element.Font.italic ] content
    , codeSpan = code
    , link =
        \{ title, destination } body ->
            Element.newTabLink
                [ Element.htmlAttribute (Html.Attributes.style "display" "inline-flex") ]
                { url = destination
                , label =
                    Element.paragraph
                        [ Element.Font.color (Element.rgb255 0 0 255)
                        ]
                        body
                }
    , hardLineBreak = Html.br [] [] |> Element.html
    , image =
        \image ->
            Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }
    , blockQuote =
        \children ->
            Element.column
                [ Element.Border.widthEach { top = 0, right = 0, bottom = 0, left = 10 }
                , Element.padding 10
                , Element.Border.color (Element.rgb255 145 145 145)
                , Element.Background.color (Element.rgb255 245 245 245)
                ]
                children
    , unorderedList =
        \items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.map
                        (\(Markdown.Block.ListItem task children) ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.row
                                    [ Element.alignTop ]
                                    ((case task of
                                        Markdown.Block.IncompleteTask ->
                                            Element.Input.defaultCheckbox False

                                        Markdown.Block.CompletedTask ->
                                            Element.Input.defaultCheckbox True

                                        Markdown.Block.NoTask ->
                                            Element.text "•"
                                     )
                                        :: Element.text " "
                                        :: children
                                    )
                                ]
                        )
                )
    , orderedList =
        \startingIndex items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.row [ Element.alignTop ]
                                    (Element.text (String.fromInt (index + startingIndex) ++ " ") :: itemBlocks)
                                ]
                        )
                )
    , codeBlock = codeBlock
    , html = Markdown.Html.oneOf []
    , table = Element.column []
    , tableHeader = Element.column []
    , tableBody = Element.column []
    , tableRow = Element.row []
    , tableHeaderCell =
        \maybeAlignment children ->
            Element.paragraph [] children
    , tableCell = Element.paragraph []
    }


code : String -> Element.Element msg
code snippet =
    Element.el
        [ Element.Background.color
            (Element.rgba 0 0 0 0.04)
        ]
    <|
        Element.html <|
            Html.div []
                [ useTheme gitHub
                , python snippet
                    |> Result.map toInlineHtml
                    |> Result.withDefault
                        (Html.pre [] [ Html.code [] [ Html.text snippet ] ])
                ]


codeBlock : { body : String, language : Maybe String } -> Element.Element msg
codeBlock details =
    Element.el
        [ Element.Background.color (Element.rgba 0 0 0 0.03)
        , Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
        , Element.spacing 10
        , Element.padding 10
        , Element.Font.family
            [ Element.Font.external
                { url = "https://fonts.googleapis.com/css?family=Fira+Mono"
                , name = "Fira Mono"
                }
            ]
        ]
    <|
        let
            languagePreset =
                case details.language of
                    Just "css" ->
                        css

                    Just "elm" ->
                        elm

                    Just "javascript" ->
                        javascript

                    Just "json" ->
                        json

                    Just "python" ->
                        python

                    Just "sql" ->
                        sql

                    Just "xml" ->
                        xml

                    _ ->
                        python
        in
        Element.html <|
            Html.div []
                [ useTheme gitHub
                , languagePreset details.body
                    |> Result.map toInlineHtml
                    |> Result.withDefault
                        (Html.pre [] [ Html.code [] [ Html.text details.body ] ])
                ]


heading : { level : Markdown.Block.HeadingLevel, rawText : String, children : List (Element.Element msg) } -> Element.Element msg
heading { level, rawText, children } =
    Element.paragraph
        [ Element.Font.size
            (case level of
                Markdown.Block.H1 ->
                    36

                Markdown.Block.H2 ->
                    24

                _ ->
                    20
            )
        , Element.Font.bold
        , Element.Region.heading (Markdown.Block.headingLevelToInt level)
        , Element.htmlAttribute
            (Html.Attributes.attribute "name" (rawTextToId rawText))
        , Element.htmlAttribute
            (Html.Attributes.id (rawTextToId rawText))
        ]
        children


rawTextToId rawText =
    rawText
        |> String.split " "
        |> String.join "-"
        |> String.toLower
