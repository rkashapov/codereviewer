module Main exposing (..)

import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation exposing (Key)
import Data exposing (Viewer)
import Element exposing (..)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Element.Keyed
import Element.Lazy
import Html
import Markdown
import Markdown.Parser
import Markdown.Renderer
import Parser as TextParser exposing ((|.), (|=))
import RemoteData
import SyntaxHighlight
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, parse, s, string)


main : Program Flags Model Msg
main =
    application
        { init = init
        , update = update
        , view = view
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        , subscriptions = subscriptions
        }


type alias Model =
    { title : String
    , maybeViewer : Maybe Viewer
    , comparison : Comparison
    , editingComment : Maybe ( String, Int, List Comment )
    , apiUrl : String
    , navigationKey : Key
    }


type alias Flags =
    String


type alias ParseResult =
    { owner : String
    , repository : String
    , baseAndHead : String
    }


type Msg
    = DataMsg Data.Msg
    | AddComment
    | EditComment String
    | CancelEdit
    | StartEditComment String Int


type CommentState
    = Displayed
    | Editing


type alias Comparison =
    { changes : List Diff
    , commits : List Data.Commit
    }


type alias Diff =
    { sha : String
    , filename : String
    , blobUrl : String
    , patch : List PatchLine
    }


type PatchLine
    = Header PatchInfo String
    | Code LineOfCode


type alias LineOfCode =
    { lineType : LineType
    , linenoBase : Int
    , linenoHead : Int
    , code : String
    , comments : List Comment
    , lineno : Int
    }


type alias PatchInfo =
    { delStarted : Int
    , delNumber : Int
    , addStarted : Int
    , addNumber : Int
    }


type LineType
    = Added
    | Deleted
    | Untouched


type alias Comment =
    { state : CommentState
    , text : String
    , author : Data.GitHubUser
    }


transformData : Data.Diff -> Comparison
transformData diff =
    { changes =
        List.foldl
            (\file diffs ->
                { sha = file.sha
                , filename = file.filename
                , blobUrl = file.blobUrl
                , patch = transformPatch file diff.comments
                }
                    :: diffs
            )
            []
            diff.files
    , commits = diff.commits
    }


transformPatch : Data.Change -> List Data.Comment -> List PatchLine
transformPatch change comments =
    parsePatch_
        (String.split "\n" change.patch)
        comments
        change.sha
        0
        0
        0


parsePatch_ : List String -> List Data.Comment -> String -> Int -> Int -> Int -> List PatchLine
parsePatch_ lines comments sha before after lineno =
    case lines of
        line :: rest ->
            let
                lineComments =
                    comments
                        |> List.filter
                            (\c -> c.sha == sha && c.lineno == lineno)
                        |> List.map
                            (\c ->
                                { text = c.text
                                , author = c.author
                                , state = Displayed
                                }
                            )
            in
            case parsePatchLine line before after of
                Just (Header header headerLine) ->
                    Header header
                        headerLine
                        :: parsePatch_
                            rest
                            comments
                            sha
                            header.delStarted
                            header.addStarted
                            (lineno + 1)

                Just (Code loc) ->
                    Code
                        { loc
                            | comments = lineComments
                            , lineno = lineno
                            , linenoBase = before
                            , linenoHead = after
                        }
                        :: parsePatch_
                            rest
                            comments
                            sha
                            loc.linenoBase
                            loc.linenoHead
                            (lineno + 1)

                Nothing ->
                    []

        _ ->
            []


parsePatchLine : String -> Int -> Int -> Maybe PatchLine
parsePatchLine line before after =
    if String.startsWith "@" line then
        case TextParser.run patchInfo line of
            Ok info ->
                Just (Header info line)

            _ ->
                Nothing

    else
        Just (Code <| lineOfCode before after line)


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


onUrlRequest : UrlRequest -> msg
onUrlRequest request =
    onUrlRequest request


splitBaseAndHead : String -> Maybe ( String, String )
splitBaseAndHead value =
    case String.split "..." value of
        [ base, head ] ->
            Just ( base, head )

        _ ->
            Nothing


parseCompareArgs : Parser (String -> String -> String -> a) a
parseCompareArgs =
    string </> string </> s "compare" </> string


parser : Parser (ParseResult -> a) a
parser =
    Url.Parser.map ParseResult parseCompareArgs


onUrlChange : Url -> Msg
onUrlChange url =
    onUrlChange url


initLoading : Maybe ParseResult -> String -> Maybe (Cmd Msg)
initLoading maybeResult apiUrl =
    case maybeResult of
        Just result ->
            case splitBaseAndHead result.baseAndHead of
                Just ( base, head ) ->
                    Just <|
                        Cmd.map DataMsg
                            (Data.makeRequest
                                { owner = result.owner
                                , repository = result.repository
                                , base = base
                                , head = head
                                }
                                apiUrl
                            )

                _ ->
                    Nothing

        _ ->
            Nothing


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init apiUrl location navigationKey =
    let
        command =
            case initLoading (parse parser location) apiUrl of
                Just cmd ->
                    cmd

                Nothing ->
                    Cmd.none
    in
    ( { title = "Code Reviewer"
      , maybeViewer = Nothing
      , comparison = { changes = [], commits = [] }
      , editingComment = Nothing
      , apiUrl = apiUrl
      , navigationKey = navigationKey
      }
    , command
    )


deleteComment : Comparison -> ( String, Int ) -> Comparison
deleteComment comparison ( sha, lineno ) =
    upsertComment comparison ( sha, lineno, [] )


upsertComment : Comparison -> ( String, Int, List Comment ) -> Comparison
upsertComment comparison ( sha, lineno, comment ) =
    let
        filterDisplayed c =
            case c.state of
                Displayed ->
                    True

                Editing ->
                    False

        upsert comments =
            List.append (List.filter filterDisplayed comments) comment

        addLocComment loc =
            case loc of
                Code l ->
                    if l.lineno == lineno then
                        Code { l | comments = upsert l.comments }

                    else
                        Code l

                Header info content ->
                    Header info content

        addPatchComment patch =
            List.map addLocComment patch

        addDiffComment diff =
            if diff.sha == sha then
                { diff | patch = addPatchComment diff.patch }

            else
                diff
    in
    { comparison | changes = List.map addDiffComment comparison.changes }


addComment model =
    case model.editingComment of
        Just ( sha, lineno, [ comment ] ) ->
            if comment.text == "" then
                ( model, Cmd.none )

            else
                ( { model
                    | comparison =
                        upsertComment
                            model.comparison
                            ( sha, lineno, [ { comment | state = Displayed } ] )
                    , editingComment = Nothing
                  }
                , Cmd.map DataMsg <|
                    Data.makeUpsertCommentRequest
                        sha
                        lineno
                        comment.text
                        model.apiUrl
                )

        _ ->
            ( model, Cmd.none )


editComment model text =
    case model.editingComment of
        Just ( sha, lineno, [ comment ] ) ->
            let
                editingComment =
                    ( sha, lineno, [ { comment | text = text } ] )
            in
            ( { model
                | editingComment = Just editingComment
                , comparison = upsertComment model.comparison editingComment
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


cancelEditComment model =
    case model.editingComment of
        Just ( sha, lineno, _ ) ->
            ( { model
                | comparison = deleteComment model.comparison ( sha, lineno )
                , editingComment = Nothing
              }
            , Cmd.none
            )

        Nothing ->
            ( model, Cmd.none )


startEditComment model sha lineno =
    case model.maybeViewer of
        Just viewer ->
            let
                comparison =
                    case model.editingComment of
                        Just ( s, l, _ ) ->
                            deleteComment model.comparison ( s, l )

                        Nothing ->
                            model.comparison

                newComment =
                    ( sha
                    , lineno
                    , [ { author =
                            { id = viewer.id
                            , name = viewer.name
                            , email = viewer.email
                            , login = viewer.login
                            , avatarUrl = viewer.avatarUrl
                            }
                        , text = ""
                        , state = Editing
                        }
                      ]
                    )
            in
            ( { model
                | editingComment = Just newComment
                , comparison = upsertComment comparison newComment
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataMsg (Data.ViewerLoaded (RemoteData.Success viewer)) ->
            ( { model
                | maybeViewer = Just viewer
                , comparison = transformData viewer.organization.repository.compare
              }
            , Cmd.none
            )

        DataMsg (Data.UpsertCommentDone _) ->
            ( model, Cmd.none )

        DataMsg _ ->
            ( model, Cmd.none )

        AddComment ->
            addComment model

        EditComment text ->
            editComment model text

        CancelEdit ->
            cancelEditComment model

        StartEditComment sha lineno ->
            startEditComment model sha lineno


view : Model -> Document Msg
view { title, maybeViewer, comparison } =
    let
        avatar =
            case maybeViewer of
                Just viewer ->
                    el [ alignRight, paddingXY 22 0 ] <|
                        viewAvatar (Maybe.withDefault "" viewer.avatarUrl) 20

                _ ->
                    el [] (text "")

        header =
            row
                [ height (px 64)
                , width fill
                , Element.Background.color (rgb255 14 16 18)
                ]
                [ viewLogo
                , avatar
                ]

        content =
            column
                [ width (fill |> maximum 980)
                , centerX
                , spacingXY 0 20
                , paddingEach { edge | bottom = 80 }
                , Element.Font.color (rgb255 36 41 46)
                , Element.Font.size 12
                , Element.Font.family
                    [ Element.Font.typeface "SFMono-Regular"
                    , Element.Font.typeface "Consolas"
                    , Element.Font.typeface "Liberation Mono"
                    , Element.Font.typeface "Menlo"
                    , Element.Font.monospace
                    ]
                ]
            <|
                viewComparison comparison
    in
    { title = title
    , body =
        [ layout
            [ Element.Font.family
                [ Element.Font.external
                    { url = "https://fonts.googleapis.com/css?family=Fira+Mono"
                    , name = "Fira Mono"
                    }
                , Element.Font.monospace
                ]
            ]
            (column
                [ width fill
                , spacingXY 0 16
                ]
                [ Element.html <| SyntaxHighlight.useTheme SyntaxHighlight.gitHub
                , header
                , content
                ]
            )
        ]
    }


viewAvatar : String -> Int -> Element msg
viewAvatar avatarUrl size =
    image
        [ height (px size)
        , width (px size)
        , Element.Border.rounded 3
        , clip
        ]
        { src = avatarUrl
        , description = "Avatar"
        }


viewLogo =
    el
        [ Element.Font.color (rgb 1 1 1)
        , Element.Font.bold
        , Element.Font.size 16
        , alignLeft
        , paddingXY 22 0
        ]
    <|
        text "Code Reviewer"


viewComparison : Comparison -> List (Element Msg)
viewComparison comparison =
    List.append
        [ Element.el
            [ Element.Font.size 16
            , Element.Font.semiBold
            ]
          <|
            text "Commits"
        , Element.column
            [ Element.Border.widthEach { edge | left = 3 }
            , borderColor
            , spacingXY 0 16
            , paddingEach { edge | left = 10 }
            ]
          <|
            List.map viewCommits comparison.commits
        ]
        (List.map viewChangedFile comparison.changes)


viewCommits : Data.Commit -> Element Msg
viewCommits commit =
    Element.row
        [ width fill
        , spacingXY 24 0
        ]
        [ row
            [ spacingXY 8 0
            , alignTop
            ]
            [ viewAvatar (Maybe.withDefault "" commit.author.avatarUrl) 20
            , text commit.commit.author.name
            ]
        , Element.newTabLink
            [ mouseOver
                [ Element.Border.color (rgb255 0 86 212)
                , Element.Font.color (rgb255 0 86 212)
                ]
            , Element.Font.color (rgb255 87 96 105)
            , Element.Border.color <|
                rgba255 255 255 255 255
            , Element.Border.widthEach
                { bottom = 1
                , left = 0
                , top = 0
                , right = 0
                }
            ]
            { url = commit.htmlUrl
            , label = text commit.commit.message
            }
        ]


patchInfo : TextParser.Parser PatchInfo
patchInfo =
    TextParser.succeed PatchInfo
        |. TextParser.symbol "@"
        |. TextParser.symbol "@"
        |. TextParser.spaces
        |. TextParser.symbol "-"
        |= TextParser.int
        |. TextParser.symbol ","
        |= TextParser.int
        |. TextParser.spaces
        |. TextParser.symbol "+"
        |= TextParser.int
        |. TextParser.symbol ","
        |= TextParser.int
        |. TextParser.chompUntilEndOr "\n"


lineOfCode : Int -> Int -> String -> LineOfCode
lineOfCode before after line =
    if String.startsWith "-" line then
        { lineType = Deleted
        , linenoBase = before + 1
        , linenoHead = after
        , code = line
        , comments = []
        , lineno = 0
        }

    else if String.startsWith "+" line then
        { lineType = Added
        , linenoBase = before
        , linenoHead = after + 1
        , code = line
        , comments = []
        , lineno = 0
        }

    else
        { lineType = Untouched
        , linenoBase = before + 1
        , linenoHead = after + 1
        , code = line
        , comments = []
        , lineno = 0
        }


viewChangedFile : Diff -> Element Msg
viewChangedFile diff =
    let
        syntax =
            highlighter diff.filename
    in
    el
        [ Element.Border.solid
        , borderColor
        , Element.Border.width 1
        , Element.Border.rounded 2
        , width fill
        , height fill
        , Element.Background.color (rgb255 250 251 252)
        ]
    <|
        Element.Lazy.lazy
            (column [ width fill ])
            [ Element.html <| SyntaxHighlight.useTheme SyntaxHighlight.gitHub
            , viewFileName diff
            , viewPatch diff.patch diff.sha syntax
            ]


viewFileName : Diff -> Element msg
viewFileName diff =
    el
        [ centerY
        , height (px 43)
        , width fill
        , paddingXY 10 15
        , Element.Background.color (rgb255 250 251 252)
        ]
    <|
        Element.newTabLink
            [ mouseOver
                [ Element.Border.color (rgb255 0 86 212)
                , Element.Font.color (rgb255 0 86 212)
                ]
            , Element.Font.color (rgb255 87 96 105)
            , Element.Border.color <|
                rgba255 255 255 255 255
            , Element.Border.widthEach
                { bottom = 1
                , left = 0
                , top = 0
                , right = 0
                }
            ]
            { url = diff.blobUrl
            , label = text diff.filename
            }


type alias Syntax =
    String -> Result (List TextParser.DeadEnd) SyntaxHighlight.HCode


viewPatch : List PatchLine -> String -> Syntax -> Element Msg
viewPatch patchLines sha syntax =
    Element.Keyed.column
        [ width fill
        , scrollbarX
        ]
    <|
        List.map (viewPatchLine syntax sha) patchLines


viewPatchHeader : String -> Element.Element Msg
viewPatchHeader header =
    Element.Lazy.lazy
        (el
            [ width fill
            , height (px 28)
            , Element.Font.size 12
            , Element.Font.color (rgb255 90 96 100)
            , paddingXY 10 7
            , Element.Background.color (rgb255 241 248 255)
            , borderColor
            , Element.Border.widthEach { edge | bottom = 1, top = 1 }
            ]
        )
    <|
        text header


viewLineNumber lineType linenoBase linenoHead =
    let
        numbers =
            case lineType of
                Added ->
                    [ el numberStyle (text "")
                    , el numberStyle (text <| String.fromInt linenoHead)
                    ]

                Deleted ->
                    [ el numberStyle (text <| String.fromInt linenoBase)
                    , el numberStyle (text "")
                    ]

                Untouched ->
                    [ el numberStyle (text <| String.fromInt linenoBase)
                    , el numberStyle (text <| String.fromInt linenoHead)
                    ]
    in
    row
        [ width (px 100)
        , lineColor lineType
        , pointer
        ]
        numbers


numberStyle =
    [ width (px 50)
    , paddingXY 10 4
    , Element.Font.color (rgb255 186 187 188)
    , mouseOver [ Element.Font.color (rgb255 90 96 100) ]
    ]


linenoColor lineType =
    case lineType of
        Added ->
            Element.Background.color (rgb255 230 255 237)

        Deleted ->
            Element.Background.color (rgb255 255 238 240)

        Untouched ->
            Element.Background.color (rgb255 255 255 255)


lineColor lineType =
    case lineType of
        Added ->
            Element.Background.color (rgb255 205 255 216)

        Deleted ->
            Element.Background.color (rgb255 255 220 224)

        Untouched ->
            Element.Background.color (rgb255 255 255 255)


viewCodeLine : Syntax -> LineOfCode -> String -> Element.Element Msg
viewCodeLine syntax loc sha =
    let
        comments =
            case loc.comments of
                [] ->
                    none

                cmnts ->
                    column
                        [ width fill
                        , padding 10
                        , borderColor
                        , Element.Background.color (rgb255 255 255 255)
                        , Element.Border.widthEach
                            { edge
                                | top = 1
                                , bottom = 1
                            }
                        ]
                    <|
                        List.map viewComment cmnts

        key =
            sha ++ String.fromInt loc.lineno
    in
    Element.Lazy.lazy
        (Element.Keyed.column
            [ width fill ]
        )
        [ ( "code-" ++ key
          , Element.Keyed.row
                [ width fill
                , Element.Events.onClick <| StartEditComment sha loc.lineno
                ]
                [ ( "lineno-" ++ key
                  , viewLineNumber
                        loc.lineType
                        loc.linenoBase
                        loc.linenoHead
                  )
                , ( key
                  , Element.el
                        [ width fill
                        , pointer
                        , paddingXY 10 4
                        , linenoColor loc.lineType
                        , centerY
                        ]
                    <|
                        Element.Lazy.lazy (highlight syntax) loc.code
                  )
                ]
          )
        , ( "comments-" ++ key, comments )
        ]


highlighter : String -> Syntax
highlighter filename =
    case List.reverse <| String.split "." filename of
        "py" :: _ ->
            SyntaxHighlight.python

        "js" :: _ ->
            SyntaxHighlight.javascript

        "jsx" :: _ ->
            SyntaxHighlight.javascript

        "css" :: _ ->
            SyntaxHighlight.css

        "scss" :: _ ->
            SyntaxHighlight.css

        "html" :: _ ->
            SyntaxHighlight.xml

        "json" :: _ ->
            SyntaxHighlight.json

        _ ->
            SyntaxHighlight.python


highlight : Syntax -> String -> Element Msg
highlight syntax code =
    Element.html <|
        Html.div []
            [ syntax code
                |> Result.map SyntaxHighlight.toInlineHtml
                |> Result.withDefault
                    (Html.pre
                        []
                        [ Html.code [] [ Html.text code ] ]
                    )
            ]


viewPatchLine : Syntax -> String -> PatchLine -> ( String, Element Msg )
viewPatchLine syntax sha line =
    case line of
        Header _ header ->
            ( sha, viewPatchHeader header )

        Code loc ->
            ( sha ++ String.fromInt loc.lineno, viewCodeLine syntax loc sha )


viewComment : Comment -> Element Msg
viewComment comment =
    case comment.state of
        Editing ->
            Element.Keyed.column
                [ width (fill |> maximum 780)
                , spacingXY 0 10
                , height fill
                ]
                [ ( "Editor"
                  , Element.Input.multiline
                        [ height fill
                        , height (px 100)
                        , Element.Input.focusedOnLoad
                        ]
                        { onChange = EditComment
                        , text = comment.text
                        , placeholder =
                            Just <|
                                Element.Input.placeholder [] <|
                                    text "Leave a comment"
                        , label = Element.Input.labelHidden ""
                        , spellcheck = False
                        }
                  )
                , ( "Editor Buttons"
                  , row
                        [ alignRight
                        , spacingXY 10 0
                        ]
                        [ Element.Input.button
                            [ padding 10
                            , borderColor
                            , Element.Background.color (rgb255 242 244 247)
                            , Element.Border.width 1
                            , Element.Border.rounded 3
                            , Element.Border.color (rgb255 198 200 204)
                            , Element.Font.bold
                            , mouseOver
                                [ Element.Background.color (rgb255 238 240 245)
                                ]
                            ]
                            { onPress = Just CancelEdit
                            , label = text "Cancel"
                            }
                        , Element.Input.button
                            [ padding 10
                            , Element.Border.color (rgb255 198 200 204)
                            , Element.Font.bold
                            , Element.Background.color (rgb255 242 244 247)
                            , Element.Border.width 1
                            , Element.Border.rounded 3
                            , mouseOver
                                [ Element.Background.color (rgb255 238 240 245)
                                ]
                            ]
                            { onPress = Just AddComment
                            , label = text "Add single comment"
                            }
                        ]
                  )
                ]

        Displayed ->
            column
                [ padding 16
                , width (fill |> maximum 780)
                ]
            <|
                [ row
                    []
                    [ el
                        [ paddingEach { edge | right = 20 }, alignTop ]
                      <|
                        viewAvatar
                            (Maybe.withDefault "" comment.author.avatarUrl)
                            28
                    , textColumn
                        [ spacingXY 0 10
                        , alignTop
                        ]
                        [ el
                            [ alignTop
                            , paddingEach { edge | bottom = 10 }
                            ]
                          <|
                            viewLogin
                                (Maybe.withDefault "" comment.author.name)
                        , renderCommentText comment.text
                        ]
                    ]
                ]


viewLogin : String -> Element msg
viewLogin login =
    el [ Element.Font.bold ] <| text login


edge =
    { top = 0, right = 0, bottom = 0, left = 0 }


borderColor =
    Element.Border.color (rgb255 221 221 221)


renderCommentText : String -> Element Msg
renderCommentText comment =
    column
        []
        (comment
            |> renderMarkdown
            |> Result.withDefault [ paragraph [] [ text comment ] ]
        )


renderMarkdown : String -> Result String (List (Element Msg))
renderMarkdown text =
    text
        |> Markdown.Parser.parse
        |> Result.mapError
            (\error ->
                error
                    |> List.map Markdown.Parser.deadEndToString
                    |> String.join "\n"
            )
        |> Result.andThen (Markdown.Renderer.render Markdown.elmUiRenderer)
