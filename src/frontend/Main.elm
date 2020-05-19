module Main exposing (..)

import Browser exposing (Document, UrlRequest, application)
import Browser.Events
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
    , device : Device
    , maybeViewer : Maybe Viewer
    , comparison : Comparison
    , editingComment : Maybe ( String, Int, List Comment )
    , apiUrl : String
    , navigationKey : Key
    }


type alias Flags =
    { apiUrl : String
    , width : Int
    , height : Int
    }


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
    | Resized Int Int


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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w h -> Resized w h)


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
init flags location navigationKey =
    let
        command =
            case initLoading (parse parser location) flags.apiUrl of
                Just cmd ->
                    cmd

                Nothing ->
                    Cmd.none
    in
    ( { title = "Code Reviewer"
      , maybeViewer = Nothing
      , comparison = { changes = [], commits = [] }
      , editingComment = Nothing
      , apiUrl = flags.apiUrl
      , navigationKey = navigationKey
      , device = classifyDevice { width = flags.width, height = flags.height }
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

        Resized width height ->
            ( { model | device = classifyDevice { width = width, height = height } }
            , Cmd.none
            )


viewHeader : Maybe Viewer -> Element Msg
viewHeader maybeViewer =
    let
        avatar =
            case maybeViewer of
                Just viewer ->
                    viewAvatar
                        20
                        [ alignRight, paddingXY 22 0 ]
                        viewer.avatarUrl

                _ ->
                    el [] (text "")
    in
    row
        [ height (px 64)
        , width fill
        , Element.Background.color (rgb255 14 16 18)
        ]
        [ viewLogo
        , avatar
        ]


view : Model -> Document Msg
view { device, title, maybeViewer, comparison } =
    let
        content =
            Element.Keyed.column
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
                viewComparison device comparison
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
                , Element.Lazy.lazy viewHeader maybeViewer
                , content
                ]
            )
        ]
    }


viewAvatar : Int -> List (Element.Attribute Msg) -> Maybe String -> Element Msg
viewAvatar size attributes maybeAvatarUrl =
    let
        src =
            case maybeAvatarUrl of
                Just avatarUrl ->
                    avatarUrl

                Nothing ->
                    "https://camo.githubusercontent.com/01ba10fe0e5de848c55f5cc9d140cc97fcdfa4bf/68747470733a2f2f322e67726176617461722e636f6d2f6176617461722f64653639613830383235376237623734373935613934356162386361333238363f643d68747470732533412532462532466769746875622e6769746875626173736574732e636f6d253246696d6167657325324667726176617461727325324667726176617461722d757365722d3432302e706e6726723d6726733d3634"
    in
    el attributes <|
        image
            [ height (px size)
            , width (px size)
            , Element.Border.rounded 3
            , clip
            ]
            { src = src
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


viewComparison : Device -> Comparison -> List ( String, Element Msg )
viewComparison device comparison =
    List.append
        [ ( "commits-header"
          , Element.el
                [ Element.Font.size 16
                , Element.Font.semiBold
                ]
            <|
                text "Commits"
          )
        , ( "commits"
          , Element.column
                [ Element.Border.widthEach { edge | left = 3 }
                , borderColor
                , spacingXY 0 16
                , paddingEach { edge | left = 10 }
                ]
            <|
                List.map (Element.Lazy.lazy viewCommit) comparison.commits
          )
        ]
        (List.map (viewChangedFile device) comparison.changes)


viewCommit : Data.Commit -> Element Msg
viewCommit commit =
    Element.row
        [ width fill
        , spacingXY 24 0
        ]
        [ row
            [ alignTop ]
            [ viewAvatar 20 [ paddingXY 8 0 ] commit.author.avatarUrl
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


viewChangedFile : Device -> Diff -> ( String, Element Msg )
viewChangedFile device diff =
    let
        syntax =
            highlighter diff.filename
    in
    ( diff.filename
    , el
        [ Element.Border.solid
        , borderColor
        , Element.Border.width 1
        , Element.Border.rounded 2
        , width fill
        , height fill
        , Element.Background.color (rgb255 250 251 252)
        ]
      <|
        Element.Keyed.column [ width fill ]
            [ ( diff.filename ++ "_style", Element.Lazy.lazy Element.html <| SyntaxHighlight.useTheme SyntaxHighlight.gitHub )
            , ( diff.filename ++ "_name", Element.Lazy.lazy viewFileName diff )
            , ( diff.filename ++ "_patch", Element.Lazy.lazy4 viewPatch device diff.patch diff.sha syntax )
            ]
    )


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


viewPatch : Device -> List PatchLine -> String -> Syntax -> Element Msg
viewPatch device patchLines sha syntax =
    Element.Keyed.column [ width fill, scrollbarX ] <|
        List.concat <|
            List.map (viewPatchLine device syntax sha) patchLines


viewPatchHeader : String -> Element Msg
viewPatchHeader header =
    el
        [ width fill
        , height (px 28)
        , Element.Font.size 12
        , Element.Font.color (rgb255 90 96 100)
        , paddingXY 10 7
        , Element.Background.color (rgb255 241 248 255)
        , borderColor
        , Element.Border.widthEach { edge | bottom = 1, top = 1 }
        ]
    <|
        text header


viewLineNumber linenoBase linenoHead lineType =
    case lineType of
        Added ->
            [ viewLineNumber_ lineType none
            , viewLineNumber_ lineType (text <| String.fromInt linenoHead)
            ]

        Deleted ->
            [ viewLineNumber_ lineType (text <| String.fromInt linenoBase)
            , viewLineNumber_ lineType none
            ]

        Untouched ->
            [ viewLineNumber_ lineType (text <| String.fromInt linenoBase)
            , viewLineNumber_ lineType (text <| String.fromInt linenoHead)
            ]


viewLineNumber_ lineType =
    el
        [ width (px 50)
        , height fill
        , paddingXY 10 4
        , lineColor lineType
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


viewComments loc =
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
                List.map (Element.Lazy.lazy viewComment) cmnts


viewCode syntax lineType code =
    Element.el
        [ width fill
        , paddingXY 10 4
        , linenoColor lineType
        ]
    <|
        Element.Lazy.lazy2 highlight syntax code


viewCodeLine : Device -> Syntax -> LineOfCode -> String -> List ( String, Element Msg )
viewCodeLine device syntax loc sha =
    let
        linenoView =
            case device.class of
                Phone ->
                    [ none ]

                _ ->
                    viewLineNumber loc.linenoBase loc.linenoHead loc.lineType
    in
    [ ( sha ++ String.fromInt loc.lineno
      , row
            [ width fill
            , pointer
            , centerY
            , Element.Events.onClick <| StartEditComment sha loc.lineno
            ]
        <|
            List.append
                linenoView
                [ viewCode syntax loc.lineType loc.code ]
      )
    , ( sha ++ String.fromInt loc.lineno ++ "_comments"
      , Element.Lazy.lazy viewComments loc
      )
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


viewPatchLine : Device -> Syntax -> String -> PatchLine -> List ( String, Element Msg )
viewPatchLine device syntax sha line =
    case line of
        Header _ header ->
            [ ( sha ++ header, Element.Lazy.lazy viewPatchHeader header ) ]

        Code loc ->
            viewCodeLine device syntax loc sha


viewCommentEditor : String -> Element Msg
viewCommentEditor commentText =
    column
        [ width (fill |> maximum 780)
        , spacingXY 0 10
        , height fill
        ]
        [ Element.Input.multiline
            [ height fill
            , height (px 100)
            , Element.Input.focusedOnLoad
            ]
            { onChange = EditComment
            , text = commentText
            , placeholder =
                Just <|
                    Element.Input.placeholder [] <|
                        text "Leave a comment"
            , label = Element.Input.labelHidden ""
            , spellcheck = False
            }
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
        ]


viewDisplayedComment : Comment -> Element Msg
viewDisplayedComment comment =
    row
        [ padding 16
        , width (fill |> maximum 780)
        ]
        [ viewAvatar
            28
            [ paddingEach { edge | right = 20 }, alignTop ]
            comment.author.avatarUrl
        , textColumn
            [ spacingXY 0 10
            , alignTop
            ]
            [ el
                [ alignTop
                , paddingEach { edge | bottom = 10 }
                ]
              <|
                viewUserName (Maybe.withDefault "" comment.author.name)
            , Element.Lazy.lazy renderCommentText comment.text
            ]
        ]


viewComment : Comment -> Element Msg
viewComment comment =
    case comment.state of
        Editing ->
            Element.Lazy.lazy viewCommentEditor comment.text

        Displayed ->
            Element.Lazy.lazy viewDisplayedComment comment


viewUserName : String -> Element msg
viewUserName login =
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
