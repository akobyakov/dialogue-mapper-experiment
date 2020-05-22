module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Element as El
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import Html as H
import Html.Attributes as Attr
import Html.Events as Evt
import Json.Decode as Decode
import Task
import Tree exposing (Tree)
import Tree.Zipper exposing (Zipper)



---- MODEL ----


type DialogueNodeType
    = Question
    | Idea
    | Pro
    | Con


type alias DialogueNodeAnnotation =
    { nodeType : DialogueNodeType
    , title : String
    , isSelected : Bool
    }


type alias DialogueNode =
    Tree DialogueNodeAnnotation


type DialogueNodeConnections
    = DialogueNodeConnections (List DialogueNode)


type alias DialogueFocus =
    Zipper DialogueNodeAnnotation


type alias AttachingError =
    String


areAttachableTypes : DialogueNodeType -> DialogueNodeType -> Bool
areAttachableTypes parentType childType =
    List.member childType
        (case parentType of
            Question ->
                [ Question, Idea ]

            Idea ->
                [ Question, Pro, Con ]

            Pro ->
                [ Question ]

            Con ->
                [ Question ]
        )


appendChildNode : DialogueFocus -> DialogueNode -> Result AttachingError DialogueFocus
appendChildNode focus newNode =
    let
        focusType =
            (Tree.label (Tree.Zipper.toTree focus)).nodeType

        newNodeType =
            (Tree.label newNode).nodeType
    in
    if areAttachableTypes focusType newNodeType then
        Ok (Tree.Zipper.mapTree (Tree.appendChild newNode) focus)

    else
        Err "Forbidden types to attach"


makeNode : DialogueNodeAnnotation -> DialogueNode
makeNode annotation =
    Tree.singleton annotation


stringToNode : DialogueNodeType -> String -> DialogueNode
stringToNode nodeType title =
    makeNode { nodeType = nodeType, title = title, isSelected = False }


startDialogue : String -> DialogueFocus
startDialogue topic =
    Tree.Zipper.fromTree (stringToNode Question topic)
        |> invertSelection


invertSelection : DialogueFocus -> DialogueFocus
invertSelection focus =
    Tree.Zipper.mapLabel (\a -> { a | isSelected = not a.isSelected }) focus


type DialogueNavigation
    = EnterLevel
    | LeaveLevel
    | Next
    | Previous


navigateDialogue : DialogueFocus -> DialogueNavigation -> DialogueFocus
navigateDialogue currentFocus navigation =
    let
        moveFocus =
            case navigation of
                EnterLevel ->
                    Tree.Zipper.firstChild

                LeaveLevel ->
                    Tree.Zipper.parent

                Next ->
                    Tree.Zipper.nextSibling

                Previous ->
                    Tree.Zipper.previousSibling
    in
    currentFocus |> invertSelection |> moveFocus |> Maybe.map invertSelection |> Maybe.withDefault currentFocus


applyStatement : DialogueFocus -> String -> Result AttachingError DialogueFocus
applyStatement focus statement =
    let
        statementToNode =
            stringToNode Question statement
    in
    appendChildNode focus statementToNode


type Model
    = ChoosingTopic { topic : String }
    | BrowsingDialogue { focus : DialogueFocus }
    | Editing { focus : DialogueFocus, statement : String, nodeType : DialogueNodeType }


init : ( Model, Cmd Msg )
init =
    ( ChoosingTopic { topic = "" }, Cmd.none )



---- UPDATE ----


type Msg
    = KeyPressed Key
    | EditTopic String
    | EditStatement String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        --
        -- Choosing a topic of the conversation
        --
        ( EditTopic newTopic, ChoosingTopic context ) ->
            ( ChoosingTopic { context | topic = newTopic }, Cmd.none )

        --
        -- Statement editor
        --
        ( EditStatement newStatement, Editing context ) ->
            ( Editing { context | statement = newStatement }, Cmd.none )

        --
        -- Keyboard navigation
        --
        ( KeyPressed key, BrowsingDialogue context ) ->
            handleKeyboardEventsWhenBrowsing context.focus key |> Maybe.withDefault ( model, Cmd.none )

        ( KeyPressed key, ChoosingTopic context ) ->
            case key of
                Enter ->
                    ( BrowsingDialogue { focus = startDialogue context.topic }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( KeyPressed key, Editing context ) ->
            case key of
                Enter ->
                    if String.isEmpty context.statement then
                        ( BrowsingDialogue { focus = context.focus }, Cmd.none )

                    else
                        case applyStatement context.focus context.statement of
                            Ok newFocus ->
                                ( BrowsingDialogue { focus = newFocus }, Cmd.none )

                            Err error ->
                                -- TODO expose errors
                                ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        --
        -- Auxiliary events
        --
        ( _, _ ) ->
            ( model, Cmd.none )


handleKeyboardEventsWhenBrowsing : DialogueFocus -> Key -> Maybe ( Model, Cmd Msg )
handleKeyboardEventsWhenBrowsing focus key =
    let
        moveSelection navigation =
            ( BrowsingDialogue { focus = navigateDialogue focus navigation }, Cmd.none )

        newNodeEditor nodeType =
            ( Editing { focus = focus, statement = "", nodeType = nodeType }, Cmd.none )
    in
    case key of
        Enter ->
            Just (newNodeEditor Question)

        CharacterKey '!' ->
            Just (newNodeEditor Idea)

        CharacterKey 'j' ->
            Just (moveSelection Next)

        CharacterKey 'k' ->
            Just (moveSelection Previous)

        CharacterKey 'h' ->
            Just (moveSelection LeaveLevel)

        CharacterKey 'l' ->
            Just (moveSelection EnterLevel)

        _ ->
            Nothing


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.map toKey
        |> Decode.map KeyPressed


type Key
    = ArrowUp
    | ArrowDown
    | ArrowLeft
    | ArrowRight
    | Enter
    | Backspace
    | CharacterKey Char
    | OtherKey


toKey : String -> Key
toKey keyValue =
    case keyValue of
        "Enter" ->
            Enter

        _ ->
            case String.uncons keyValue of
                Just ( ch, _ ) ->
                    CharacterKey (Char.toLower ch)

                _ ->
                    OtherKey



---- VIEW ----


view : Model -> H.Html Msg
view model =
    let
        layout =
            El.layout
                [ Border.rounded 1
                , El.padding 30
                , Font.color (El.rgba 0 0 0 1)
                , Font.size 18
                , Font.family
                    [ Font.external
                        { url = "https://fonts.googleapis.com/css?family=EB+Garamond"
                        , name = "EB Garamond"
                        }
                    , Font.sansSerif
                    ]
                ]

        onEnter msg =
            El.htmlAttribute
                (Evt.on "keyup"
                    (Decode.field "key" Decode.string
                        |> Decode.andThen
                            (\key ->
                                if key == "Enter" then
                                    Decode.succeed msg

                                else
                                    Decode.fail "Not the enter key"
                            )
                    )
                )
    in
    layout <|
        case model of
            ChoosingTopic context ->
                El.row [ El.width (El.fill |> El.maximum 640), El.centerY, El.centerX ]
                    [ Input.spellChecked
                        [ El.spacing 12
                        , Input.focusedOnLoad
                        , Font.size 32
                        ]
                        { onChange = EditTopic
                        , text = context.topic
                        , placeholder = Just (Input.placeholder [] (El.text "What the dialogue is about today?"))
                        , label = Input.labelHidden "Topic"
                        }
                    ]

            Editing context ->
                El.column [ El.width El.fill, El.height El.fill ]
                    [ dialogueBrowser context.focus
                    , Input.spellChecked [ El.spacing 12, El.width El.fill, Input.focusedOnLoad ]
                        { onChange = EditStatement
                        , text = context.statement
                        , placeholder = Just (Input.placeholder [] (El.text "Enter the statement"))
                        , label = Input.labelHidden "Statement"
                        }
                    ]

            BrowsingDialogue context ->
                El.column [ El.width El.fill, El.height El.fill ]
                    [ dialogueBrowser context.focus
                    ]


dialogueBrowser : DialogueFocus -> El.Element msg
dialogueBrowser dialogueFocus =
    Tree.Zipper.root dialogueFocus
        |> Tree.Zipper.toTree
        |> Tree.restructure renderAnnotation renderAnnotationRecursively
        |> (\root ->
                El.column
                    [ El.spacing 20, El.width El.fill, El.height El.fill, El.explain Debug.todo ]
                    [ root ]
           )


renderAnnotation : DialogueNodeAnnotation -> El.Element msg
renderAnnotation annotation =
    let
        selectedItemAttributes =
            if annotation.isSelected then
                [ Font.bold ]

            else
                []

        calculateAttributes =
            selectedItemAttributes ++ [ El.padding 0, El.spacing 0, El.alignLeft, El.width El.fill ]
    in
    El.el calculateAttributes (El.text annotation.title)


renderAnnotationRecursively : El.Element msg -> List (El.Element msg) -> El.Element msg
renderAnnotationRecursively label children =
    El.row [ El.padding 0, El.spacing 0, El.alignLeft, El.width El.fill ] <|
        case children of
            [] ->
                [ label ]

            _ ->
                [ label, El.column [ El.padding 10, El.spacing 10, El.width El.fill ] children ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> Browser.Events.onKeyPress keyDecoder
        }
