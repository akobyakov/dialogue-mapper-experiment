module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Element as El
import Element.Background as Background
import Element.Font as Font
import Element.Input
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
    ( ChoosingTopic { topic = "" }, focusOnElementWithId "input-dialogue-topic" )



---- UPDATE ----


type Msg
    = Focus (Result Dom.Error ())
    | KeyPressed Key
    | EditTopic String
    | StartDialogue
    | EditStatement String
    | ProcessStatement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        --
        -- Choosing a topic of the conversation
        --
        ( EditTopic newTopic, ChoosingTopic context ) ->
            ( ChoosingTopic { context | topic = newTopic }, Cmd.none )

        ( StartDialogue, ChoosingTopic context ) ->
            ( BrowsingDialogue { focus = startDialogue context.topic }, Cmd.none )

        --
        -- Statement editor
        --
        ( EditStatement newStatement, Editing context ) ->
            ( Editing { context | statement = newStatement }, Cmd.none )

        ( ProcessStatement, Editing context ) ->
            if String.isEmpty context.statement then
                ( BrowsingDialogue { focus = context.focus }, Cmd.none )

            else
                case applyStatement context.focus context.statement of
                    Ok newFocus ->
                        ( BrowsingDialogue { focus = newFocus }, Cmd.none )

                    Err error ->
                        -- TODO expose errors
                        ( model, Cmd.none )

        --
        -- Keyboard navigation
        --
        ( KeyPressed key, BrowsingDialogue context ) ->
            handleKeyboardEventsWhenBrowsing context.focus key |> Maybe.withDefault ( model, Cmd.none )

        --
        -- Auxiliary events
        --
        ( Focus (Err e), _ ) ->
            let
                _ =
                    Debug.log "Couldn't find the element id" e
            in
            ( model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


handleKeyboardEventsWhenBrowsing : DialogueFocus -> Key -> Maybe ( Model, Cmd Msg )
handleKeyboardEventsWhenBrowsing focus key =
    let
        moveSelection navigation =
            ( BrowsingDialogue { focus = navigateDialogue focus navigation }, Cmd.none )

        newNodeEditor nodeType =
            ( Editing { focus = focus, statement = "", nodeType = nodeType }, focusOnElementWithId "input-statement" )
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


focusOnElementWithId : String -> Cmd Msg
focusOnElementWithId elementId =
    Task.attempt Focus (Dom.focus elementId)


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
    case model of
        ChoosingTopic context ->
            H.div []
                [ H.form [ Evt.onSubmit StartDialogue ]
                    [ H.input
                        [ Attr.id "input-dialogue-topic"
                        , Attr.placeholder "What is the topic of this conversation?"
                        , Attr.value context.topic
                        , Evt.onInput EditTopic
                        ]
                        []
                    ]
                ]

        Editing context ->
            H.div []
                [ H.div [] [ renderDialogue context.focus ]
                , H.div []
                    [ H.form [ Evt.onSubmit ProcessStatement ]
                        [ H.input
                            [ Attr.id "input-statement"
                            , Attr.placeholder "Enter a statement..."
                            , Attr.value context.statement
                            , Evt.onInput EditStatement
                            ]
                            []
                        ]
                    ]
                ]

        BrowsingDialogue context ->
            --H.div []
            --                [ renderDialogue context.focus ]
            blabla


blabla =
    El.layout
        [ Font.color (rgba 0 0 0 1)
        , Font.size 32
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=EB+Garamond"
                , name = "EB Garamond"
                }
            , Font.sansSerif
            ]
        ]
    <|
        El.column
            [ El.spacing 20, El.padding 10 ]
            []


annotationToHtml : DialogueNodeAnnotation -> H.Html msg
annotationToHtml annotation =
    if annotation.isSelected then
        H.text (">>>" ++ annotation.title ++ "<<<")

    else
        H.text annotation.title


toListItems : H.Html msg -> List (H.Html msg) -> H.Html msg
toListItems label children =
    case children of
        [] ->
            H.li [] [ label ]

        _ ->
            H.li []
                [ label
                , H.ul [] children
                ]


renderDialogue : DialogueFocus -> H.Html msg
renderDialogue focus =
    Tree.Zipper.root focus
        |> Tree.Zipper.toTree
        |> Tree.restructure annotationToHtml toListItems
        |> (\root -> H.ul [] [ root ])



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> Browser.Events.onKeyPress keyDecoder
        }
