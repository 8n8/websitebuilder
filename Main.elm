module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Debug
import Element as E
import Element.Border as Eb
import Element.Events as Ev
import Element.Input as Ei
import Task
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = \_ -> DoNothing
        , onUrlChange = \_ -> DoNothing
        }


view : Model -> Browser.Document Msg
view model =
    { title = "Website builder"
    , body = [ E.layout [] (homePage model) ]
    }


type alias Model =
    { viewport : Dom.Viewport
    , mode : Mode
    , website : Maybe Website
    , focussedNode : Maybe Int
    , internalErr : Maybe String
    , maxId : Int
    }


type alias Website =
    ( Component, Int )


type Component
    = Text String
    | Row (List Website)
    | Column (List Website)


type Mode
    = InsertTextMode
    | InsertRowMode
    | SelectorMode
    | EditComponentMode


type Msg
    = DoNothing
    | Viewport Dom.Viewport
    | InsertText
    | EmptyClick
    | EditComponent
    | EditText Int String
    | Clicked Int
    | Unfocussed Int
    | Unselect
    | InsertRow


notEmptyErr =
    "received EmptyClick but the website is not empty"


nothingToSelectErr =
    "selection mode is not allowed when there is nothing to select"


nothingToEditErr =
    "edit mode is not allowed when there is nothing to edit"


createTopNode : Model -> ( Model, Cmd Msg )
createTopNode model =
    case model.mode of
        InsertTextMode ->
            ( { model
                | website = Just ( Text "Some new text", 0 )
                , maxId = 0
              }
            , Cmd.none
            )

        InsertRowMode ->
            ( { model
                | website = Just ( Row [], 0 )
                , maxId = 0
              }
            , Cmd.none
            )

        EditComponentMode ->
            ( { model | internalErr = Just nothingToEditErr }
            , Cmd.none
            )

        SelectorMode ->
            ( { model | internalErr = Just nothingToSelectErr }
            , Cmd.none
            )


updateTextNode : Int -> String -> Website -> Website
updateTextNode id newText ( component, oldId ) =
    case ( component, oldId == id ) of
        ( Text oldText, True ) ->
            ( Text newText, id )

        _ ->
            case component of
                Row websites ->
                    ( Row <|
                        List.map
                            (updateTextNode id newText)
                            websites
                    , oldId
                    )

                Column websites ->
                    ( Column <|
                        List.map (updateTextNode id newText) websites
                    , oldId
                    )

                _ ->
                    ( component, oldId )


editEmptyErr =
    "can't edit text in empty website"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InsertRow ->
            ( { model | mode = InsertRowMode }, Cmd.none )

        Unselect ->
            ( { model | focussedNode = Nothing }, Cmd.none )

        Clicked id ->
            ( { model | focussedNode = Just id }, Cmd.none )

        Unfocussed id ->
            ( { model | focussedNode = Nothing }, Cmd.none )

        EditText id newText ->
            case model.website of
                Just ws ->
                    ( { model
                        | website =
                            Just <| updateTextNode id newText ws
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | internalErr = Just editEmptyErr }
                    , Cmd.none
                    )

        EditComponent ->
            ( { model | mode = EditComponentMode }, Cmd.none )

        EmptyClick ->
            case model.website of
                Just _ ->
                    ( { model | internalErr = Just notEmptyErr }
                    , Cmd.none
                    )

                Nothing ->
                    createTopNode model

        InsertText ->
            ( { model | mode = InsertTextMode }, Cmd.none )

        Viewport vp ->
            ( { model | viewport = vp }
            , Cmd.none
            )

        DoNothing ->
            ( model, Cmd.none )


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { viewport = initViewport
      , mode = SelectorMode
      , website = Nothing
      , internalErr = Nothing
      , focussedNode = Nothing
      , maxId = 0
      }
    , Task.perform Viewport Dom.getViewport
    )


initViewport : Dom.Viewport
initViewport =
    { scene =
        { width = 0
        , height = 0
        }
    , viewport =
        { x = 0
        , y = 0
        , width = 0
        , height = 0
        }
    }


homePage : Model -> E.Element Msg
homePage model =
    E.row
        [ E.width E.fill ]
        [ emptyOrErrOrFull model model.website model.internalErr
        , tools model
        ]


emptyOrErrOrFull :
    Model
    -> Maybe Website
    -> Maybe String
    -> E.Element Msg
emptyOrErrOrFull model website err =
    case err of
        Just e ->
            E.text e

        Nothing ->
            case website of
                Nothing ->
                    emptySite

                Just something ->
                    showWebsite model something


emptySite : E.Element Msg
emptySite =
    E.row
        [ E.width E.fill
        , E.height E.fill
        , Ev.onClick EmptyClick
        ]
        []


showText : String -> Int -> E.Element Msg
showText txt id =
    E.el [ Ev.onClick (Clicked id) ] <|
        E.text txt


showWebsite : Model -> Website -> E.Element Msg
showWebsite model website =
    E.el [ E.width E.fill ] <|
        case website of
            ( Text text, id ) ->
                case ( model.focussedNode, model.mode ) of
                    ( Just focussedId, EditComponentMode ) ->
                        if focussedId == id then
                            Ei.multiline [ Ev.onClick (Clicked id) ]
                                { onChange = EditText id
                                , text = text
                                , placeholder =
                                    Just <|
                                        Ei.placeholder [] <|
                                            E.text "type text here"
                                , label = Ei.labelHidden "text entry box"
                                , spellcheck = True
                                }

                        else
                            showText text id

                    _ ->
                        showText text id

            ( Row websites, id ) ->
                Debug.log "newRow" <|
                    E.row
                        [ Ev.onClick (Clicked id)
                        , Eb.width 5
                        , Eb.solid
                        , E.height <| E.px 20
                        , E.width E.fill
                        ]
                    <|
                        List.map (showWebsite model) websites

            ( Column websites, id ) ->
                E.column [] <| List.map (showWebsite model) websites


tools : Model -> E.Element Msg
tools model =
    E.column [ E.alignTop ]
        [ Ei.button []
            { onPress = Just InsertText
            , label = E.text "Insert text"
            }
        , Ei.button []
            { onPress = Just InsertRow
            , label = E.text "Insert row"
            }
        , Ei.button []
            { onPress = Just EditComponent
            , label = E.text "Edit"
            }
        , Ei.button []
            { onPress = Just Unselect
            , label = E.text "Unselect"
            }
        ]
