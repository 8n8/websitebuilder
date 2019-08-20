module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Debug
import Element as E
import Element.Border as Eb
import Element.Events as Ev
import Element.Input as Ei
import Html.Attributes as Hat
import Set
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
    , focussedNodes : Set.Set Int
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
    | InsertColumnMode
    | EditComponentMode


type Msg
    = DoNothing
    | Viewport Dom.Viewport
    | InsertText
    | EmptyClick
    | EditComponent
    | EditText Int String
    | Clicked Int
    | Unselect
    | InsertRow
    | InsertColumn
    | Unclicked Int


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

        InsertColumnMode ->
            ( { model
                | website = Just ( Column [], 0 )
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


someJust : List (Maybe Website) -> Bool
someJust mw =
    List.any ((/=) Nothing) mw


updateCells : List Website -> List (Maybe Website) -> List Website
updateCells olds news =
    List.map2 updateCell olds news


updateCell : Website -> Maybe Website -> Website
updateCell old maybeNew =
    case maybeNew of
        Nothing ->
            old

        Just new ->
            new


containsIds : Set.Set Int -> Website -> Bool
containsIds ids ws =
    List.any (\id -> containsId id ws) <| Set.toList ids


containsId : Int -> Website -> Bool
containsId id ws =
    case ws of
        ( Text _, compId ) ->
            compId == id

        ( Row [], rowId ) ->
            rowId == id

        ( Row children, rowId ) ->
            if rowId == id then
                True

            else
                List.any (containsId id) children

        ( Column [], colId ) ->
            colId == id

        ( Column children, colId ) ->
            if colId == id then
                True

            else
                List.any (containsId id) children


{-| It decides if parent with ID id has children with an ID in
focussed.

focussedChildren : Int -> Set.Set Int -> Website -> Bool
focussedChildren id focussed ws =
case ws of
(Text _, _) -> False
(Row [], id) -> False
(Row children, rowId) ->
if rowId == id then
List.any (focussedChildren

-}
updateWebsiteOnClick : Model -> Int -> Website -> Maybe Website
updateWebsiteOnClick model clickId oldWebsite =
    case ( oldWebsite, model.mode ) of
        -- ( ( Row cells, id ), InsertTextMode ) ->
        --     if id == clickId then
        --         ( Row <|
        --             ( Text "Enter text here", model.maxId + 1 )
        --                 :: cells
        --         , id
        --         )
        --     else
        --         ( Row <|
        --             List.map
        --                 (updateWebsiteOnClick model clickId)
        --                 cells
        --         , id
        --         )
        -- ( ( Row cells, id ), InsertColumnMode ) ->
        --     if id == clickId then
        --         ( Row <|
        --             ( Column [], model.maxId + 1 )
        --                 :: cells
        --         , id
        --         )
        --     else
        --         ( Row <|
        --             List.map
        --                 (updateWebsiteOnClick model clickId)
        --                 cells
        --         , id
        --         )
        ( ( Row cells, id ), InsertRowMode ) ->
            let
                updatedCells =
                    List.map
                        (updateWebsiteOnClick model clickId)
                        cells

                focussedChild =
                    List.any (containsIds model.focussedNodes) cells
            in
            if Debug.log "focussedChild" focussedChild then
                Nothing

            else if someJust updatedCells then
                Just ( Row <| updateCells cells updatedCells, id )

            else if id == clickId then
                Just
                    ( Row <|
                        ( Row [], model.maxId + 1 )
                            :: cells
                    , id
                    )

            else
                Nothing

        -- if id == clickId then
        --     ( Row <|
        --         ( Row [], model.maxId + 1 )
        --             :: cells
        --     , id
        --     )
        -- else
        --     ( Row <|
        --         List.map
        --             (updateWebsiteOnClick model clickId)
        --             cells
        --     , id
        --     )
        -- ( ( Column cells, id ), InsertTextMode ) ->
        --     if id == clickId then
        --         ( Column <|
        --             ( Text "Enter text here", model.maxId + 1 )
        --                 :: cells
        --         , id
        --         )
        --     else
        --         ( Column <|
        --             List.map
        --                 (updateWebsiteOnClick model clickId)
        --                 cells
        --         , id
        --         )
        -- ( ( Column cells, id ), InsertColumnMode ) ->
        --     if id == clickId then
        --         ( Column <|
        --             ( Column [], model.maxId + 1 )
        --                 :: cells
        --         , id
        --         )
        --     else
        --         ( Column <|
        --             List.map
        --                 (updateWebsiteOnClick model clickId)
        --                 cells
        --         , id
        --         )
        -- ( ( Column cells, id ), InsertRowMode ) ->
        --     if id == clickId then
        --         ( Column <|
        --             ( Row [], model.maxId + 1 )
        --                 :: cells
        --         , id
        --         )
        --     else
        --         ( Column <|
        --             List.map
        --                 (updateWebsiteOnClick model clickId)
        --                 cells
        --         , id
        --         )
        _ ->
            Nothing


clickNothingErr =
    "can't click on something that isn't there"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Unclicked id ->
            ( { model
                | focussedNodes = Set.remove (Debug.log "unclicked id" id) model.focussedNodes
              }
            , Cmd.none
            )

        InsertRow ->
            ( { model | mode = InsertRowMode }, Cmd.none )

        InsertColumn ->
            ( { model | mode = InsertColumnMode }, Cmd.none )

        Unselect ->
            ( { model | focussedNodes = Set.empty }, Cmd.none )

        Clicked id ->
            case model.website of
                Nothing ->
                    ( { model | internalErr = Just clickNothingErr }
                    , Debug.log "Clicked - Nothing" Cmd.none
                    )

                Just oldWebsite ->
                    case updateWebsiteOnClick model id oldWebsite of
                        Nothing ->
                            ( Debug.log "model if site not updated"
                                { model
                                    | focussedNodes =
                                        Set.insert
                                            (Debug.log "id" id)
                                            model.focussedNodes
                                }
                            , Cmd.none
                            )

                        Just newSite ->
                            ( Debug.log "model"
                                { model
                                    | focussedNodes =
                                        Set.insert
                                            (model.maxId + 1)
                                        <|
                                            Set.insert
                                                id
                                                model.focussedNodes
                                    , website = Just newSite
                                    , maxId = model.maxId + 1
                                }
                            , Cmd.none
                            )

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
      , mode = InsertRowMode
      , website = Nothing
      , internalErr = Nothing
      , focussedNodes = Set.empty
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
    E.el
        [ Ev.onFocus (Clicked id)
        , Ev.onLoseFocus (Unclicked id)
        , E.htmlAttribute <| Hat.tabindex 0
        ]
    <|
        E.text txt


showWebsite : Model -> Website -> E.Element Msg
showWebsite model website =
    E.el [ E.width E.fill, E.alignTop, E.height E.fill ] <|
        case website of
            ( Text text, id ) ->
                case model.mode of
                    EditComponentMode ->
                        if Set.member id model.focussedNodes then
                            Ei.multiline
                                [ Ev.onFocus <| Clicked id
                                , Ev.onLoseFocus <| Unclicked id
                                ]
                                { onChange = EditText id
                                , text = text
                                , placeholder =
                                    Just <|
                                        Ei.placeholder [] <|
                                            E.text "type text here"
                                , label =
                                    Ei.labelHidden
                                        "text entry box"
                                , spellcheck = True
                                }

                        else
                            showText text id

                    _ ->
                        showText text id

            ( Row websites, id ) ->
                E.row
                    [ Ev.onClick (Clicked id)
                    , Ev.onLoseFocus (Unclicked id)
                    , E.htmlAttribute <| Hat.tabindex 0
                    , Eb.widthXY 1 2
                    , Eb.dashed
                    , E.height E.fill
                    , E.width E.fill
                    , E.spacing gap
                    , E.padding gap
                    ]
                <|
                    List.map (showWebsite model) websites

            ( Column websites, id ) ->
                E.column
                    [ Ev.onClick (Clicked id)
                    , Eb.widthXY 2 1
                    , Eb.dashed
                    , E.height E.fill
                    , E.width E.fill
                    , E.padding gap
                    , E.spacing gap
                    ]
                <|
                    List.map (showWebsite model) websites


gap =
    20


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
            { onPress = Just InsertColumn
            , label = E.text "Insert column"
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
