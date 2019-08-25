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
import Html.Events as Hev
import Json.Decode as Jd
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
    , focussedNode : Maybe Int
    , internalErr : Maybe String
    , maxId : Int
    , freezeAddChild : Bool
    , mouseDrag : Maybe MouseMoveData
    , insertion : Insertion
    }


type Insertion
    = LeftInsertion
    | RightInsertion
    | AboveInsertion
    | BelowInsertion
    | InsideInsertion


defaultInsertion =
    InsideInsertion


decoder : Jd.Decoder MouseMoveData
decoder =
    Jd.map2 MouseMoveData
        (Jd.at [ "offsetX" ] Jd.int)
        (Jd.at [ "offsetY" ] Jd.int)


insertComponent :
    Model
    -> Component
    -> ( Model, Cmd Msg )
insertComponent model component =
    ( case model.website of
        Nothing ->
            { model | website = Just ( component, 0 ) }

        Just website ->
            case model.focussedNode of
                Nothing ->
                    model

                Just _ ->
                    case
                        updateWebsiteOnClick
                            model
                            component
                            website
                    of
                        Just newSite ->
                            { model
                                | website = Just newSite
                                , mode = None
                                , focussedNode = Nothing
                                , maxId = model.maxId + 1
                            }

                        Nothing ->
                            model
    , Cmd.none
    )


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
    | EditTextMode
    | SelectionMode
    | None


type Msg
    = DoNothing
    | Viewport Dom.Viewport
    | NoneMode
    | InsertText
    | EmptyClick
    | EditTextButton
    | EditText Int String
    | Clicked Int
    | Unselect
    | InsertRow
    | InsertColumn
    | Unclicked Int
    | Select
    | InsertOnLeftChecked Bool


notEmptyErr =
    "received EmptyClick but the website is not empty"


nothingToSelectErr =
    "selection mode is not allowed when there is nothing to select"


nothingToEditErr =
    "edit mode is not allowed when there is nothing to edit"


createTopNode : Model -> ( Model, Cmd Msg )
createTopNode model =
    ( { model
        | website = Just ( Row [], 0 )
      }
    , Cmd.none
    )


selectNothingErr =
    "can't select something that isn't there"


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


firstJust : List (Maybe a) -> Maybe a
firstJust ms =
    case List.filter (\x -> x /= Nothing) ms of
        [] ->
            Nothing

        head :: _ ->
            head


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


childest : Int -> Int -> Website -> Maybe Int
childest id1 id2 ws =
    case ws of
        ( Text _, id ) ->
            if id == id1 then
                Just id1

            else if id == id2 then
                Just id2

            else
                Nothing

        ( Row cells, id ) ->
            case firstJust <| List.map (childest id1 id2) cells of
                Nothing ->
                    if id == id1 then
                        Just id1

                    else if id == id2 then
                        Just id2

                    else
                        Nothing

                Just childId ->
                    Just childId

        ( Column cells, id ) ->
            case firstJust <| List.map (childest id1 id2) cells of
                Nothing ->
                    if id == id1 then
                        Just id1

                    else if id == id2 then
                        Just id2

                    else
                        Nothing

                Just childId ->
                    Just childId



-- let
--     id1InCell = List.map (containsId id1) cells
--     id1InCells = List.any id1InCell
--     id2InCell = List.map (containsId id2) cells
--     id2InCells = List.any id2InCell
-- in
-- case ( id == id1, id == id2, id1InCells, id2InCells ) of
--     (False, False, False, False) -> Nothing
--     (False, False, False, True) -> Nothing
--     (False, False, True, False) -> Nothing
--     (False, False, True, True) ->
--     (False, True, False, False) ->
--     (False, True, False, True) ->
--     (False, True, True, False) ->
--     (False, True, True, True) ->
--     (True, False, False, False) ->
--     (True, False, False, True) ->
--     (True, False, True, False) ->
--     (True, False, True, True) ->
--     (True, True, False, False) ->
--     (True, True, False, True) ->
--     (True, True, True, False) ->
--     (True, True, True, True) ->


containsIds : Set.Set Int -> Website -> Bool
containsIds ids ws =
    List.any (\id -> containsId id ws) <| Set.toList ids


textSelectStyle : Maybe Int -> Int -> List (E.Attribute Msg)
textSelectStyle maybeFId id =
    case maybeFId of
        Nothing ->
            []

        Just fId ->
            if fId == id then
                [ Eb.width 3
                , darkBlue
                ]

            else
                []


emphasised : Maybe Int -> Int -> E.Attribute Msg
emphasised focussed id =
    case focussed of
        Nothing ->
            paleBlue

        Just f ->
            if f == id then
                darkBlue

            else
                paleBlue


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


insertComponentLeft :
    List Website
    -> Component
    -> Int
    -> Int
    -> Maybe (List Website)
insertComponentLeft oldWebsites component compId idToBeLeftOf =
    case oldWebsites of
        [] ->
            Just []

        ( c, id ) :: ws ->
            if id == idToBeLeftOf then
                Just <| ( component, compId ) :: ( c, id ) :: ws

            else
                case
                    insertComponentLeft
                        ws
                        component
                        compId
                        idToBeLeftOf
                of
                    Nothing ->
                        Nothing

                    Just ok ->
                        Just <| ( c, id ) :: ok


updateWebsiteOnClick : Model -> Component -> Website -> Maybe Website
updateWebsiteOnClick model component oldWebsite =
    case ( oldWebsite, model.focussedNode, model.insertion ) of
        ( ( Row cells, id ), Nothing, _ ) ->
            Nothing

        ( ( Row cells, id ), Just fId, LeftInsertion ) ->
            if fId == id then
                Nothing

            else
                case
                    insertComponentLeft
                        cells
                        component
                        (model.maxId + 1)
                        fId
                of
                    Nothing ->
                        updateRowCells model component cells id

                    Just updatedCells ->
                        Just ( Row updatedCells, id )

        ( ( Row cells, id ), Just fId, _ ) ->
            if fId == id then
                Just
                    ( Row <|
                        ( component, model.maxId + 1 )
                            :: cells
                    , id
                    )

            else
                updateRowCells model component cells id

        _ ->
            Nothing


updateRowCells : Model -> Component -> List Website -> Int -> Maybe Website
updateRowCells model component cells id =
    let
        updatedCells =
            List.map
                (updateWebsiteOnClick
                    model
                    component
                )
                cells
    in
    Just
        ( Row <|
            updateCells cells updatedCells
        , id
        )


updateRowOrCol :
    Model
    -> Int
    -> List Website
    -> Component
    -> (List Website -> Component)
    -> Maybe Website
updateRowOrCol model id cells component rowOrCol =
    case model.focussedNode of
        Nothing ->
            Nothing

        Just fId ->
            if fId == id then
                Just
                    ( rowOrCol <|
                        ( component, model.maxId + 1 )
                            :: cells
                    , id
                    )

            else
                let
                    updatedCells =
                        List.map
                            (updateWebsiteOnClick model component)
                            cells
                in
                Just
                    ( rowOrCol <|
                        updateCells cells updatedCells
                    , id
                    )


colOrRow : Mode -> Maybe (List Website -> Component)
colOrRow mode =
    case mode of
        InsertRowMode ->
            Just Row

        InsertColumnMode ->
            Just Column

        _ ->
            Nothing


clickNothingErr =
    "can't click on something that isn't there"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InsertOnLeftChecked True ->
            ( { model
                | insertion = LeftInsertion
              }
            , Cmd.none
            )

        InsertOnLeftChecked False ->
            ( { model
                | insertion = defaultInsertion
              }
            , Cmd.none
            )

        NoneMode ->
            ( { model | mode = None }, Cmd.none )

        Unclicked id ->
            case model.focussedNode of
                Nothing ->
                    ( model, Cmd.none )

                Just fid ->
                    if id == fid then
                        ( { model
                            | focussedNode = Nothing
                            , mode = None
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

        Select ->
            ( { model
                | mode = SelectionMode
                , focussedNode = Nothing
              }
            , Cmd.none
            )

        InsertRow ->
            insertComponent model (Row [])

        -- ( case model.website of
        --     Nothing ->
        --         { model | website = Just ( Row [], 0 ) }
        --     Just website ->
        --         case model.focussedNode of
        --             Nothing ->
        --                 model
        --             Just _ ->
        --                 case updateWebsiteOnClick model website of
        --                     Just newSite ->
        --                         { model
        --                             | website = Just newSite
        --                             , mode = None
        --                             , focussedNode = Nothing
        --                             , maxId = model.maxId + 1
        --                         }
        --                     Nothing ->
        --                         model
        -- , Cmd.none
        -- )
        InsertColumn ->
            insertComponent model (Column [])

        -- ( { model | mode = InsertColumnMode }, Cmd.none )
        Unselect ->
            ( { model | focussedNode = Nothing }, Cmd.none )

        Clicked id ->
            Debug.log "Clicked"
                ( if model.mode == SelectionMode then
                    case ( model.focussedNode, model.website ) of
                        ( Nothing, Just _ ) ->
                            { model | focussedNode = Just id }

                        ( Just lastFocus, Just website ) ->
                            { model
                                | focussedNode =
                                    Debug.log "childest" <|
                                        childest
                                            (Debug.log "lastFocus" lastFocus)
                                            (Debug.log "id" id)
                                            website
                            }

                        _ ->
                            { model
                                | internalErr =
                                    Just
                                        clickNothingErr
                            }

                  else
                    model
                , Cmd.none
                )

        -- Debug.log "clicked" <|
        --     case model.website of
        --         Nothing ->
        --             ( { model | internalErr = Just clickNothingErr }
        --             , Cmd.none
        --             )
        --         Just oldWebsite ->
        --             if model.freezeAddChild then
        --                 ( model, Cmd.none )
        --             else
        --                 case updateWebsiteOnClick model id oldWebsite of
        --                     Nothing ->
        --                         ( { model
        --                             | focussedNodes =
        --                                 Set.insert
        --                                     (Debug.log "id" id)
        --                                     model.focussedNodes
        --                           }
        --                         , Cmd.none
        --                         )
        --                     Just newSite ->
        --                         ( { model
        --                             | focussedNodes =
        --                                 Set.insert
        --                                     (model.maxId + 1)
        --                                 <|
        --                                     Set.insert
        --                                         id
        --                                         model.focussedNodes
        --                             , website = Just newSite
        --                             , maxId = model.maxId + 1
        --                             , freezeAddChild = True
        --                           }
        --                         , Cmd.none
        --                         )
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

        EditTextButton ->
            ( { model | mode = EditTextMode }, Cmd.none )

        EmptyClick ->
            case model.mode of
                None ->
                    ( model, Cmd.none )

                SelectionMode ->
                    ( model, Cmd.none )

                _ ->
                    createTopNode model

        InsertText ->
            insertComponent { model | mode = InsertTextMode } (Text "Insert some text here")

        Viewport vp ->
            ( { model | viewport = vp }
            , Cmd.none
            )

        DoNothing ->
            ( model, Cmd.none )


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { viewport = initViewport
      , mode = None
      , website = Nothing
      , internalErr = Nothing
      , focussedNode = Nothing
      , maxId = 0
      , freezeAddChild = True
      , mouseDrag = Nothing
      , insertion = defaultInsertion
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
        [ E.width E.fill
        ]
        [ tools model
        , emptyOrErrOrFull model model.website model.internalErr
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


showText : String -> Int -> Maybe Int -> E.Element Msg
showText txt id maybeFocussed =
    E.el
        ([ Ev.onFocus (Clicked id)
         , E.htmlAttribute <| Hat.tabindex 0
         ]
            ++ textSelectStyle maybeFocussed id
        )
    <|
        E.text txt


showWebsite : Model -> Website -> E.Element Msg
showWebsite model website =
    E.el
        [ E.width E.fill
        , E.alignTop
        , E.height E.fill
        ]
    <|
        case website of
            ( Text text, id ) ->
                case model.mode of
                    EditTextMode ->
                        if model.focussedNode == Just id then
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
                            showText text id model.focussedNode

                    _ ->
                        showText text id model.focussedNode

            ( Row websites, id ) ->
                E.row
                    [ Ev.onClick (Clicked id)
                    , emphasised model.focussedNode id
                    , E.htmlAttribute <| Hat.tabindex 0
                    , Eb.widthXY 1 4
                    , E.height E.fill
                    , E.width E.fill
                    , Eb.dashed
                    , E.spacing gap
                    , E.padding gap
                    ]
                <|
                    List.map (showWebsite model) websites

            ( Column websites, id ) ->
                E.column
                    [ Ev.onClick (Clicked id)
                    , emphasised model.focussedNode id
                    , Eb.widthXY 4 1
                    , E.height E.fill
                    , E.width E.fill
                    , Eb.dashed
                    , E.padding gap
                    , E.spacing gap
                    ]
                <|
                    List.map (showWebsite model) websites


paleBlue =
    Eb.color <| E.rgb255 195 214 247


darkBlue =
    Eb.color <| E.rgb255 29 93 204


gap =
    20


findFocussed model website =
    case ( website, model.focussedNode ) of
        ( _, Nothing ) ->
            Nothing

        ( ( Text t, id ), Just fid ) ->
            if fid == id then
                Just (Text t)

            else
                Nothing

        ( ( Row cells, id ), Just fid ) ->
            if fid == id then
                Just (Row cells)

            else
                firstJust <| List.map (findFocussed model) cells

        ( ( Column cells, id ), Just fid ) ->
            if fid == id then
                Just (Column cells)

            else
                firstJust <| List.map (findFocussed model) cells


parent : Model -> Website -> Maybe Component
parent model website =
    case model.focussedNode of
        Nothing ->
            Nothing

        Just fId ->
            case website of
                ( Text _, _ ) ->
                    Nothing

                ( Row cells, id ) ->
                    if List.any (\( _, cid ) -> cid == fId) cells then
                        Just <| Row cells

                    else
                        firstJust <| List.map (parent model) cells

                ( Column cells, id ) ->
                    if List.any (\( _, cid ) -> cid == fId) cells then
                        Just <| Row cells

                    else
                        firstJust <| List.map (parent model) cells


textEditButtons : List (E.Element Msg)
textEditButtons =
    [ Ei.button []
        { onPress = Just EditTextButton
        , label = E.text "Edit text"
        }
    ]


startUpTools : List (E.Element Msg)
startUpTools =
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
    ]


selectButton : E.Element Msg
selectButton =
    Ei.button []
        { onPress = Just Select
        , label = E.text "Select"
        }


insertOnLeftCheckbox : Bool -> E.Element Msg
insertOnLeftCheckbox checked =
    Ei.checkbox []
        { onChange = InsertOnLeftChecked
        , icon = Ei.defaultCheckbox
        , checked = checked
        , label = Ei.labelRight [] <| E.text "Insert on left"
        }


selectedEmptyRowInRow : Model -> List (E.Element Msg)
selectedEmptyRowInRow model =
    [ insertRowButton
    , insertOnLeftCheckbox <| model.insertion == LeftInsertion
    , selectButton
    ]


tools : Model -> E.Element Msg
tools model =
    E.column [ E.alignTop, E.alignRight, E.width <| E.px 150 ] <|
        case model.website of
            Nothing ->
                startUpTools

            Just website ->
                case ( Debug.log "focussed" <| findFocussed model website, Debug.log "parent" <| parent model website ) of
                    ( Nothing, Nothing ) ->
                        if model.mode == SelectionMode then
                            []

                        else
                            [ Ei.button []
                                { onPress = Just Select
                                , label = E.text "Select"
                                }
                            ]

                    ( Just (Text _), Nothing ) ->
                        textEditButtons

                    ( Just (Row []), Nothing ) ->
                        oneTopRowOrCol model.mode

                    ( Just (Row (x :: xs)), Nothing ) ->
                        oneTopRowOrCol model.mode

                    ( Just (Column []), Nothing ) ->
                        oneTopRowOrCol model.mode

                    ( Just (Row []), Just (Row parentCells) ) ->
                        selectedEmptyRowInRow model

                    _ ->
                        []


type alias MouseMoveData =
    { x : Int
    , y : Int
    }


insertRowButton : E.Element Msg
insertRowButton =
    Ei.button []
        { onPress = Just InsertRow
        , label = E.text "Insert row"
        }


oneTopRowOrCol : Mode -> List (E.Element Msg)
oneTopRowOrCol mode =
    [ Ei.button []
        { onPress = Just InsertText
        , label = E.text "Insert text"
        }
    , insertRowButton
    , Ei.button []
        { onPress = Just InsertColumn
        , label = E.text "Insert column"
        }
    ]
        ++ (if mode == SelectionMode then
                []

            else
                [ Ei.button []
                    { onPress = Just Select
                    , label = E.text "Select"
                    }
                ]
           )



--     E.column [ E.alignTop ]
--         [ Ei.button []
--             { onPress = Just InsertText
--             , label = E.text "Insert text"
--             }
--         , Ei.button []
--             { onPress = Just InsertRow
--             , label = E.text "Insert row"
--             }
--         , Ei.button []
--             { onPress = Just InsertColumn
--             , label = E.text "Insert column"
--             }
--         , Ei.button []
--             { onPress = Just EditTextButton
--             , label = E.text "Edit text"
--             }
--         , Ei.button []
--             { onPress = Just Select
--             , label = E.text "Select"
--             }
--         , Ei.button []
--             { onPress = Just Unselect
--             , label = E.text "Unselect"
--             }
--         , Ei.button []
--             { onPress = Just NoneMode
--             , label = E.text "Passive mode"
--             }
--         ]
