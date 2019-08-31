module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Debug
import Dict
import Element as E
import Element.Border as Eb
import Element.Events as Ev
import Element.Input as Ei
import Html.Attributes as Hat
import Html.Events as Hev
import Json.Decode as Jd
import Parser
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
    , mouseDrag : Maybe MouseMoveData
    , insertion : Insertion
    , widths : Dict.Dict Int E.Length
    , widthMax : Maybe Int
    , widthMin : Maybe Int
    , widthPx : Maybe Int
    , widthFillPortion : Maybe Int
    , widthRadio : Width
    }


type Width
    = Px
    | Shrink
    | Fill
    | FillPortion


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
    | InsertionRadio Insertion
    | WidthRadio Width
    | WidthFillPortion (Maybe Int)
    | WidthPx (Maybe Int)


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


insertComponentAbove :
    List Website
    -> Component
    -> Int
    -> Int
    -> Maybe (List Website)
insertComponentAbove oldWebsites component compId idToBeAbove =
    case oldWebsites of
        [] ->
            Just []

        ( c, id ) :: ws ->
            if id == idToBeAbove then
                Just <| ( component, compId ) :: ( c, id ) :: ws

            else
                case
                    insertComponentAbove
                        ws
                        component
                        compId
                        idToBeAbove
                of
                    Nothing ->
                        Nothing

                    Just ok ->
                        Just <| ( c, id ) :: ok


insertComponentBelow :
    List Website
    -> Component
    -> Int
    -> Int
    -> Maybe (List Website)
insertComponentBelow oldWebsites component compId idToBeBelow =
    case oldWebsites of
        [] ->
            Just []

        ( c, id ) :: ws ->
            if id == idToBeBelow then
                Just <| ( c, id ) :: ( component, compId ) :: ws

            else
                case
                    insertComponentBelow
                        ws
                        component
                        compId
                        idToBeBelow
                of
                    Nothing ->
                        Nothing

                    Just ok ->
                        Just <| ( c, id ) :: ok


updateWebsiteOnClick : Model -> Component -> Website -> Maybe Website
updateWebsiteOnClick model component oldWebsite =
    case ( oldWebsite, model.focussedNode, model.insertion ) of
        ( _, Nothing, _ ) ->
            Nothing

        ( ( Text _, _ ), Just _, _ ) ->
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

        ( ( Row cells, id ), Just fId, RightInsertion ) ->
            if fId == id then
                Nothing

            else
                case
                    insertComponentRight
                        cells
                        component
                        (model.maxId + 1)
                        fId
                of
                    Nothing ->
                        updateRowCells model component cells id

                    Just updatedCells ->
                        Just ( Row updatedCells, id )

        ( ( Row cells, id ), Just fId, InsideInsertion ) ->
            if fId == id then
                Just
                    ( Row <|
                        ( component, model.maxId + 1 )
                            :: cells
                    , id
                    )

            else
                updateRowCells model component cells id

        ( ( Row cells, id ), Just fId, _ ) ->
            if fId == id then
                Nothing

            else
                updateRowCells model component cells id

        ( ( Column cells, id ), Just fId, AboveInsertion ) ->
            if fId == id then
                Nothing

            else
                case
                    insertComponentAbove
                        cells
                        component
                        (model.maxId + 1)
                        fId
                of
                    Nothing ->
                        updateColumnCells model component cells id

                    Just updatedCells ->
                        Just ( Column updatedCells, id )

        ( ( Column cells, id ), Just fId, BelowInsertion ) ->
            if fId == id then
                Nothing

            else
                case
                    insertComponentBelow
                        cells
                        component
                        (model.maxId + 1)
                        fId
                of
                    Nothing ->
                        updateColumnCells model component cells id

                    Just updatedCells ->
                        Just ( Column updatedCells, id )

        ( ( Column cells, id ), Just fId, InsideInsertion ) ->
            if fId == id then
                Just
                    ( Column <|
                        ( component, model.maxId + 1 )
                            :: cells
                    , id
                    )

            else
                updateColumnCells model component cells id

        ( ( Column cells, id ), Just fId, _ ) ->
            if fId == id then
                Nothing

            else
                updateColumnCells model component cells id


insertComponentRight :
    List Website
    -> Component
    -> Int
    -> Int
    -> Maybe (List Website)
insertComponentRight oldWebsites component compId idToBeRightOf =
    case oldWebsites of
        [] ->
            Just []

        ( c, id ) :: ws ->
            if id == idToBeRightOf then
                Just <| ( c, id ) :: ( component, compId ) :: ws

            else
                case
                    insertComponentRight
                        ws
                        component
                        compId
                        idToBeRightOf
                of
                    Nothing ->
                        Nothing

                    Just ok ->
                        Just <| ( c, id ) :: ok


updateColumnCells :
    Model
    -> Component
    -> List Website
    -> Int
    -> Maybe Website
updateColumnCells model component cells id =
    let
        updatedCells =
            List.map
                (updateWebsiteOnClick model component)
                cells
    in
    Just ( Column <| updateCells cells updatedCells, id )


updateRowCells :
    Model
    -> Component
    -> List Website
    -> Int
    -> Maybe Website
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
        WidthRadio width ->
            Debug.log "WidthRadio"
                ( { model
                    | widthRadio = width
                    , widthPx = Nothing
                    , widthFillPortion = Nothing
                    , widths =
                        case ( model.focussedNode, width ) of
                            ( Nothing, _ ) ->
                                model.widths

                            ( Just fId, Px ) ->
                                case model.widthPx of
                                    Nothing ->
                                        model.widths

                                    Just px ->
                                        Dict.insert fId
                                            (E.px px)
                                            model.widths

                            ( Just fId, Shrink ) ->
                                Dict.insert fId E.shrink model.widths

                            ( Just fId, Fill ) ->
                                Dict.insert fId E.fill model.widths

                            ( Just fId, FillPortion ) ->
                                case model.widthFillPortion of
                                    Nothing ->
                                        model.widths

                                    Just fp ->
                                        Dict.insert fId
                                            (E.fillPortion fp)
                                            model.widths
                  }
                , Cmd.none
                )

        WidthFillPortion fillPortion ->
            ( { model
                | widthFillPortion = fillPortion
                , widthRadio = FillPortion
                , widthPx = Nothing
                , widths =
                    if model.widthRadio == FillPortion then
                        case ( model.focussedNode, fillPortion ) of
                            ( Nothing, _ ) ->
                                model.widths

                            ( _, Nothing ) ->
                                model.widths

                            ( Just fId, Just fp ) ->
                                Dict.insert fId
                                    (E.fillPortion fp)
                                    model.widths

                    else
                        model.widths
              }
            , Cmd.none
            )

        WidthPx widthPx ->
            ( { model
                | widthPx = widthPx
                , widthFillPortion = Nothing
                , widthRadio = Px
                , widths =
                    if model.widthRadio == Px then
                        case ( model.focussedNode, widthPx ) of
                            ( Nothing, _ ) ->
                                model.widths

                            ( _, Nothing ) ->
                                model.widths

                            ( Just fId, Just px ) ->
                                Dict.insert fId
                                    (E.px px)
                                    model.widths

                    else
                        model.widths
              }
            , Cmd.none
            )

        InsertionRadio insertion ->
            ( { model | insertion = insertion }, Cmd.none )

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

        InsertColumn ->
            insertComponent model (Column [])

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
      , mouseDrag = Nothing
      , insertion = defaultInsertion
      , widthMax = Nothing
      , widthMin = Nothing
      , widthPx = Nothing
      , widthFillPortion = Nothing
      , widthRadio = Fill
      , widths = Dict.empty
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


showText : String -> Int -> Maybe Int -> Dict.Dict Int E.Length -> E.Element Msg
showText txt id maybeFocussed widths =
    E.paragraph
        ([ Ev.onFocus (Clicked id)
         , E.htmlAttribute <| Hat.tabindex 0
         , E.height E.fill
         , E.width <|
            case Dict.get id widths of
                Nothing ->
                    Debug.log "fill" E.fill

                Just w ->
                    Debug.log "w" w
         ]
            ++ textSelectStyle maybeFocussed id
        )
    <|
        [ E.text txt ]


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
                                , E.width <|
                                    case Dict.get id model.widths of
                                        Nothing ->
                                            E.fill

                                        Just w ->
                                            w
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
                            showText
                                text
                                id
                                model.focussedNode
                                model.widths

                    _ ->
                        showText text
                            id
                            model.focussedNode
                            model.widths

            ( Row websites, id ) ->
                E.row
                    [ Ev.onClick (Clicked id)
                    , emphasised model.focussedNode id
                    , E.htmlAttribute <| Hat.tabindex 0
                    , Eb.widthXY 1 4
                    , E.height E.fill
                    , E.width <|
                        case Dict.get id model.widths of
                            Nothing ->
                                E.fill

                            Just w ->
                                w
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
                    , E.width <|
                        case Dict.get id model.widths of
                            Nothing ->
                                E.fill

                            Just w ->
                                w
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


chooseWidthTool : Model -> E.Element Msg
chooseWidthTool model =
    E.column [ E.spacing 10 ]
        [ Ei.radio [ E.spacing 10 ]
            { onChange = WidthRadio
            , selected = Just model.widthRadio
            , label = Ei.labelAbove [] <| E.text "width options"
            , options =
                [ Ei.option Shrink <| E.text "shrink to fit"
                , Ei.option Fill <| E.text "fill space"
                ]
            }
        , intBox
            { value = model.widthFillPortion
            , msg = WidthFillPortion
            , label = "fill portion"
            }
        , intBox
            { value = model.widthPx
            , msg = WidthPx
            , label = "in pixels"
            }
        ]


type alias IntBox =
    { value : Maybe Int
    , msg : Maybe Int -> Msg
    , label : String
    }


intDec : (Maybe Int -> Msg) -> String -> Msg
intDec msg raw =
    case Parser.run Parser.int raw of
        Err _ ->
            msg Nothing

        Ok i ->
            msg <| Just i


intBox : IntBox -> E.Element Msg
intBox { value, msg, label } =
    Ei.text [ E.width <| E.px 80 ]
        { onChange = intDec msg
        , text = intBoxText value
        , placeholder =
            Just <|
                Ei.placeholder [] <|
                    E.text "Type a number"
        , label = Ei.labelRight [] <| E.text label
        }


intBoxText : Maybe Int -> String
intBoxText maybeInt =
    case maybeInt of
        Nothing ->
            ""

        Just i ->
            String.fromInt i



-- portionWidthRadio : Model -> E.Element Msg
-- portionWidthRadio model =
--     Ei.text
--         { onChange = widthPortionDec
--         , text = widthPxText model.widthPx
--         , placeholder =
--             Just <|
--                 Ei.placeholder [] <|
--                     E.text "Type a number"
--         , label = Ei.labelLeft [] <| E.text "in pixels"
--         }
-- widthPxDec : String -> Msg
-- widthPxDec raw =
--     case Parser.run Parser.int raw of
--         Err _ ->
--             WidthPx Nothing
--
--         Ok i ->
--             WidthPx <| Just i
--
--
-- widthPxText : Maybe Int -> String
-- widthPxText maybeWidth =
--     case maybeWidth of
--         Nothing ->
--             "Type a number"
--
--         Just i ->
--             String.fromInt i
--
--
-- pxWidthRadio : Model -> E.Element Msg
-- pxWidthRadio model =
--     Ei.text
--         { onChange = widthPxDec
--         , text = widthPxText model.widthPx
--         , placeholder =
--             Just <|
--                 Ei.placeholder [] <|
--                     E.text "Type a number"
--         , label = Ei.labelLeft [] <| E.text "in pixels"
--         }


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
                        Just <| Column cells

                    else
                        firstJust <| List.map (parent model) cells


textEditButton : E.Element Msg
textEditButton =
    Ei.button []
        { onPress = Just EditTextButton
        , label = E.text "Edit text"
        }


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



{-
   insertLeftCheckbox : Insertion -> E.Element Msg
   insertLeftCheckbox insertion =
       Ei.checkbox []
           { onChange = InsertOnLeftChecked
           , icon = Ei.defaultCheckbox
           , checked = insertion == LeftInsertion
           , label = Ei.labelRight [] <| E.text "Insert on left"
           }
-}
{-
   insertAboveCheckbox : Bool -> E.Element Msg
   insertAboveCheckbox checked =
       Ei.checkbox []
           { onChange = InsertAboveChecked
           , icon = Ei.defaultCheckbox
           , checked = checked
           , label = Ei.labelRight [] <| E.text "Insert above"
           }
-}


selectedEmptyRowInRow : Model -> List (E.Element Msg)
selectedEmptyRowInRow model =
    [ insertRowButton
    , leftRightInsideRadio model.insertion
    , selectButton
    ]


emptyRowOrColButtons : Model -> List (E.Element Msg)
emptyRowOrColButtons model =
    [ insertTextButton
    , insertColumnButton
    , insertRowButton
    , maybeSelectionButton model
    , chooseWidthTool model
    ]


tools : Model -> E.Element Msg
tools model =
    E.column [ E.alignTop, E.alignRight, E.width <| E.px 200, E.spacing 20 ] <|
        case model.website of
            Nothing ->
                startUpTools

            Just website ->
                case ( Debug.log "focussed" <| findFocussed model website, Debug.log "parent" <| parent model website ) of
                    ( Nothing, _ ) ->
                        [ maybeSelectionButton model ]

                    ( Just (Text _), Nothing ) ->
                        [ textEditButton
                        , chooseWidthTool model
                        ]

                    ( Just (Row []), Nothing ) ->
                        emptyRowOrColButtons model

                    ( Just (Row (x :: xs)), Nothing ) ->
                        [ maybeSelectionButton model
                        ]

                    ( Just (Column []), Nothing ) ->
                        emptyRowOrColButtons model

                    ( Just (Row []), Just (Row parentCells) ) ->
                        [ insertRowButton
                        , insertColumnButton
                        , insertTextButton
                        , leftRightInsideRadio model.insertion
                        , maybeSelectionButton model
                        , chooseWidthTool model
                        ]

                    ( Just (Text _), Just (Row _) ) ->
                        [ textEditButton
                        , insertRowButton
                        , insertColumnButton
                        , insertTextButton
                        , leftRightRadio model.insertion
                        , maybeSelectionButton model
                        , chooseWidthTool model
                        ]

                    ( _, Just (Text _) ) ->
                        fatalErrTools
                            "An element can't have text for a parent."

                    ( Just (Text _), Just (Column (_ :: _)) ) ->
                        [ textEditButton
                        , insertRowButton
                        , insertColumnButton
                        , insertTextButton
                        , upDownRadio model.insertion
                        , maybeSelectionButton model
                        , chooseWidthTool model
                        ]

                    ( Just (Row []), Just (Column (_ :: _)) ) ->
                        [ insertRowButton
                        , insertColumnButton
                        , insertTextButton
                        , upDownInsideRadio model.insertion
                        , maybeSelectionButton model
                        , chooseWidthTool model
                        ]

                    ( _, Just (Row []) ) ->
                        fatalErrTools
                            "An empty row can't have a child."

                    ( Just (Row (_ :: _)), Just (Row (_ :: _)) ) ->
                        [ insertRowButton
                        , insertColumnButton
                        , insertTextButton
                        , leftRightRadio model.insertion
                        , maybeSelectionButton model
                        , chooseWidthTool model
                        ]

                    ( _, Just (Column []) ) ->
                        fatalErrTools
                            "An empty column can't have a child."

                    ( Just (Row (_ :: _)), Just (Column (_ :: _)) ) ->
                        [ insertRowButton
                        , insertColumnButton
                        , insertTextButton
                        , upDownRadio model.insertion
                        , maybeSelectionButton model
                        , chooseWidthTool model
                        ]

                    ( Just (Column (_ :: _)), Nothing ) ->
                        [ maybeSelectionButton model
                        , chooseWidthTool model
                        ]

                    ( Just (Column []), Just (Row (_ :: _)) ) ->
                        [ insertRowButton
                        , insertColumnButton
                        , insertTextButton
                        , leftRightInsideRadio model.insertion
                        , maybeSelectionButton model
                        , chooseWidthTool model
                        ]

                    ( Just (Column []), Just (Column (_ :: _)) ) ->
                        [ insertRowButton
                        , insertColumnButton
                        , insertTextButton
                        , upDownInsideRadio model.insertion
                        , maybeSelectionButton model
                        , chooseWidthTool model
                        ]

                    ( Just (Column (_ :: _)), Just (Row (_ :: _)) ) ->
                        [ insertRowButton
                        , insertColumnButton
                        , insertTextButton
                        , leftRightRadio model.insertion
                        , maybeSelectionButton model
                        , chooseWidthTool model
                        ]

                    ( Just (Column (_ :: _)), Just (Column (_ :: _)) ) ->
                        [ insertRowButton
                        , insertColumnButton
                        , insertTextButton
                        , upDownRadio model.insertion
                        , maybeSelectionButton model
                        , chooseWidthTool model
                        ]


leftRightInsideRadio : Insertion -> E.Element Msg
leftRightInsideRadio insertion =
    Ei.radio []
        { onChange = InsertionRadio
        , selected = Just insertion
        , label =
            Ei.labelAbove [] <| E.text "insertion options"
        , options =
            [ Ei.option LeftInsertion <| E.text "Insert on left"
            , Ei.option RightInsertion <| E.text "Insert on right"
            , Ei.option InsideInsertion <| E.text "Insert inside"
            ]
        }


upDownInsideRadio : Insertion -> E.Element Msg
upDownInsideRadio insertion =
    Ei.radio []
        { onChange = InsertionRadio
        , selected = Just insertion
        , label =
            Ei.labelAbove [] <| E.text "insertion options"
        , options =
            [ Ei.option AboveInsertion <| E.text "Insert above"
            , Ei.option BelowInsertion <| E.text "Insert below"
            , Ei.option InsideInsertion <| E.text "Insert inside"
            ]
        }


upDownRadio : Insertion -> E.Element Msg
upDownRadio insertion =
    Ei.radio []
        { onChange = InsertionRadio
        , selected = Just insertion
        , label =
            Ei.labelAbove [] <| E.text "insertion options"
        , options =
            [ Ei.option AboveInsertion <| E.text "Insert above"
            , Ei.option BelowInsertion <| E.text "Insert below"
            ]
        }


leftRightRadio : Insertion -> E.Element Msg
leftRightRadio insertion =
    Ei.radio []
        { onChange = InsertionRadio
        , selected = Just insertion
        , label =
            Ei.labelAbove [] <| E.text "Insertion options"
        , options =
            [ Ei.option LeftInsertion <| E.text "Insert on left"
            , Ei.option RightInsertion <| E.text "Insert on right"
            ]
        }


fatalErrTools : String -> List (E.Element Msg)
fatalErrTools err =
    [ E.text "fatal error:"
    , E.text err
    ]


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


insertTextButton : E.Element Msg
insertTextButton =
    Ei.button []
        { onPress = Just InsertText
        , label = E.text "Insert text"
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


insertColumnButton : E.Element Msg
insertColumnButton =
    Ei.button []
        { onPress = Just InsertColumn
        , label = E.text "Insert column"
        }


maybeSelectionButton : Model -> E.Element Msg
maybeSelectionButton model =
    if
        model.mode
            == SelectionMode
            && model.focussedNode
            == Nothing
    then
        E.none

    else
        selectButton



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
