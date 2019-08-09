module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Element as E
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
    }


type Mode
    = InsertTextMode
    | SelectorMode


type Msg
    = DoNothing
    | Viewport Dom.Viewport
    | InsertText


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
    ( { viewport = initViewport, mode = SelectorMode }
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
    Ei.button []
        { onPress = Just InsertText
        , label = E.text "Insert text"
        }
