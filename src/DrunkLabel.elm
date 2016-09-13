module DrunkLabel exposing (
  Model,
  defaultModel,
  defaultTypoPool,
  Msg
    ( SetValue
    , SetSobriety
    , SetBrashness
    , SetMinWait
    , SetMaxWait
    , ShowCursor
    , SetCursorBlinkInterval
    , SetTypoPool
    ),
  init,
  update,
  view,
  subscriptions)

{-| Mistyping as a service

# Model
@docs Model, defaultModel, defaultTypoPool

# Customization
@docs Msg

# Wiring
@docs init, update, view, subscriptions
-}

import Html exposing (..)
import Html.App as App
import Time exposing (Time, millisecond)
import Char
import String
import Random
import List exposing (..)
import List.Extra exposing (..)
import DrunkTyper exposing (..)
import Array exposing (Array)


-- MODEL

{-| Contains the state of this component -}
type alias Model = DrunkTyper.Model


{-| Sensible teetotaling defaults -}
defaultModel : Model
defaultModel =
  { value = ""
  , inProcess = ""
  , sobriety = 1
  , brashness = 0
  , nextSeed = Random.initialSeed 0
  , nextWait = 50 * millisecond
  , minWait = 30 * millisecond
  , maxWait = 200 * millisecond
  , dir = Forward
  , showCursor = True
  , cursorOn = False
  , cursorBlinkInterval = 500 * millisecond
  , typoPool = defaultTypoPool
  }

{-| Numbers and basic symbols -}
defaultTypoPool : Array Char
defaultTypoPool =
  Array.fromList <| List.map Char.fromCode [48..122]

{-| Wire `init` into the parent components initialization function. Takes a random seed -}
init : Random.Seed -> (Model, Cmd Msg)
init seed =
  { defaultModel | nextSeed = seed } ! []


-- UPDATE

{-| You can modify the settings and sobriety on the fly by passing these messages in from the parent component.

Most of these messages will cause the typist to backspace all the way to the beginning to start over.

* `SetValue` changes the target value.
* `SetSobriety` changes the accuracy percentage. It expects a value between 0 and 1.
* `SetBrashness` changes the confidence level of the typist. The higher the value, the less likely the
typist is to realize they made a mistake. It expects a value between 0 and 1.
* `SetMinWait` changes the mininum delay between each key press.
* `SetMaxWait` changes the maximum delay between each key press.
* `ShowCursor` changes whether the cursor is visible.
* `SetCursorBlinkInterval` changes how fast the cursor blinks
* `SetTypoPool` changes the pool of characters from which typos are pulled
-}
type Msg
  = SetValue String
  | SetSobriety Float
  | SetBrashness Float
  | SetMinWait Time
  | SetMaxWait Time
  | ShowCursor Bool
  | SetCursorBlinkInterval Time
  | SetTypoPool (Array Char)
  | ToggleCursor
  | NextKey

{-| Wiring for the `update` function -}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetValue val ->
      { model | value = val, dir = Backward True } ! []
    SetSobriety val ->
      { model | sobriety = val, dir = Backward True } ! []
    SetBrashness val ->
      { model | brashness = val, dir = Backward True } ! []
    SetMinWait min ->
      { model | minWait = min, maxWait = max min model.maxWait, dir = Backward True } ! []
    SetMaxWait max ->
      { model | minWait = min model.minWait max, maxWait = max, dir = Backward True } ! []
    ToggleCursor ->
      { model | cursorOn = model.showCursor && not model.cursorOn } ! []
    ShowCursor show ->
      { model | showCursor = show } ! []
    SetCursorBlinkInterval val ->
      { model | cursorBlinkInterval = val } ! []
    SetTypoPool pool ->
      let pool' = if Array.isEmpty pool then defaultTypoPool else pool
      in { model | typoPool = pool', dir = Backward True } ! []
    NextKey ->
      let
        (nextText, dir, nextSeed) = drunkTyper model
        (nextWait, nextSeed') = Random.step (Random.float model.minWait model.maxWait) nextSeed
      in
        { model
          | inProcess = nextText
          , nextSeed = nextSeed'
          , nextWait = nextWait
          , dir = dir
        } ! []


-- SUBSCRIPTIONS

{-| Wiring for the `subscriptions` function -}
subscriptions : Model -> Sub Msg
subscriptions model =
  let
    typing =
      case model.dir of
        Backward True -> Time.every (min model.maxWait <| 50 * millisecond) (always NextKey)
        _ ->
          if model.value == model.inProcess
            then Sub.none
            else Time.every model.nextWait (always NextKey)
    cursorBlinking =
      if model.showCursor
        then Time.every model.cursorBlinkInterval (always ToggleCursor)
        else Sub.none
  in
    Sub.batch [ typing, cursorBlinking ]


-- VIEW

{-| Wiring for the `view` function -}
view : Model -> Html Msg
view model =
  let
    cursor =
      if model.showCursor && model.cursorOn
        then cursorCharacter
        else ""
  in
    text <| model.inProcess ++ cursor

cursorCharacter : String
cursorCharacter =
  String.fromList [ Char.fromCode 9608 ]

