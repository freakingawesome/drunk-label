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
import Array exposing (Array)


-- MODEL

{-| Contains the state of this component -}
type alias Model =
  { value : String
  , inProcess : String
  , sobriety : Float
  , brashness : Float
  , nextSeed : Random.Seed
  , nextWait : Time
  , minWait : Time
  , maxWait : Time
  , dir : Direction
  , showCursor : Bool
  , cursorOn : Bool
  , cursorBlinkInterval : Time
  , typoPool : Array Char
  }


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
      case (model.showCursor, model.cursorOn) of
        (True, True) -> cursorChar
        (True, False) -> nbspChar
        _ -> ""
  in
    text <| model.inProcess ++ cursor

cursorChar : String
cursorChar =
  String.fromChar <| Char.fromCode 9608

nbspChar : String
nbspChar =
  String.fromChar <| Char.fromCode 160


--------------------------------------------------------------------------------
-- Internal utility functions
--------------------------------------------------------------------------------
type TypedKey
  = Untyped Char
  | Matched Char
  | Wrong Char Char -- 1: Expected, 2: Typed
  | Excess Char

type Direction
  = Forward
  | Backward Bool

type FullZipItem a b
  = First a
  | Second b
  | Both a b

drunkTyper : Model -> (String, Direction, Random.Seed)
drunkTyper model =
  let
    typedKeys = toTypedKeys model.value model.inProcess
    numWrong =
      length
        <| filter (\x ->
          case x of
            Wrong _ _ -> True
            _ -> False
          ) typedKeys
    (drunked, nextSeed') =
      case model.dir of
        Forward ->
          appendNextLetter typedKeys model
        Backward False ->
          Maybe.withDefault [] (List.Extra.init (String.toList model.inProcess))
            |> String.fromList
            |> flip (,) model.nextSeed
        Backward True ->
          (String.slice 0 -2 model.inProcess, model.nextSeed)
    (dir, nextSeed'') =
        case model.dir of
          Forward ->
            if numWrong == 0
              then (Forward, nextSeed')
              else Random.step (Random.map (\f -> if f > model.brashness then Backward False else Forward) (Random.float 0 1)) nextSeed'
          Backward False ->
            if numWrong == 0
              then (Forward, nextSeed')
              else (Backward False, nextSeed')
          Backward True ->
            if String.length model.inProcess == 0
              then (Forward, nextSeed')
              else (Backward True, nextSeed')
  in
    (drunked, dir, nextSeed'')

appendNextLetter : List TypedKey -> Model -> (String, Random.Seed)
appendNextLetter typedKeys model =
  let
    (accuracy, nextSeed) =
      Random.step (Random.float 0 1) model.nextSeed
    (randChar, nextSeed') =
      Random.step
        (Random.map (Maybe.withDefault 'X' << flip Array.get model.typoPool) (Random.int 0 <| (Array.length model.typoPool) - 1))
        nextSeed
    filterTyped x =
      case x of
        Matched c -> Just c
        Wrong _ c -> Just c
        _ -> Nothing
    skipTyped =
      filter (\x ->
        case x of
          Untyped c -> True
          _ -> False)
    nextLetter =
      case head <| skipTyped typedKeys of
        Just (Untyped c) ->
          if accuracy > model.sobriety
            then [randChar]
            else [c]
        _ -> []
  in
    filterMap filterTyped typedKeys ++ nextLetter
      |> String.fromList
      |> flip (,) nextSeed

toTypedKeys : String -> String -> List TypedKey
toTypedKeys expected current =
  let
    typedKey entry =
      case entry of
        First c -> Untyped c
        Second c -> Excess c
        Both exp cur -> if exp == cur then Matched exp else Wrong exp cur
  in
    zipAll (String.toList expected) (String.toList current)
      |> List.map typedKey

zipAll : List a -> List b -> List (FullZipItem a b)
zipAll a b =
  case (a, b) of
    ([], []) -> []
    (x::xs, []) -> First x :: zipAll xs []
    ([], y::ys) -> Second y :: zipAll [] ys
    (x::xs, y::ys) -> Both x y :: zipAll xs ys


