module DrunkLabel exposing (
  Model,
  Flags,
  defaultFlags,
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
  subscriptions,
  main)

{-| Mistyping as a service

# Model
@docs Flags, defaultFlags, defaultTypoPool, Model

# Customization
@docs Msg

# Wiring
@docs init, update, view, subscriptions

# Using directly from javascript
@docs main
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

{-| You can use DrunkLabel directly from javascript with the exposed main function.

    var Elm = require("dist/elm/drunk-label.js");
    var node = document.getElementById("drunk-target");

    var flags = {
      value: "Hello, Elm!",
      sobriety: 1,
      brashness: 0,
      minWait: 30,
      maxWait: 200,
      showCursor: true,
      cursorBlinkInterval: 500,
      typoPool: "", // defaults to ascii 48-122
      initialSeed: Date.now()
    };

    var app = Elm.DrunkLabel.embed(node, flags);

The use of main is unnecessary when embedded in an Elm app.
-}
main : Program Flags
main =
  App.programWithFlags
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


-- MODEL

{-| Contains the internal state of this component. Use Flags and Msg to alter state -}
type Model = Model InternalModel

type alias InternalModel =
  { value : String
  , sobriety : Float
  , brashness : Float
  , minWait : Time
  , maxWait : Time
  , showCursor : Bool
  , cursorBlinkInterval : Time
  , typoPool : Array Char
  , inProcess : String
  , nextSeed : Random.Seed
  , nextWait : Time
  , dir : Direction
  , cursorOn : Bool
  }

{-| Use Flags to pass in the initial state -}
type alias Flags =
  { value : String
  , sobriety : Float
  , brashness : Float
  , minWait : Time
  , maxWait : Time
  , showCursor : Bool
  , cursorBlinkInterval : Time
  , typoPool : String
  , initialSeed : Int
  }

{-| Sensible teetotaling defaults -}
defaultFlags : Flags
defaultFlags =
  { value = ""
  , sobriety = 1
  , brashness = 0
  , minWait = 30 * millisecond
  , maxWait = 200 * millisecond
  , showCursor = True
  , cursorBlinkInterval = 500 * millisecond
  , typoPool = String.fromList <| Array.toList defaultTypoPool
  , initialSeed = 0
  }

{-| Letters, numbers, and basic symbols -}
defaultTypoPool : Array Char
defaultTypoPool =
  Array.fromList <| List.map Char.fromCode [48..122]

{-| Wire `init` into the parent components initialization function -}
init : Flags -> (Model, Cmd Msg)
init flags = Model
  { value = flags.value
  , sobriety = clamp 0 1 flags.sobriety
  , brashness = clamp 0 1 flags.brashness
  , minWait = max 0 flags.minWait
  , maxWait = max flags.minWait <| max 0 flags.maxWait
  , showCursor = flags.showCursor
  , cursorBlinkInterval = max 0 flags.cursorBlinkInterval
  , typoPool =
    case flags.typoPool of
      "" -> defaultTypoPool
      _ -> Array.fromList <| String.toList flags.typoPool
  , nextSeed = Random.initialSeed flags.initialSeed
  , inProcess = ""
  , nextWait = 50 * millisecond
  , dir = Forward
  , cursorOn = False
  } ! []


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
update msg model' =
  case model' of
    Model model ->
      case msg of
        SetValue val ->
          Model { model | value = val, dir = Backward True } ! []
        SetSobriety val ->
          Model { model | sobriety = val, dir = Backward True } ! []
        SetBrashness val ->
          Model { model | brashness = val, dir = Backward True } ! []
        SetMinWait min ->
          Model { model | minWait = min, maxWait = max min model.maxWait, dir = Backward True } ! []
        SetMaxWait max ->
          Model { model | minWait = min model.minWait max, maxWait = max, dir = Backward True } ! []
        ToggleCursor ->
          Model { model | cursorOn = model.showCursor && not model.cursorOn  } ! []
        ShowCursor show ->
          Model { model | showCursor = show } ! []
        SetCursorBlinkInterval val ->
          Model { model | cursorBlinkInterval = val } ! []
        SetTypoPool pool ->
          let pool' = if Array.isEmpty pool then defaultTypoPool else pool
          in Model { model | typoPool = pool', dir = Backward True } ! []
        NextKey ->
          let
            (chance, nextSeed) = Random.step (chanceGenerator model) model.nextSeed
            (nextText, dir) = drunkTyper model chance
          in
            Model
              { model
              | inProcess = nextText
              , nextSeed = nextSeed
              , nextWait = chance.nextWait
              , dir = dir
              } ! []


-- SUBSCRIPTIONS

{-| Wiring for the `subscriptions` function -}
subscriptions : Model -> Sub Msg
subscriptions model' =
  case model' of
    Model model ->
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
view model' =
  case model' of
    Model model ->
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

type alias Chance =
  { nextWait : Time
  , accuracy : Float
  , typo : Char
  , dirIfTypos : Direction
  }

chanceGenerator : InternalModel -> Random.Generator Chance
chanceGenerator model =
  Random.map4 Chance
    (Random.float model.minWait model.maxWait)
    (Random.float 0 1)
    (Random.map (Maybe.withDefault 'X' << flip Array.get model.typoPool) (Random.int 0 <| (Array.length model.typoPool) - 1))
    (Random.map (\f -> if f > model.brashness then Backward False else Forward) (Random.float 0 1))

drunkTyper : InternalModel -> Chance -> (String, Direction)
drunkTyper model chance =
  let
    typedKeys = toTypedKeys model.value model.inProcess
    wrongs x =
      case x of
        Wrong _ _ -> True
        _ -> False
    numWrong = length <| filter wrongs typedKeys
    drunked =
      case model.dir of
        Forward ->
          appendNextLetter typedKeys model chance
        Backward False ->
          Maybe.withDefault [] (List.Extra.init (String.toList model.inProcess))
            |> String.fromList
        Backward True ->
          String.slice 0 -2 model.inProcess
    dir =
        case model.dir of
          Forward ->
            if numWrong == 0
              then Forward
              else chance.dirIfTypos
          Backward False ->
            if numWrong == 0
              then Forward
              else Backward False
          Backward True ->
            if String.length model.inProcess == 0
              then Forward
              else Backward True
  in
    (drunked, dir)

appendNextLetter : List TypedKey -> InternalModel -> Chance -> String
appendNextLetter typedKeys model chance =
  let
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
          if chance.accuracy > model.sobriety
            then [chance.typo]
            else [c]
        _ -> []
  in
    filterMap filterTyped typedKeys ++ nextLetter
      |> String.fromList

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


