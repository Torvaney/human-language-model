module Main exposing (..)

import Browser
import Browser.Events

import Element as El
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input

import File
import File.Select as Select
import Json.Decode as Decode
import Random
import Random.List
import String.Extra
import Task


main =
    Browser.element
        { init   = init
        , update = update
        , view   = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd msg )
init _ =
    ( { current   = Nothing
      , previous  = Nothing
      , upcoming  = []
      , results   = []
      , windowLen = 10
      , content   = ""
      }
    , Cmd.none
    )


-- MODEL

type alias Model =
    { current   : Maybe Observation
    , previous  : Maybe Observation
    , upcoming  : List Observation
    , results   : List Bool
    , windowLen : Int
    , content   : String
    }


type alias Observation =
    { window     : String
    , nextLetter : Char
    }



-- UPDATE

type Msg
    = Enter Char
    | InvalidInput
    | TextRequested
    | TextSelected File.File
    | TextLoaded String
    | Shuffle (List Observation)


-- TODO: name this fn better
loadFromUpcoming model =
    { model
    | current  = List.head model.upcoming
    , upcoming = List.tail model.upcoming |> Maybe.withDefault []
    }


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  case msg of
    Enter c ->
        case model.current of
            Just obs ->
                ( { model
                  | results  = (c == obs.nextLetter) :: model.results
                  , previous = model.current
                  } |> loadFromUpcoming
                , Cmd.none
                )
            Nothing ->
                ( model, Cmd.none )

    InvalidInput ->
        ( model, Cmd.none )

    TextRequested ->
        ( model
        , Select.file ["text/plain"] TextSelected
        )

    TextSelected file ->
        ( model
        , Task.perform TextLoaded (File.toString file)
        )

    TextLoaded content ->
        let
            upcoming = parseUploadedContent model.windowLen content
        in
            ( { model
              | results  = []
              , previous = Nothing
              , content  = content
              }
            , Random.generate Shuffle (Random.List.shuffle upcoming)
            )

    Shuffle shuffled ->
        ( { model
          | upcoming = shuffled
          } |> loadFromUpcoming
        , Cmd.none
        )


stringToObservation s =
    String.reverse s |>
        String.uncons |>
        Maybe.map (\(n, w) -> Observation (String.reverse w) n)


-- Parse an uploaded file into a bunch of "observations"
-- Treat empty lines as content separators, and linebreaks as spaces
parseUploadedContent : Int -> String -> List Observation
parseUploadedContent windowLen content =
    content |>
        String.split "\n\n" |>
        List.concatMap (parseCorpus windowLen)


parseCorpus : Int -> String -> List Observation
parseCorpus windowLen content =
    let
        observationLen = windowLen + 1
    in
        content |>
            String.replace "\n" " " |>
            String.Extra.break observationLen |>
            List.filter (\s -> (String.length s) == observationLen) |>
            List.filterMap stringToObservation


-- VIEW

defaultPadding =
    { top    = 10
    , right  = 10
    , bottom = 10
    , left   = 10
    }


view model =
    El.layout [ ] <|
        El.column
            [ El.centerX, El.centerY ]
            [ El.el
                [ El.centerX
                , El.paddingEach defaultPadding
                , Font.size 24
                , Font.bold
                ]
                (El.text "Test your language model")
            , El.el
                [ El.centerX
                , El.paddingEach defaultPadding
                ]
                (El.text "Which character comes next?")
            , El.el
                [ El.centerX
                , El.paddingEach { defaultPadding | top = 20 }
                , Font.size 18
                , Font.family
                    [ Font.monospace
                    ]
                ]
                (El.text <| showCurrent model.current)
            , El.el
                [ El.centerX
                , El.paddingEach { defaultPadding | bottom = 20 }
                , Font.color (El.rgb255 140 140 140)
                , Font.size 18
                , Font.family
                    [ Font.monospace
                    ]
                ]
                (El.text <| showPrevious model.previous)
            , El.el
                [ El.centerX
                , El.paddingEach defaultPadding
                ]
                (El.text <| (++) "Accuracy: " <| showAccuracy <| model.results)
            , El.el
                [ El.centerX
                , El.paddingEach { defaultPadding | top = 20 }
                ]
                ( Input.button
                    [ Background.color (El.rgb255 240 240 240)
                    , Border.color (El.rgb255 220 220 220)
                    , Border.rounded 20
                    , Border.width 2
                    , El.padding 10
                    ]
                    { onPress = Just TextRequested
                    , label   = El.text "Upload"
                    }
                )
            , El.el
                [ El.centerX
                , El.paddingEach { defaultPadding | top = 20, bottom = 20 }
                , Font.color (El.rgb255 220 220 220)
                ]
                (El.text "---------")
            , El.newTabLink
                [ El.centerX
                , Border.color (El.rgb255 220 220 220)
                , Border.rounded 20
                , Border.width 2
                , El.padding 10
                ]
                { url = "https://github.com/Torvaney/human-language-model"
                , label = El.text "view source"
                }
            ]


showWindow obs =
    "\"..." ++ obs.window ++ "\""


showObservation obs =
    String.join ""
        [ (showWindow obs)
        , " + \""
        , (String.fromChar obs.nextLetter)
        , "\""
        ]


showCurrent obs =
    Maybe.map showWindow obs |>
        Maybe.withDefault "(No examples left! Upload a .txt file to try more!)"


showPrevious obs =
    Maybe.map showObservation obs |>
        Maybe.withDefault "(No previous attempts to show)"


boolToInt : Bool -> Int
boolToInt b =
    case b of
        True -> 1
        False -> 0


divideWithZero : Float -> Float -> Maybe Float
divideWithZero a b =
    if b == 0 then
        Nothing
    else
        Just (a / b)


countResults : List Bool -> (Int, Int)
countResults results =
    let
        successes = List.map boolToInt results |> List.sum
        trials    = List.length results
    in
        (successes, trials)


roundFloat : Int -> Float -> Float
roundFloat n f =
    let
        scaleBy = toFloat (10 ^ n)
    in
        f * scaleBy |>
            round |>
            toFloat |>
            (\x -> x / scaleBy)


showPercent : Maybe Float -> String
showPercent division =
    case division of
        Just p ->
            p * 100 |>
                round |>
                String.fromInt |>
                (\x -> x ++ "%")
        Nothing ->
            "(No results yet)"


showAccuracy : List Bool -> String
showAccuracy results =
    let
        (successes, trials) = countResults results
        acc                 = divideWithZero (toFloat successes) (toFloat trials)
    in
        String.join ""
            [ (showPercent acc)
            , " ("
            , String.fromInt successes
            , " out of "
            , String.fromInt trials
            , ")"
            ]



-- SUBSCRIPTIONS


keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map toMsg (Decode.field "key" Decode.string)


toMsg : String -> Msg
toMsg string =
  case String.uncons string of
    Just (char, "") ->
      Enter char

    _ ->
      InvalidInput


subscriptions model =
   Browser.Events.onKeyPress keyDecoder
