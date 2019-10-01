port module ElmQuiz exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Element exposing (Element, alignBottom, alignRight, alignTop, centerY, column, el, fill, padding, rgb255, row, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (OptionState(..))
import Html exposing (Html)
import Html.Events as HE
import Http
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List.Extra as List



-- DEFAULTS


defaultQuiz =
    QuizQuestion "Question Default" [ "(A)", " (B)", " (C)", " (D)" ] "A is DefaultCorrect" "User says A" 0
        |> List.singleton


defaultModel =
    { jsonPath = "", quiz = defaultQuiz, userScore = 0.0 }



{-
   input_quiz =
       D.decodeString quizListDecoder input_string
           |> Result.toMaybe
           |> Maybe.withDefault defaultQuiz

   decodedJsonModel =
       { jsonPath = "", quiz = input_quiz }

-}


grey =
    Element.rgb255 232 232 232


red =
    Element.rgb 0.8 0 0


silver =
    Element.rgb255 189 189 195


pinkishsilver =
    Element.rgb255 240 228 255



-- MAIN


main =
    Browser.document
        { init = init
        , view = \model -> { title = "Elm • Quiz", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = subscriptions
        }


port setStorage : Model -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage newModel, cmds ]
    )



-- MODEL


type Choice
    = A
    | B
    | C
    | D


type alias Model =
    { jsonPath : String
    , quiz : List QuizQuestion
    , userScore : Float
    }


type alias QuizQuestion =
    { question : String
    , choices : List String
    , correct : String

    -- to handle user response and Unique question_id (qid)
    , userResponse : String
    , qid : Int
    }


init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    ( Maybe.withDefault defaultModel maybeModel, getJsonFile )



-- UPDATE


type Msg
    = ReceivedJson (Result Http.Error (List QuizQuestion)) -- if you received json from server
    | UpdateUserChoice Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- JsonPath newPath ->
        --     ( { model | jsonPath = newPath }, getJsonFile )
        ReceivedJson result ->
            case result of
                Ok quizQuestionList ->
                    ( { model | quiz = quizQuestionList }, Cmd.none )

                Err _ ->
                    ( { model | jsonPath = "Failed to load given json path." }, Cmd.none )

        UpdateUserChoice id ans ->
            ( model |> updUserChoice id ans |> updUserScore
            , Cmd.none
            )


updUserScore : Model -> Model
updUserScore model =
    { model | userScore = countScore model.quiz }


updUserChoice : Int -> String -> Model -> Model
updUserChoice id ans model =
    let
        updateUserAns quizQuestion =
            if quizQuestion.qid == id then
                { quizQuestion | userResponse = ans }

            else
                quizQuestion
    in
    { model | quiz = List.map updateUserAns model.quiz }


getJsonFile : Cmd Msg
getJsonFile =
    Http.get
        { --   url = "./AugInsights.json"
          --  url = "https://gist.githubusercontent.com/TwistingTwists/409437d7fc146d05f313e59da44535e6/raw/8a7179d0fb2fed5c73938ab43cc04fd5ca0eaaa0/AugInsights.json"
          url = "http://192.168.0.104:8080/src/AugInsights.json"

        -- , headers = [ Http.header "Access-Control-Allow-Origin: nickname.github.io" ]
        --   url = "http://192.168.0.105:8080/src/quizzy.json"
        , expect = Http.expectJson ReceivedJson quizListDecoder
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- DECODER


quizListDecoder : Decoder (List QuizQuestion)
quizListDecoder =
    D.list quizDecoder


quizDecoder : D.Decoder QuizQuestion
quizDecoder =
    D.succeed QuizQuestion
        |> required "question" qaDecoder
        |> required "choices" choicesDecoder
        |> required "correct" qaDecoder
        |> optional "userResponse" D.string "Z"
        |> optional "id" D.int 0


qaDecoder : D.Decoder String
qaDecoder =
    D.string


choicesDecoder : D.Decoder (List String)
choicesDecoder =
    D.list D.string



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [ padding 50, spacing 100 ]
        (viewCombine model)



-- (text <| Debug.toString model.jsonPath)


viewCombine : Model -> Element Msg
viewCombine model =
    column [ spacing 50 ]
        [ decodedQuizList model.quiz ]



-- , scoreView model ]


scoreView : Model -> Element Msg
scoreView model =
    Element.el [ Element.above (text (String.fromFloat model.userScore)), alignTop, alignRight, Font.bold, Font.color red ]
        (text <| "Score is ")



-- (text <| String.fromFloat <| countScore model.quiz)


countScore : List QuizQuestion -> Float
countScore quiz =
    countScoreHelper quiz 0.0


countScoreHelper : List QuizQuestion -> Float -> Float
countScoreHelper quiz scoreSoFar =
    case quiz of
        [] ->
            scoreSoFar

        head :: tail ->
            let
                val =
                    if String.toUpper head.userResponse == String.toUpper head.correct then
                        scoreSoFar + 2.0

                    else
                        scoreSoFar - (2 / 3)
            in
            countScoreHelper tail val


decodedQuizList : List QuizQuestion -> Element Msg
decodedQuizList modelQuizList =
    wrappedRow [ width fill, Element.centerX ]
        [ -- [ text <| Debug.toString modelQuizList
          -- (viewQuizList input_quiz)
          -- (viewQuizList modelQuizList)
          column
            [ Font.color (Element.rgba255 12 6 14 1)
            , padding 90
            , spacing 30
            ]
            -- (List.map radioQuiz modelQuizList)
            -- (List.map2 questionsView radioQuiz modelQuizList)
            (List.map (\x -> column [ Element.spaceEvenly ] [ questionsView x, radioQuiz x ]) modelQuizList)
        ]


questionsView : QuizQuestion -> Element Msg
questionsView q =
    -- wrappedRow [ Background.color grey, width fill, centerY, padding 10, Border.solid]
    --     [ text <| q.question ]
    Element.textColumn [ padding 10 ]
        [ Element.el
            [ Element.spaceEvenly
            , Font.justify
            , Font.size 38
            , padding 20
            , width fill
            ]
            (text <| q.question)

        -- , el [ Element.alignLeft ] Element.none
        ]


radioQuiz : QuizQuestion -> Element Msg
radioQuiz q =
    wrappedRow [ Background.color grey, width fill, centerY ]
        [ Input.radio
            [ padding 40, spacing 50, Font.size 30 ]
            { onChange = UpdateUserChoice q.qid
            , selected = Just q.userResponse
            , label = Input.labelHidden ("Question" ++ String.fromInt q.qid)
            , options =
                -- List.map2 (\op ch -> Input.option op (text ch)) [ "A", "B", "C", "D" ] q.choices
                List.map2 (\op ch -> Input.optionWith op (niceViewop ch)) [ "A", "B", "C", "D" ] q.choices

            -- List.map2 (\op ch -> Input.option op (text ch)) [ A, B, C, D ] choices
            }
        ]


niceViewop : String -> Input.OptionState -> Element Msg
niceViewop choice op =
    let
        attrs =
            [ width fill, padding 5, Border.rounded 10 ]
    in
    case op of
        Idle ->
            el (List.append [ Background.color (Element.rgb255 236 242 240) ] attrs) (text choice)

        Focused ->
            el (List.append [ Background.color (Element.rgb255 52 152 219) ] attrs) (text choice)

        Selected ->
            el (List.append [ Background.color (Element.rgb255 241 196 15) ] attrs) (text choice)



{-


   viewQuizList : List QuizQuestion -> Element Msg
   viewQuizList quizList =
       case quizList of
           Just listofquiz ->
               -- viewInputQuiz model (List.head listofquiz)
               column [] (List.map radioQuiz listofquiz)

           Nothing ->
               text <| "Could not parse quizlist succesfullly"


      viewChoices : QuizQuestion -> Element Msg
      viewChoices ({ quiz } as model) =
          Input.radio [ padding 10, spacing 5, Font.size 16 ]
              { onChange = UpdateUserChoice quiz.qid
              , selected = Just model.quiz.userResponse
              , label = Input.labelAbove [] (text model.quiz.question)
              , options =
                  List.map2 (\op ch -> Input.option op (text ch)) [ "A", "B", "C", "D" ] quiz.choices

              -- List.map2 (\op ch -> Input.option op (text ch)) [ A, B, C, D ] choices
              }

-}
--- ANOTHER GARBAGE SECTION
-- Input.radio
-- [ spacing 12
-- , Background.color grey
-- ]
-- { selected = Just model.lunch
-- , onChange = \new -> Update { model | lunch = new }
-- , label = Input.labelAbove [ Font.size 14, paddingXY 0 12 ] (text "What would you like for lunch?")
-- , options =
--     [ Input.option Gyro (text "Gyro")
--     , Input.option Burrito (text "Burrito")
--     , Input.option Taco (text "Taco")
--     ]
-- }
-- ( model |> updateUsrResponse id ans, Cmd.none )
-- HELPER FUNCTIONS to update model
-- https://gist.github.com/s-m-i-t-a/2a83c0bc5b7d7081b019d18520ebc62c
{-
   setUserResponse : String -> QuizQuestion -> QuizQuestion
   setUserResponse ans quiz =
       { quiz | userResponse = ans }


   setQuiz : (QuizQuestion -> QuizQuestion) -> Model -> Model
   setQuiz fnQuiz model =
       { model | quiz = fnQuiz model.quiz }


   updateUsrResponse : Int -> String -> Model -> Model
   updateUsrResponse id ans =
       setUserResponse id ans
           |> setQuiz

-}
-- viewInputQuiz : Model -> List QuizQuestion -> Element Msg
-- viewInputQuiz model listofquiz =
--     viewSingleQuestion model (List.head listofquiz)
-- viewSingleQuestion : Model -> QuizQuestion -> Element Msg
-- viewSingleQuestion model inp =
--     radioQuiz model inp
-- case inp of
--     Just a ->
--         radioQuiz model a
--     Nothing ->
--         text <| "Question could not be parsed!"
