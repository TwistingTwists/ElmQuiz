port module ElmQuiz exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Colors exposing (..)
import Element exposing (Element, alignBottom, alignRight, alignTop, centerX, centerY, column, el, fill, height, padding, rgb255, row, spacing, text, width, wrappedRow)
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
import QuizMonth as QM



{-
   input_quiz =
       D.decodeString quizListDecoder input_string
           |> Result.toMaybe
           |> Maybe.withDefault defaultQuiz

   decodedJsonModel =
       { jsonPath = "", quiz = input_quiz }

-}
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
    , pageNum : Int
    }


type alias QuizQuestion =
    { question : String
    , choices : List String
    , correct : String

    -- to handle user response and Unique question_id (qid)
    , userResponse : String
    , qid : Int
    }



-- DEFAULTS


defaultQuiz =
    QuizQuestion "Question Default" [ "(A)", " (B)", " (C)", " (D)" ] "A is DefaultCorrect" "User says A" 0
        |> List.singleton


defaultModel =
    { jsonPath = "", quiz = defaultQuiz, userScore = 0.0, pageNum = 1 }


init : () -> ( Model, Cmd Msg )
init _ =
    -- ( defaultModel, getJsonFile )
    ( defaultModel, Cmd.none )



-- UPDATE


type Msg
    = ReceivedJson (Result Http.Error (List QuizQuestion)) -- if you received json from server
    | UpdateUserChoice Int String
    | LoadMore Int
    | ChosenQuizMonth String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        LoadMore pgNum ->
            ( { model | pageNum = pgNum + 1 }, Cmd.none )

        ChosenQuizMonth mahina ->
            ( model, getJsonFromUrl (mahina |> QM.month2url) )


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


getJsonFromUrl : String -> Cmd Msg
getJsonFromUrl urlReceived =
    Http.get
        { url = urlReceived
        , expect = Http.expectJson ReceivedJson quizListDecoder
        }


getJsonFile : Cmd Msg
getJsonFile =
    Http.get
        { --   url = "./AugInsights.json"
          -- url = "https://gist.githubusercontent.com/TwistingTwists/409437d7fc146d05f313e59da44535e6/raw/8a7179d0fb2fed5c73938ab43cc04fd5ca0eaaa0/AugInsights.json"
          --   url = "https://gist.githubusercontent.com/TwistingTwists/d1b0a7ec408e1ccfa511f1d11d4458d9/raw/c87c46a68a18a2c50fe9932bfc703cdcbc43d190/JulyInsights.json"
          url = "https://gist.githubusercontent.com/TwistingTwists/d1b0a7ec408e1ccfa511f1d11d4458d9/raw/ce0b11428cbd308c9d6789b46213a34b6f4aeee5/JuneInsights.json"

        --   url = "http://192.168.0.104:8080/src/AugInsights.json"
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


getUpdatedQuizViewList : Model -> List QuizQuestion
getUpdatedQuizViewList model =
    let
        totalPages =
            (toFloat (List.length model.quiz) / 5)
                |> ceiling

        ( newQuizList, a ) =
            List.splitAt (model.pageNum * 5) model.quiz
    in
    newQuizList


view : Model -> Html Msg
view model =
    Element.layout [ padding 10, spacing 30 ]
        (viewCombine model)



-- (text <| Debug.toString model.jsonPath)


viewCombine : Model -> Element Msg
viewCombine model =
    column []
        [ viewHeader
        , viewQuiz model
        ]


viewQuiz : Model -> Element Msg
viewQuiz model =
    Element.textColumn
        [ padding 20, width fill ]
        [ model
            |> getUpdatedQuizViewList
            |> decodedQuizList
        , Element.textColumn [ alignRight, alignBottom ]
            [ viewMorePlease model.pageNum ((toFloat (List.length model.quiz) / 5) |> ceiling)
            , scoreView model
            ]
        ]


viewHeader : Element Msg
viewHeader =
    Element.row [ Element.spaceEvenly, spacing 50, centerX, centerY ]
        [ viewMonthButton "June"
        , viewMonthButton "July"
        , viewMonthButton "August"
        ]


viewMonthButton : String -> Element Msg
viewMonthButton s =
    -- TODO : can implement QUiz button as Radio : ie. choose one of the quiz options ONLY, but not needed since model.quiz can have only one quiz at a time.
    -- wrappedRow [ Background.color pinkishsilver ]
    --     [ Input.radio
    --         [ padding 30, spacing 90, Font.size 30, width fill ]
    --         -- (fill |> Element.maximum 800) ]
    --         { onChange = ChosenQuizMonth s
    --         , selected = Just q.userResponse
    --         , label = Input.labelHidden ("Question" ++ String.fromInt q.qid)
    --         , options =
    --             -- List.map2 (\op ch -> Input.option op (text ch)) [ "A", "B", "C", "D" ] q.choices
    --             List.map2 (\op ch -> Input.optionWith op (niceViewop { choice = ch, userResponse = q.userResponse, correct = q.correct })) [ "A", "B", "C", "D" ] (setChoiceDefaultIfError <| q.choices)
    --         -- List.map2 (\op ch -> Input.option op (text ch)) [ A, B, C, D ] choices
    --         }
    --     ]
    Input.button
        [ Background.color yellow
        , Font.color black
        , Font.size 30
        , padding 20
        , spacing 10
        , Border.rounded 20
        , Element.focused
            [ Background.color pinkishsilver, Font.color blue ]
        ]
        { onPress = Just <| ChosenQuizMonth s
        , label = text ("Quiz from Month " ++ s)
        }


decodedQuizList : List QuizQuestion -> Element Msg
decodedQuizList modelQuizList =
    Element.textColumn [ width fill, Element.centerX ]
        [ Element.textColumn
            [ padding 90
            , spacing 50
            ]
            (List.map (\x -> Element.wrappedRow [ Element.spaceEvenly, padding 5 ] [ questionsView x, radioQuiz x ]) modelQuizList)
        ]


questionsView : QuizQuestion -> Element Msg
questionsView q =
    Element.textColumn
        -- [ Font.justify
        [ width fill
        , Font.size 38
        , padding 5
        , Element.onLeft (Element.el [ Font.color blue, Font.size 50, Font.bold ] (text <| "(" ++ String.fromInt q.qid ++ ")"))
        ]
        [ text <| q.question ]


setChoiceDefaultIfError : List String -> List String
setChoiceDefaultIfError choices =
    if List.length choices < 4 then
        [ "A", "B", "C", "D" ]

    else
        choices


radioQuiz : QuizQuestion -> Element Msg
radioQuiz q =
    wrappedRow [ Background.color grey, height fill ]
        [ Input.radio
            [ padding 10, spacing 90, Font.size 30, width fill ]
            { onChange = UpdateUserChoice q.qid
            , selected = Just q.userResponse
            , label = Input.labelHidden ("Question" ++ String.fromInt q.qid)
            , options =
                -- List.map2 (\op ch -> Input.option op (text ch)) [ "A", "B", "C", "D" ] q.choices
                List.map2 (\op ch -> Input.optionWith op (niceViewop { choice = ch, userResponse = q.userResponse, correct = q.correct })) [ "A", "B", "C", "D" ] (setChoiceDefaultIfError <| q.choices)
            }
        ]


niceViewop : { choice : String, userResponse : String, correct : String } -> Input.OptionState -> Element Msg
niceViewop { choice, userResponse, correct } op =
    let
        attrs =
            [ width (fill |> Element.maximum 1000 |> Element.minimum 100), padding 10 ]
    in
    case op of
        Idle ->
            el (List.append [ Background.color (Element.rgb255 236 242 240) ] attrs) (text choice)

        Focused ->
            el (List.append [ Background.color (Element.rgb255 52 152 219) ] attrs) (text choice)

        Selected ->
            if String.toUpper userResponse == String.toUpper correct then
                el (List.append [ Background.color lightgreen ] attrs) (text choice)

            else
                el (List.append [ Background.color (Element.rgb255 241 196 15) ] attrs) (text choice)


viewMorePlease : Int -> Int -> Element Msg
viewMorePlease pageNum totalPages =
    if pageNum < totalPages then
        Input.button
            [ Background.color yellow
            , Font.color black
            , padding 20
            , spacing 10
            , Border.rounded 20
            , Element.focused
                [ Background.color lightgreen, Font.color blue ]
            ]
            { onPress = Just <| LoadMore pageNum
            , label = text ("More Please LeftPages = " ++ String.fromInt (totalPages - pageNum))
            }

    else
        Element.el [ Background.color black, Font.color yellow, Font.size 50 ] (text <| "You've reached the end of quiz")


scoreView : Model -> Element Msg
scoreView model =
    Element.el [ Element.below (text <| String.slice 0 6 <| String.fromFloat <| model.userScore), alignTop, alignRight, Font.bold, Font.size 30, Font.color red ]
        (text <| "Score is ")



-- SCORING SCHEME


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
                    getMeScore ( head.userResponse, head.correct ) scoreSoFar
            in
            countScoreHelper tail val


getMeScore : ( String, String ) -> Float -> Float
getMeScore ( a, correct ) score =
    if String.toUpper a == "Z" then
        score

    else
        getMeScoreHelper ( a, correct ) score


getMeScoreHelper : ( String, String ) -> Float -> Float
getMeScoreHelper ( a, correct ) scoreSoFar =
    if String.toUpper a == String.toUpper correct then
        scoreSoFar + 2.0

    else
        scoreSoFar - (2 / 3)
