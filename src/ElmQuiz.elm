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



{-
   input_quiz =
       D.decodeString quizListDecoder input_string
           |> Result.toMaybe
           |> Maybe.withDefault defaultQuiz

   decodedJsonModel =
       { jsonPath = "", quiz = input_quiz }

-}


yellow =
    Element.rgb255 255 247 25


grey =
    Element.rgb255 232 232 232


black =
    Element.rgb255 0 0 0


lightgreen =
    Element.rgb255 30 255 43


purple =
    Element.rgb255 180 22 255


blue =
    Element.rgb255 72 114 255


red =
    Element.rgb 0.8 0 0


silver =
    Element.rgb255 189 189 195


pinkishsilver =
    Element.rgb255 240 228 255


lime =
    Element.rgb255 205 220 57



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


init : () -> ( Model, Cmd Msg )
init _ =
    ( defaultModel, getJsonFile )



-- DEFAULTS


defaultQuiz =
    QuizQuestion "Question Default" [ "(A)", " (B)", " (C)", " (D)" ] "A is DefaultCorrect" "User says A" 0
        |> List.singleton


defaultModel =
    { jsonPath = "", quiz = defaultQuiz, userScore = 0.0, pageNum = 1 }



-- UPDATE


type Msg
    = ReceivedJson (Result Http.Error (List QuizQuestion)) -- if you received json from server
    | UpdateUserChoice Int String
    | LoadMore Int


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

        LoadMore pgNum ->
            ( { model | pageNum = pgNum + 1 }, Cmd.none )


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
          url = "https://gist.githubusercontent.com/TwistingTwists/409437d7fc146d05f313e59da44535e6/raw/8a7179d0fb2fed5c73938ab43cc04fd5ca0eaaa0/AugInsights.json"

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
            (toFloat (List.length model.quiz) / 10)
                |> ceiling

        ( newQuizList, a ) =
            List.splitAt (model.pageNum * 10) model.quiz
    in
    newQuizList


view : Model -> Html Msg
view model =
    Element.layout [ padding 50, spacing 100 ]
        (viewCombine model)



-- (text <| Debug.toString model.jsonPath)


viewCombine : Model -> Element Msg
viewCombine model =
    column [ spacing 50 ]
        [ model
            |> getUpdatedQuizViewList
            |> decodedQuizList
        , wrappedRow [ alignRight, spacing 20 ]
            [ viewMorePlease model.pageNum ((toFloat (List.length model.quiz) / 10) |> ceiling)
            , scoreView model
            ]
        ]


viewMorePlease : Int -> Int -> Element Msg
viewMorePlease pageNum totalPages =
    if pageNum < totalPages then
        Input.button
            [ Background.color lightgreen
            , Font.color black
            , padding 10
            , Border.rounded 10
            , Element.focused
                [ Background.color blue, Font.color yellow ]
            ]
            { onPress = Just <| LoadMore pageNum
            , label = text ("More Please LeftPages = " ++ String.fromInt (totalPages - pageNum))
            }

    else
        Element.el [ Background.color red, Font.color lime ] (text <| "You've reached the end of quiz")


scoreView : Model -> Element Msg
scoreView model =
    Element.el [ Element.below (text <| String.slice 0 6 <| String.fromFloat <| model.userScore), alignTop, alignRight, Font.bold, Font.color red ]
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
        [ --  Element.explain Debug.todo,
          -- text <| Debug.toString modelQuizList
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


setChoiceDefaultIfError : List String -> List String
setChoiceDefaultIfError choices =
    if List.length choices < 4 then
        [ "A", "B", "C", "D" ]

    else
        choices


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
                List.map2 (\op ch -> Input.optionWith op (niceViewop { choice = ch, userResponse = q.userResponse, correct = q.correct })) [ "A", "B", "C", "D" ] (setChoiceDefaultIfError <| q.choices)

            -- List.map2 (\op ch -> Input.option op (text ch)) [ A, B, C, D ] choices
            }
        ]


niceViewop : { choice : String, userResponse : String, correct : String } -> Input.OptionState -> Element Msg
niceViewop { choice, userResponse, correct } op =
    let
        attrs =
            [ width fill, padding 5, Border.rounded 10, Border.solid, Border.color black ]
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
