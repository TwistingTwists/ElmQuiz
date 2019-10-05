module QuizMonth exposing (Month, QuizMahina, month2url)


type Month
    = June String
    | July String
    | August String
    | September String
    | October String
    | November String
    | December String
    | NeverHappens String


type QuizMahina
    = QuizMahina String


string2Month : String -> Month
string2Month st =
    case st of
        "June" ->
            June "https://gist.githubusercontent.com/TwistingTwists/d1b0a7ec408e1ccfa511f1d11d4458d9/raw/ce0b11428cbd308c9d6789b46213a34b6f4aeee5/JuneInsights.json"

        "July" ->
            July "https://gist.githubusercontent.com/TwistingTwists/d1b0a7ec408e1ccfa511f1d11d4458d9/raw/c87c46a68a18a2c50fe9932bfc703cdcbc43d190/JulyInsights.json"

        "August" ->
            August "https://gist.githubusercontent.com/TwistingTwists/409437d7fc146d05f313e59da44535e6/raw/8a7179d0fb2fed5c73938ab43cc04fd5ca0eaaa0/AugInsights.json"

        "September" ->
            September "Quiz not ready yet. Feel free to email to abeehsake2@telegmail.com"

        "October" ->
            October "Quiz not ready yet. Feel free to email to abeehsake2@telegmail.com"

        "November" ->
            November "Quiz not ready yet. Feel free to email to abeehsake2@telegmail.com"

        "December" ->
            December "Quiz not ready yet. Feel free to email to abeehsake2@telegmail.com"

        _ ->
            NeverHappens "If you've reached here, Congratulations! You've reached and impossible state in my Elm program."


month2String : Month -> String
month2String m =
    case m of
        June a ->
            a

        July a ->
            a

        August a ->
            a

        September a ->
            a

        October a ->
            a

        November a ->
            a

        December a ->
            a

        NeverHappens a ->
            a


month2url : String -> String
month2url mahina =
    mahina
        |> string2Month
        |> month2String
