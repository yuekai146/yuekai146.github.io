import Browser
import Dict
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import List


vec_dim = 4
num_nearest = 1

-- Linear algebra operators
square n = n*n


normalize v = 
  List.map2 (/) v (List.repeat (List.length v) (sqrt (List.sum (List.map square v))))


vec_add a b = 
  List.map2 (+) a b


vec_minus a b = 
  List.map2 (-) a b


vec_mul a b = 
  List.map2 (*) a b


vec_div a b = 
  List.map2 (/) a b


vec_dot a b = 
   List.sum (vec_mul a b)


mat_vec_mul mat vec = 
  List.map2 vec_dot mat (List.repeat (List.length mat) vec)


zip_list word_list score_list = 
  List.map2 (\a b -> {word=a, score=-b}) word_list score_list


get_topk word_list score_list = 
  List.map .word (List.take num_nearest (List.sortBy .score (zip_list word_list score_list)))


get_vec wordvec_dict w = case Dict.get w wordvec_dict of
 Just vec -> vec
 Nothing -> List.repeat 300 0


check wordvec_dict w = case Dict.get w wordvec_dict of
  Just vec -> True

  Nothing -> False


check_triple wordvec_dict w1 w2 w3 = 
  (check wordvec_dict w1) && ( ( check wordvec_dict w2 ) && ( check wordvec_dict w3 ) )


get_candidate_vec wordvec_dict w1 w2 w3 = 
  normalize (vec_add (vec_minus ( get_vec wordvec_dict w1 ) ( get_vec wordvec_dict w2 )) ( get_vec wordvec_dict w3 ))


get_word2vec wordvec_dict w1 w2 w3 = 
  Dict.filter (\k v -> ((k /= w1) && (k /= w2) && (k /= w3))) wordvec_dict


get_result wordvec_dict w1 w2 w3 = 
 if (check_triple wordvec_dict w1 w2 w3) then
  List.foldl (\t1 t2 -> (t1 ++ ("\n" ++ t2))) "" (get_topk (Dict.keys (get_word2vec wordvec_dict w1 w2 w3)) (mat_vec_mul (Dict.values (get_word2vec wordvec_dict w1 w2 w3)) (get_candidate_vec wordvec_dict w1 w2 w3)))
 else
  ""


convert l = 
 Tuple.mapSecond ( List.map toFloat ) (split_w_v l)


toFloat x = 
 case String.toFloat x of
 Just val -> val        
 Nothing -> 0.0


split_w_v l =
 case ( List.head l ) of 
  Just word ->                       
   case ( List.tail l ) of           
    Just vec -> (word, vec)         
    Nothing ->                        
     ("", ( List.repeat vec_dim "0.0" ))      
  Nothing -> ("", ( List.repeat vec_dim "0.0" ))

filter_wordvec_dict w2v = 
 Dict.filter (\k v -> k /= "") w2v

parse_wordvec wordvec_text = 
 filter_wordvec_dict ( Dict.map (\k a -> (normalize a)) ( Dict.fromList ( List.map convert ( List.map String.words (String.lines wordvec_text) ) ) ) ) 



-- MAIN


main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Content =
  {w1 : String, w2 : String, w3 : String }


type Model =
  Failure
  | Loading
  | Success ( Dict.Dict String (List Float) ) Content


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
     { url = "https://raw.githubusercontent.com/yuekai146/yuekai146.github.io/master/uy_model.vec"
     , expect = Http.expectString GotText
     }
  )


-- UPDATE


type Msg
  = Change String
  | Change2 String
  | Change3 String
  | GotText (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Change newContent ->
     case model of 
      Success wordvec_dict content -> 
       ( ( Success wordvec_dict { content | w1 = newContent } ), Cmd.none )
      Failure ->
       ( Failure, Cmd.none )
      Loading ->
       ( Loading, Cmd.none )
    Change2 newContent ->
     case model of 
      Success wordvec_dict content -> 
       ( ( Success wordvec_dict { content | w2 = newContent } ), Cmd.none ) 
      Failure ->
       ( Failure, Cmd.none )
      Loading ->
       ( Loading, Cmd.none )
    Change3 newContent ->
     case model of 
      Success wordvec_dict content -> 
       ( ( Success wordvec_dict { content | w3 = newContent } ), Cmd.none )
      Failure ->
       ( Failure, Cmd.none )
      Loading ->
       ( Loading, Cmd.none )
    GotText result ->
      case result of
        Ok fullText ->
          ( Success (parse_wordvec fullText) ({ w1 = "", w2 = "", w3 = "" }), Cmd.none )

        Err _ ->
          ( Failure, Cmd.none )


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
 Sub.none


-- VIEW


view : Model -> Html Msg
view model =
 case model of
  Failure ->
   text "Unable to load uyghur word vectors."

  Loading ->
   text "Loading uyghur word vectors."

  Success wordvec_dict content ->
    div [style "font-size" "48px"]
      [ text "Word2vec demo"
      , div [] []
      , input [ placeholder "Input a word", value content.w1, onInput Change ] []
      , div [style "font-size" "24px", style "display" "inline"] [text " - "]
      , input [ placeholder "Input a word", value content.w2, onInput Change2 ] []
      , div [style "font-size" "16px", style "display" "inline"] [text " + "]
      , input [ placeholder "Input a word", value content.w3, onInput Change3 ] []
      , div [] []
      , div [style "font-size" "24px", style "display" "inline"] [text " = ", text (
          get_result wordvec_dict content.w1 content.w2 content.w3
          ) ]
      ]