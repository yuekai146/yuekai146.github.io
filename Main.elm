import Array
import Browser
import Bytes
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (..)
import Dict
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import List


vec_dim = 5
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
 Nothing -> List.repeat vec_dim 0


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


toFloat x = 
 case String.toFloat x of
 Just val -> val        
 Nothing -> 0.0


filter_wordvec_dict w2v = 
 Dict.filter (\k v -> k /= "") w2v


vec_reshape vec = List.map Array.toList ( Array.toList ( Array.initialize ( (List.length vec) // vec_dim ) ( \row -> ( Array.initialize vec_dim ( \col -> list_take ( row * vec_dim + col + 1 ) vec ) ) ) ) )


list_take index vec = 
 if index == 1 then
  case (List.head vec) of 
   Just x -> x
   Nothing -> 0
 else
  case (List.tail vec) of 
   Just l_tmp -> (list_take (index-1) l_tmp)
   Nothing -> 0


parse_wordvec words vecs = 
 filter_wordvec_dict ( Dict.map (\k a -> (normalize a)) ( Dict.fromList ( List.map2 ( \k v -> (k, v) ) ( String.lines words ) ( vec_reshape vecs ) ) ) )

-- MAIN


main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Content =
  {w1 : String, w2 : String, w3 : String }


type Model =
  Failure
  | Loading
  | Success_words String
  | Success ( Dict.Dict String (List Float) ) Content


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Cmd.batch 
     [ Http.get
        { url = "https://raw.githubusercontent.com/yuekai146/yuekai146.github.io/master/model.words"
        , expect = Http.expectString GotWords
        }
     ]
  )


-- UPDATE


type Msg
  = Change String
  | Change2 String
  | Change3 String
  | GotWords (Result Http.Error String) 
  | GotVecs (Result Http.Error (List Float) )


list : Decoder a -> Decoder (List a)
list decoder =
  unsignedInt32 BE
    |> andThen (\len -> Decode.loop (len, []) (listStep decoder))


listStep : Decoder a -> (Int, List a) -> Decoder (Step (Int, List a) (List a))
listStep decoder (n, xs) =
  if n <= 0 then
    succeed (Done xs)
  else
    Decode.map (\x -> Loop (n - 1, x :: xs)) decoder


decodeVecs = list ( Decode.float32 BE )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Change newContent ->
     case model of 
      Success wordvec_dict content -> 
       ( ( Success wordvec_dict { content | w1 = newContent } ), Cmd.none )
      Success_words words ->
       ( Failure, Cmd.none )
      Failure ->
       ( Failure, Cmd.none )
      Loading ->
       ( Loading, Cmd.none )
    
    Change2 newContent ->
     case model of 
      Success wordvec_dict content -> 
       ( ( Success wordvec_dict { content | w2 = newContent } ), Cmd.none )
      Success_words words ->
       ( Failure, Cmd.none ) 
      Failure ->
       ( Failure, Cmd.none )
      Loading ->
       ( Loading, Cmd.none )
    
    Change3 newContent ->
     case model of 
      Success wordvec_dict content -> 
       ( ( Success wordvec_dict { content | w3 = newContent } ), Cmd.none )
      Success_words words ->
       ( Failure, Cmd.none )
      Failure ->
       ( Failure, Cmd.none )
      Loading ->
       ( Loading, Cmd.none )
    
    GotWords result ->
      case result of
       Ok fullWords -> 
        ( Success_words fullWords,  Http.get { url = "https://raw.githubusercontent.com/yuekai146/yuekai146.github.io/master/model.vec.b32", expect = Http.expectBytes GotVecs decodeVecs } )
       Err _ ->
        ( Failure, Cmd.none )

    GotVecs result ->
      case result of 
       Ok fullVecs ->
        case model of 
         Success wordvec_dict content -> 
          ( Success wordvec_dict content, Cmd.none )
         Success_words words ->
          ( Success ( parse_wordvec words fullVecs ) { w1 = "", w2 = "", w3 = "" }, Cmd.none )
         Failure ->
          ( Failure, Cmd.none )
         Loading ->
          ( Loading, Cmd.none )
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
   text "Loading uyghur words."

  Success_words words ->
   text "Loading word vectors."

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
