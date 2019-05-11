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


-- MAIN


main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


errorMessage : Http.Error -> String
errorMessage error =
  case error of
    Http.Timeout ->
      "Timeout"
    Http.NetworkError  ->
      "Network Error"
    Http.BadUrl url ->
      "BadUrl: " ++ url
    Http.BadStatus status_code ->
      "BadStatus: " ++ ( String.fromInt status_code )
    Http.BadBody body ->
      "Bad body: " ++ body
 

-- MODEL


type Model =
  Failure String
  | Loading
  | Success Float


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Cmd.batch 
     [ Http.get
        { url = "https://raw.githubusercontent.com/yuekai146/yuekai146.github.io/master/model.vec.b32"
        , expect = Http.expectBytes GotVecs decodeVecs
        }
     ]
  )


-- UPDATE


type Msg
  = GotVecs ( Result Http.Error (List Float) )


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
    GotVecs result ->
      case result of 
       Ok fullVecs -> 
         ( Success ( List.sum fullVecs ), Cmd.none )
       Err error ->
         ( Failure ( errorMessage error ), Cmd.none )


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
 Sub.none


-- VIEW


view : Model -> Html Msg
view model =
 case model of
  Failure err ->
   
   text err

  Loading ->
   text "Loading word vectors."

  Success vec_sum ->
    div [style "font-size" "48px"]
      [ text "Word2vec demo"
      , div [] []
      , div [style "font-size" "24px", style "display" "inline"] [text ( String.fromFloat vec_sum )]
      ]
