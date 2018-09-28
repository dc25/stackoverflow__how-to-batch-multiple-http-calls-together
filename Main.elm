module Main exposing (..)

import String
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Maybe
import Svg exposing (Svg, svg, rect, polygon, polyline, image)
import Svg.Attributes as SA
import Svg.Events as SE
import List exposing (head, drop, take)
import Json.Decode as DC exposing (Decoder)
import Task exposing (andThen)
import Browser


type Msg
    = 
      SetPhotos (Result Http.Error (List Photo))
    | SetDescription (Result Http.Error ( String, String ))


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = \m -> Sub.none
    }

type alias Model = Result Http.Error (List Photo) 

decodeUser : DC.Decoder String
decodeUser =
    DC.at [ "user", "id" ] DC.string


type alias Photo =
    { id : String
    , secret : String
    , server : String
    , farm : Int
    , description : Maybe String
    }



-- Create a Photo record from info retrieved from flickr api.
-- Defer extra api call needed for Description until photo displayed.


initPhoto : String -> String -> String -> Int -> Photo
initPhoto id sec ser farm =
    Photo id sec ser farm Nothing


decodePhotoList : DC.Decoder (List Photo)
decodePhotoList =
    (DC.list <|
        DC.map4 initPhoto
            ((DC.at [ "id" ]) DC.string)
            ((DC.at [ "secret" ]) DC.string)
            ((DC.at [ "server" ]) DC.string)
            ((DC.at [ "farm" ]) DC.int)
    )



-- Decode photos from "flickr.people.getPublicPhotos" request.


decodePhotos : DC.Decoder (List Photo)
decodePhotos =
    DC.at [ "photos", "photo" ] decodePhotoList



-- Decode descripion of photo from "flickr.photos.getInfo" request.

decodePhotoDescription : DC.Decoder String
decodePhotoDescription =
    DC.at [ "photo", "description", "_content" ] DC.string


-- api key from flickr.  Anyone who clones this project should
-- get their own api key.

apiKey : String
apiKey =
    "859b1fdf671b6419805ec3d2c7578d70"


flickrRestServices : String
flickrRestServices =
    "https://api.flickr.com/services/rest/?"


noJsonCallback : String
noJsonCallback =
    "&format=json&nojsoncallback=1"


userUrl : String -> String
userUrl name =
    flickrRestServices
        ++ "&method=flickr.people.findByUserName"
        ++ "&api_key="
        ++ apiKey
        ++ "&username="
        ++ name
        ++ noJsonCallback


publicPhotosUrl : String -> String
publicPhotosUrl uid =
    flickrRestServices
        ++ "&method=flickr.people.getPublicPhotos"
        ++ "&api_key="
        ++ apiKey
        ++ "&user_id="
        ++ uid
        ++ noJsonCallback


photoInfoUrl : String -> String
photoInfoUrl photo =
    flickrRestServices
        ++ "&method=flickr.photos.getInfo"
        ++ "&api_key="
        ++ apiKey
        ++ "&photo_id="
        ++ photo
        ++ noJsonCallback



-- Cmd to get visible photo's description from flickr.
-- Package results as SetDescription message.
-- Save the photo id with Task.map to verify the same photo is being displayed when the response comes back.


setDescriptionCmd : Photo -> Cmd Msg
setDescriptionCmd dp =
            case (dp.description) of
                Nothing ->
                    Task.attempt SetDescription (Task.map (\s -> ( dp.id, s )) <| Http.toTask <| Http.get (photoInfoUrl (dp.id)) decodePhotoDescription)

                Just des ->
                    Cmd.none



-- Cmd to get users public photos from flickr.
-- Package results as SetPhotos message.

getPhotosCmd : String -> Cmd Msg
getPhotosCmd name =
    let
        req =
            Http.get (userUrl name) decodeUser

        userTask =
            Http.toTask req

        publicPhotosTask uid =
            Http.toTask (Http.get (publicPhotosUrl uid) decodePhotos)

        userPhotosTask =
            userTask |> (andThen publicPhotosTask)
    in
        Task.attempt SetPhotos userPhotosTask



init : () -> ( Model, Cmd Msg )
init _ =
    let
        cmd = getPhotosCmd "dave20477"
    in
        ( Ok [],cmd )

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPhotos phs ->
            ( phs, Cmd.none )

        -- Update description of the currently viewed photo.
        SetDescription (Ok ( photoid, desc )) ->
            case model of
                Err e ->
                    ( Err e, Cmd.none )

                Ok phs ->
                    ( Ok phs, Cmd.none )

        SetDescription (Err e) ->
            ( Err e, Cmd.none )



-- VIEW


-- Compute a photo URL from a Photo record.
-- per: https://www.flickr.com/services/api/misc.urls.html


photoUrl : Photo -> String
photoUrl ps =
    "https://farm"
        ++ String.fromInt ps.farm
        ++ ".staticflickr.com/"
        ++ ps.server
        ++ "/"
        ++ ps.id
        ++ "_"
        ++ ps.secret
        ++ "_b.jpg"



-- show an image and description if available.


photoInDiv : Photo -> Html Msg
photoInDiv ps =
    div
        [ 
              HA.style "height" "100%" 
            , HA.style "width" "100%" 
            , HA.style "margin" "0" 
             
        ]
        [ div
                [ HA.style   "height"  "90%" 
                , HA.style   "width"  "100%" 
                , HA.style   "margin"  "0" 
                ]
            [ svg
                  [ SA.version "1.1"
                  , SA.width "100%"
                  , SA.height "100%"
                  , SA.viewBox "-100 -60 200 120"
                  , SA.preserveAspectRatio "none"
                  ]
                  ([ image
                      [ SA.xlinkHref (photoUrl ps)
                      , SA.x "-100"
                      , SA.y "-60"
                      , SA.width "200"
                      , SA.height "120"
                      ]
                      []
                   ]
                  )
            ]

        , div
                [ HA.style   "height"  "10%"
                , HA.style   "width"  "100%"
                , HA.style   "margin"  "0"
                ]
            [ div [ HA.style "text-align" "center" ]
                [ text <| Maybe.withDefault "" ps.description ]
            ]
        ]

-- Draw an image or display the reason the image is not available.


view : Model -> Html Msg
view model =
    div []
        [ case model of
            Err s ->
                text ("Error: " )

            Ok ([] ) ->
                    div
                        [ 
                                    HA.style "height" "100%" , 
                                    HA.style "width" "100%" , 
                                    HA.style "margin" "0" 
                        ]
                        [ ]
            Ok (ph :: others) ->
                    div
                        [ 
                                    HA.style "height" "100%" , 
                                    HA.style "width" "100%" , 
                                    HA.style "margin" "0" 
                        ]
                        [ photoInDiv ph ]
        ]
