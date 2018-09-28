module Main exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Http
import Json.Decode as DC
import Svg
import Svg.Attributes as SA
import Task


type Msg
    = SetPhotos (Result Http.Error (List Photo))
    | SetDescription (Result Http.Error ( String, String ))


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        }


type alias Model =
    Result Http.Error
        { untitled : List Photo
        , titled : List Photo
        }


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
-- Get description later


initPhoto : String -> String -> String -> Int -> Photo
initPhoto id sec ser farm =
    Photo id sec ser farm Nothing


decodePhotoList : DC.Decoder (List Photo)
decodePhotoList =
    DC.list <|
        DC.map4 initPhoto
            (DC.at [ "id" ] DC.string)
            (DC.at [ "secret" ] DC.string)
            (DC.at [ "server" ] DC.string)
            (DC.at [ "farm" ] DC.int)



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
    "e9d3fdd5c2e26f9ebd13f4983cf727db"


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



-- Cmd to get photo's description from flickr.
-- Package results as SetDescription message.
-- Save the photo id with Task.map to apply the description to the right photo


setDescriptionCmd : Photo -> Cmd Msg
setDescriptionCmd dp =
    case dp.description of
        Nothing ->
            Task.attempt SetDescription (Task.map (\s -> ( dp.id, s )) <| Http.toTask <| Http.get (photoInfoUrl dp.id) decodePhotoDescription)

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
            userTask |> Task.andThen publicPhotosTask
    in
    Task.attempt SetPhotos userPhotosTask


init : () -> ( Model, Cmd Msg )
init _ =
    ( Ok
        { untitled = []
        , titled = []
        }
    , getPhotosCmd "elmDemo" -- flickr user name
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPhotos (Ok photos) ->
            ( Ok
                { untitled = photos
                , titled = []
                }
            , Cmd.batch <| List.map setDescriptionCmd photos
            )

        SetPhotos (Err e) ->
            ( Err e
            , Cmd.none
            )

        -- Update description of the photo with matching id.
        SetDescription (Ok ( photoId, desc )) ->
            case model of
                Ok photos ->
                    let
                        justTitled =
                            photos.untitled
                                |> List.filter (\ph -> ph.id == photoId)
                                |> List.map (\ph -> { ph | description = Just desc })
                        newTitled = photos.titled ++ justTitled 
                        newPhotos = { photos | titled = newTitled }
                    in
                    ( Ok newPhotos
                    , if
                        List.length newPhotos.titled
                            == List.length newPhotos.untitled
                      then
                        Cmd.none -- Could do something else here.

                      else
                        Cmd.none
                    )

                Err e ->
                    ( Err e
                    , Cmd.none
                    )

        SetDescription (Err e) ->
            ( Err e
            , Cmd.none
            )



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


viewPhoto : Photo -> Html Msg
viewPhoto ps =
    div
        [ HA.style "height" "20%"
        , HA.style "width" "20%"
        , HA.style "margin" "0"
        ]
        [ div
            [ HA.style "height" "90%"
            , HA.style "width" "100%"
            , HA.style "margin" "0"
            ]
            [ Svg.svg
                [ SA.version "1.1"
                , SA.width "100%"
                , SA.height "100%"
                , SA.viewBox "-1 -0.6 2 1.2"
                , SA.preserveAspectRatio "none"
                ]
                [ Svg.image
                    [ SA.xlinkHref (photoUrl ps)
                    , SA.x "-1"
                    , SA.y "-0.6"
                    , SA.width "2"
                    , SA.height "1.2"
                    ]
                    []
                ]
            ]
        , div
            [ HA.style "height" "10%"
            , HA.style "width" "100%"
            , HA.style "margin" "0"
            ]
            [ div
                [ HA.style "text-align" "center" ]
                [ text <| Maybe.withDefault "" ps.description ]
            ]
        ]



-- Draw an image or display the reason the image is not available.


view : Model -> Html Msg
view model =
    case model of
        Err s ->
            text "Error: "

        Ok photos ->
            div []
                [ div [] [ text "Untitled" ]
                , div [] (List.map viewPhoto photos.untitled)
                , div [] [ text "Titled" ]
                , div [] (List.map viewPhoto photos.titled)
                ]
