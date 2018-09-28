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
import Browser.Navigation as Navigation
import Url as Url 


type Direction
    = Left
    | Right


type Msg
    = 
      SetPhotos (Result Http.Error (List Photo))
    | ScrollPick Direction
    | SetDescription (Result Http.Error ( String, String ))


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = \m -> Sub.none
    }



-- MODEL


type alias Model =
    { photos : Result Http.Error { left : List Photo, shown : Maybe Photo, right : List Photo }
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



-- Decode photos from "flickr.photosets.getPhotos" request.


decodeAlbumPhotos : DC.Decoder (List Photo)
decodeAlbumPhotos =
    DC.at [ "photoset", "photo" ] decodePhotoList



-- Decode names of photosets from "flickr.photosets.getList" request.


decodePhotoSets : DC.Decoder (List ( String, String ))
decodePhotoSets =
    DC.at [ "photosets", "photoset" ]
        (DC.list <|
            DC.map2 (\l r -> (l,r))
                ((DC.at [ "id" ]) DC.string)
                ((DC.at [ "title", "_content" ]) DC.string)
        )



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


photoSetsUrl : String -> String
photoSetsUrl uid =
    flickrRestServices
        ++ "&method=flickr.photosets.getList"
        ++ "&api_key="
        ++ apiKey
        ++ "&user_id="
        ++ uid
        ++ noJsonCallback


albumPhotosUrl : String -> ( String, List ( String, String ) ) -> Maybe String
albumPhotosUrl album ( uid, setList ) =
    let
        setForAlbum =
            List.head <| List.filter (\( id, name ) -> name == album) setList
    in
        case setForAlbum of
            Nothing ->
                Nothing

            Just ( id, name ) ->
                Just
                    (flickrRestServices
                        ++ "&method=flickr.photosets.getPhotos"
                        ++ "&api_key="
                        ++ apiKey
                        ++ "&user_id="
                        ++ uid
                        ++ "&photoset_id="
                        ++ id
                        ++ noJsonCallback
                    )


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


setDescriptionCmd : Maybe Photo -> Cmd Msg
setDescriptionCmd mdp =
    case mdp of
        Nothing ->
            Cmd.none

        Just dp ->
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



-- Cmd to get public photos in named user's album from flickr.
-- Package results as SetPhotos message.


getAlbumPhotosCmd : String -> String -> Cmd Msg
getAlbumPhotosCmd name album =
    let
        req =
            Http.get (userUrl name) decodeUser

        userTask =
            Http.toTask req

        setsTask uid =
            Task.map (\s -> ( uid, s )) <| Http.toTask (Http.get (photoSetsUrl uid) decodePhotoSets)

        albumPhotosTask sets =
            let
                murl =
                    albumPhotosUrl album sets
            in
                case murl of
                    Nothing ->
                        Task.fail (Http.BadUrl <| "album not found: " ++ album)

                    Just url ->
                        Http.toTask (Http.get url decodeAlbumPhotos)

        userPhotosTask =
            userTask |> (andThen setsTask) |> (andThen albumPhotosTask)
    in
        Task.attempt SetPhotos userPhotosTask



-- Initialize model based on URL 'routing' arguments.


init : () -> ( Model, Cmd Msg )
init _ =
    let
        cmd = getPhotosCmd "dave20477"
    in
        ( { photos = Ok { left = [], shown = Nothing, right = [] }}, cmd )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPhotos (Ok []) ->
            ( { model | photos = Ok { left = [], shown = Nothing, right = [] } }, Cmd.none )

        SetPhotos (Ok (f :: fs)) ->
            ( { model | photos = Ok { left = [], shown = Just f, right = fs } }, setDescriptionCmd (Just f) )

        SetPhotos (Err e) ->
            ( { model | photos = Err e }, Cmd.none )

        -- A left or right arrow was picked.  Move a photo left or right.
        -- Issue command to update the description for the displayed photo.
        ScrollPick dir ->
            case model.photos of
                Err e ->
                    ( { model | photos = Err e }, Cmd.none )

                Ok { right, shown, left } ->
                    case shown of
                        Nothing ->
                            ( model, Cmd.none )

                        Just sh ->
                            let
                                ns =
                                    case dir of
                                        Right ->
                                            case head right of
                                                Nothing ->
                                                    let
                                                        rev =
                                                            List.reverse <| sh :: left
                                                    in
                                                        { left = []
                                                        , shown = head rev
                                                        , right = drop 1 rev
                                                        }

                                                Just nsh ->
                                                    { left = sh :: left
                                                    , shown = Just nsh
                                                    , right = List.drop 1 right
                                                    }

                                        Left ->
                                            case head left of
                                                Nothing ->
                                                    let
                                                        rev =
                                                            List.reverse <| sh :: right
                                                    in
                                                        { right = []
                                                        , shown = head rev
                                                        , left = drop 1 rev
                                                        }

                                                Just nsh ->
                                                    { right = sh :: right
                                                    , shown = Just nsh
                                                    , left = List.drop 1 left
                                                    }
                            in
                                ( { photos = Ok ns }, setDescriptionCmd ns.shown )

        -- Update description of the currently viewed photo.
        SetDescription (Ok ( photoid, desc )) ->
            case model.photos of
                Err e ->
                    ( { photos = Err e}, Cmd.none )

                Ok { left, shown, right } ->
                    case (shown) of
                        Nothing ->
                            ( model, Cmd.none )

                        Just sh ->
                            let
                                described =
                                    { sh | description = Just desc }
                            in
                                if
                                    -- caption the right photo
                                    (described.id == photoid)
                                then
                                    ( { model | photos = Ok { left = left, shown = Just described, right = right } }, Cmd.none )
                                else
                                    ( model, Cmd.none )

        SetDescription (Err e) ->
            ( { photos = Err e}, Cmd.none )



-- VIEW


-- Draw an image 


imageWithArrows : String -> Html Msg
imageWithArrows im =
    svg
        [ SA.version "1.1"
        , SA.width "100%"
        , SA.height "100%"
        , SA.viewBox "-100 -60 200 120"
        , SA.preserveAspectRatio "none"
        ]
        ([ image
            [ SA.xlinkHref im
            , SA.x "-100"
            , SA.y "-60"
            , SA.width "200"
            , SA.height "120"
            ]
            []
         ]
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
            [ imageWithArrows (photoUrl ps) ]
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
        [ case model.photos of
            Err s ->
                text ("Error: " )

            Ok scroll ->
                    div
                        [ 
                                    HA.style "height" "100%" , 
                                    HA.style "width" "100%" , 
                                    HA.style "margin" "0" 
                        ]
                        (case scroll.shown of
                            Nothing ->
                                []

                            Just ph ->
                                [ photoInDiv ph ]
                        )
        ]
