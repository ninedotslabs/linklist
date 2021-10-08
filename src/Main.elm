module Main exposing (..)

-- Link List app

import Browser
import Browser.Navigation as Nav
import Components.Logo exposing (spinner)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as JD
import Url
import Url.Builder as Url
import Url.Parser as Parser exposing ((</>), Parser, custom, fragment, map, oneOf, s, top)



-- Types


type alias Link =
    { name : String
    , url : String
    }


type alias Person =
    { name : String
    , bio : String
    , avatar : String
    , links : List Link
    , slug : String
    }


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type ContentIndex
    = CIFailure
    | CILoading
    | CISuccess (List Person)


type ContentPerson
    = CPFailure
    | CPLoading
    | CPSuccess Person


type alias IndexModel =
    { content : ContentIndex }


type alias PersonModel =
    { content : ContentPerson }


type Page
    = PageNotFound
    | PageIndex IndexModel
    | PagePerson PersonModel


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | IndexMsg IndexMsg
    | PersonMsg PersonMsg


type IndexMsg
    = NoListPerson
    | GotListPerson (Result Http.Error (List Person))


type PersonMsg
    = NoPerson
    | GotPerson (Result Http.Error Person)

-- Logic


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    goTo url
        { key = key
        , page = PageIndex (IndexModel CILoading)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            goTo url model

        IndexMsg iMsg ->
            case iMsg of
                NoListPerson ->
                    ( model, Cmd.none )

                GotListPerson listPerson ->
                    case listPerson of
                        Ok lP ->
                            ( { model | page = PageIndex (IndexModel (CISuccess lP)) }, Cmd.none )

                        Err _ ->
                            ( { model | page = PageIndex (IndexModel CIFailure) }, Cmd.none )

        PersonMsg pMsg ->
            case pMsg of
                NoPerson ->
                    ( model, Cmd.none )

                GotPerson person ->
                    case person of
                        Ok p ->
                            ( { model | page = PagePerson (PersonModel (CPSuccess p)) }, Cmd.none )

                        Err _ ->
                            ( { model | page = PagePerson (PersonModel CPFailure) }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Views


view : Model -> Browser.Document Msg
view model =
    case model.page of
        PageNotFound ->
            { title = "404 Not Found"
            , body =
                [ viewStatic "THIS CONTENT IS NOT AVAILABLE IN YOUR COUNTRY." ]
                , a [ href "/" ] [ text "GO HOME" ]
                ]
            }

        PageIndex iModel ->
            { title = "Home"
            , body =
                case iModel.content of
                    CIFailure ->
                        [ viewStatic "Failure..." ]

                    CILoading ->
                        [ viewSpinner ]

                    CISuccess lP ->
                        [ div [ class "container" ] (List.map viewPersonHome lP) ]
            }

        PagePerson pModel ->
            case pModel.content of
                CPFailure ->
                    { title = "Failure"
                    , body = [ viewStatic "THIS CONTENT IS NOT AVAILABLE IN YOUR COUNTRY." ]
                    }

                CPLoading ->
                    { title = "Loading"
                    , body = [ viewSpinner ]
                    }

                CPSuccess p ->
                    let
                        { name } =
                            p
                    in
                    { title = name
                    , body = [ viewPerson p ]
                    }


viewSpinner =
    div [ class "container" ] [ spinner ]


viewStatic t =
    div [] [ text t ]


viewPersonHome : Person -> Html msg
viewPersonHome ps =
    div []
        [ a [ href ("/" ++ ps.slug), title ps.name ] [ img [ src ps.avatar, alt ps.name ] [] ]
        ]


viewPerson : Person -> Html msg
viewPerson ps =
    div [ class "p" ]
        [ img [ src ps.avatar, alt ps.name ] []
        , a [ href ps.slug ] [ h2 [] [ text ps.name ] ]
        , p [] [ text ps.bio ]
        , ul [] (List.map viewLink ps.links)
        ]


viewLink : Link -> Html msg
viewLink l =
    li [] [ a [ href l.url ] [ text l.name ] ]



-- Helper


goTo : Url.Url -> Model -> ( Model, Cmd Msg )
goTo url model =
    let
        parser =
            oneOf
                [ route top
                    (posIndex model
                        ( IndexModel CILoading
                        , getListPerson
                        )
                    )
                , route person_
                    (\person ->
                        case model.page of
                            PageIndex m ->
                                case m.content of
                                    CISuccess lP ->
                                        let
                                            filterPerson =
                                                List.head (List.filter (\p -> p.slug == person) lP)
                                        in
                                        case filterPerson of
                                            Just p ->
                                                posPerson model ( PersonModel (CPSuccess p), Cmd.none )

                                            Nothing ->
                                                posNotFound model

                                    _ ->
                                        posPerson model ( PersonModel CPLoading, getPerson person )

                            _ ->
                                posPerson model ( PersonModel CPLoading, getPerson person )
                    )
                ]
    in
    case Parser.parse parser url of
        Just answer ->
            answer

        Nothing ->
            ( { model | page = PageNotFound }
            , Cmd.none
            )


person_ : Parser (String -> a) a
person_ =
    Parser.string


posNotFound : Model -> ( Model, Cmd Msg )
posNotFound model =
    ( { model | page = PageNotFound }
    , Cmd.none
    )


posIndex : Model -> ( IndexModel, Cmd IndexMsg ) -> ( Model, Cmd Msg )
posIndex model ( iModel, cmds ) =
    ( { model | page = PageIndex iModel }
    , Cmd.map IndexMsg cmds
    )


posPerson : Model -> ( PersonModel, Cmd PersonMsg ) -> ( Model, Cmd Msg )
posPerson model ( pModel, cmds ) =
    ( { model | page = PagePerson pModel }
    , Cmd.map PersonMsg cmds
    )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


toPerson : String -> String
toPerson uName =
    Url.absolute [ uName ] []



-- HTTP


getPerson uName =
    Http.get
        { url = "/assets/data/" ++ uName ++ ".json"
        , expect = Http.expectJson GotPerson personDecoder
        }


getListPerson =
    Http.get
        { url = "/list.json"
        , expect = Http.expectJson GotListPerson listPersonDecoder
        }



-- DECODER


linkDecoder : JD.Decoder Link
linkDecoder =
    JD.map2 Link
        (JD.field "name" JD.string)
        (JD.field "url" JD.string)


personDecoder : JD.Decoder Person
personDecoder =
    JD.map5 Person
        (JD.field "name" JD.string)
        (JD.field "bio" JD.string)
        (JD.field "avatar" JD.string)
        (JD.field "links" (JD.list linkDecoder))
        (JD.field "slug" JD.string)


listPersonDecoder : JD.Decoder (List Person)
listPersonDecoder =
    JD.list personDecoder
