module Main exposing (main)

import Html exposing (Html, div, text, form, textarea, button, input)
import Html.Attributes exposing (type_, action, value, disabled)
import Html.Events exposing (onSubmit, onInput)
import Http
import Json.Decode as Json
import Json.Encode
import Browser
import Debug exposing (toString)

type alias Model =
    { newComment : NewComment
    , comments : List Comment
    }


emptyModel : Model
emptyModel =
    { newComment = emptyNewComment
    , comments = []
    }


emptyNewComment =
    NewComment "" ""


type alias NewComment =
    { title : String
    , body : String
    }


type Msg
    = AddComment
    | UpdateComment NewComment
    | AddCommentHttp (Result Http.Error Comment)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddComment ->
            let
                newComment =
                     model.newComment
            in
                ( { model | newComment = emptyNewComment }, postComment newComment )

        UpdateComment newComment ->
            ( { model | newComment = newComment }, Cmd.none )

        AddCommentHttp (Ok response) ->
            let
                _ =
                     response
            in
                ( { model | comments = model.comments ++ [ response ] }, Cmd.none )

        AddCommentHttp (Err err) ->
            let
                _ =
                     err
            in
                ( model, Cmd.none )

postComment newComment =
    Http.post
        { url = "https://jsonplaceholder.typicode.com/posts"
        , body = encodeNewComment newComment
        , expect = Http.expectJson AddCommentHttp decodeComment
        }


encodeNewComment : NewComment -> Http.Body
encodeNewComment newComment =
    Http.jsonBody <|
        Json.Encode.object
            [ ( "title", Json.Encode.string newComment.title )
            , ( "body", Json.Encode.string newComment.body )
            ]


type alias Comment =
    { title : String
    , body : String
    }


decodeComment : Json.Decoder Comment
decodeComment =
    Json.map2 Comment
        (Json.field "title" Json.string)
        (Json.field "body" Json.string)


view : Model -> Html Msg
view model =
    div [] <| [ viewForm model.newComment UpdateComment AddComment]
            ++ List.map (\comment -> div [] [ text <| toString comment ]) model.comments


viewForm : NewComment -> (NewComment -> msg) -> msg -> Html msg
viewForm newComment toUpdateComment addComment =
    form
        [ onSubmit addComment, action "javascript:void(0);" ]
        [ div []
            [ input
                [ value newComment.title
                , onInput (\v -> toUpdateComment { newComment | title = v })
                ]
                []
            ]
        , textarea
            [ value newComment.body
            , onInput (\v -> toUpdateComment { newComment | body = v })
            ]
            []
        , div []
            [ button
                [ type_ "submit"
                , disabled <| isEmpty newComment.title || isEmpty newComment.body
                ]
                [ text "Add Comment" ]
            ]
        ]

isEmpty : String -> Bool
isEmpty =
    String.isEmpty << String.trim


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> (emptyModel, Cmd.none)
        , update = update
        , subscriptions = always Sub.none
        }