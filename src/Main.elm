module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Task



-- ---------------------------
-- MODEL
-- ---------------------------


type Flags
    = Flags Bool Bool Bool


type alias Model =
    { message : String
    , clickCount : Int
    , flag2Count : Int
    , currentFlags : Flags
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { message = "いろいろクリック"
      , clickCount = 0
      , flag2Count = 0
      , currentFlags = Flags False False False
      }
    , Cmd.none
    )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = ClickFlag1
    | ClickFlag2
    | ClickFlag3
    | ClickGoal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ clickCount, flag2Count, currentFlags } as model) =
    case msg of
        ClickFlag1 ->
            case currentFlags of
                Flags True _ _ ->
                    ( { model
                        | message = "フラグ1は立っています。"
                        , clickCount = clickCount + 1
                      }
                    , Cmd.none
                    )

                Flags False flag2 flag3 ->
                    ( { model
                        | message = "フラグ1が立ちました。"
                        , clickCount = clickCount + 1
                        , currentFlags = Flags True flag2 flag3
                      }
                    , Cmd.none
                    )

        ClickFlag2 ->
            case currentFlags of
                Flags flag1 False flag3 ->
                    if flag2Count < 3 then
                        ( { model
                            | message = "フラグ2はあと、" ++ String.fromInt (3 - flag2Count) ++ "回"
                            , flag2Count = flag2Count + 1
                            , clickCount = clickCount + 1
                          }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | message = "フラグ2が立ちました。"
                            , currentFlags = Flags flag1 True flag3
                            , clickCount = clickCount + 1
                          }
                        , Cmd.none
                        )

                Flags flag1 True flag3 ->
                    ( { model
                        | message = "フラグ2は立っています。"
                        , clickCount = clickCount + 1
                      }
                    , Cmd.none
                    )

        ClickFlag3 ->
            case currentFlags of
                Flags flag1 flag2 False ->
                    ( { model
                        | message = "フラグ3はONになりました。"
                        , clickCount = clickCount + 1
                        , currentFlags = Flags flag1 flag2 True
                      }
                    , Cmd.none
                    )

                Flags flag1 flag2 True ->
                    ( { model
                        | message = "フラグ3はOFFになりました。"
                        , clickCount = clickCount + 1
                        , currentFlags = Flags flag1 flag2 False
                      }
                    , Cmd.none
                    )

        ClickGoal ->
            ( { model
                | message = "ゴール！"
                , clickCount = clickCount + 1
              }
            , Cmd.none
            )



-- ---------------------------
-- VIEW
-- ---------------------------


activeGoal : Flags -> String
activeGoal flags =
    case flags of
        Flags True True True ->
            " active"

        _ ->
            ""


view : Model -> Html Msg
view { message, currentFlags, clickCount } =
    section []
        [ div [ class "flag1", onClick ClickFlag1 ] [ h2 [] [ text "フラグ1" ] ]
        , div [ class "flag2", onClick ClickFlag2 ] [ h2 [] [ text "フラグ2" ] ]
        , div [ class "flag3", onClick ClickFlag3 ] [ h2 [] [ text "フラグ3" ] ]
        , div [ class <| "goal" ++ activeGoal currentFlags, onClick ClickGoal ] [ h2 [] [ text "ゴール" ] ]
        , Keyed.node "div"
            [ class "message" ]
            [ ( String.fromInt clickCount
              , h1 [] [ text message ]
              )
            ]
        ]



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "脱出ゲー"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }
