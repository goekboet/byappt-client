module Home exposing (index)

import Html as Html exposing (Html)
import Html.Attributes as Attr exposing (class)
import Html.Events as Event
import Types exposing (..)


index : AuthNStatus -> Html Msg
index status =
    Html.div
        [ class "pure-menu" ]
        [ Html.ul
            [ class "pure-menu-list" ]
            [ authStatusLink status
            , internalLink "Hosts" "/hosts"
            , myAppointmentsLink status
            ]
        ]

myAppointmentsLink : AuthNStatus -> Html Msg
myAppointmentsLink status =
    case status of
        SignedIn _ -> internalLink "My Appointments" "/appointments"
        _ -> Html.text ""

internalLink : String -> String -> Html Msg
internalLink name target =
    Html.li
        [ class "pure-menu-item" ]
        [ Html.a
            [ Attr.href target
            , class "pure-menu-link"
            ]
            [ Html.text name ]
        ]


authStatusLink : AuthNStatus -> Html Msg
authStatusLink status =
    case status of
        NotSignedIn ->
            Html.li
                [ class "pure-menu-item" ]
                [ Html.a
                    [ Event.onClick RememberSession
                    , class "pure-menu-link"
                    ]
                    [ Html.text "Sign in" ]
                ]

        SignedIn _ ->
            Html.li
                [ class "pure-menu-item" ]
                [ Html.a
                    [ Event.onClick SignOut
                    , class "pure-menu-link"
                    ]
                    [ Html.text "Sign out" ]
                ]

        SigningOut ->
            Html.li
                [ class "pure-menu-item" ]
                [ Html.a
                    [ class "pure-menu-link" ]
                    [ Html.text "Signing out" ]
                ]

        _ ->
            Html.li
                [ class "pure-menu-item" ]
                [ Html.a
                    [ class "pure-menu-link" ]
                    [ Html.text "Signing in" ]
                ]
