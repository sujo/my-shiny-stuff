port module Main exposing (main)

import Html exposing (Html, div, text, img, h3, h4, section, button, span, a, p, i)
import Html.Attributes exposing (class, src, style, href)
import Html.Events exposing (onClick)
import Browser exposing (document)
import Http
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Set

import Bulma exposing (viewTextField, viewButton, viewErrors, hero)
import GuildWars2 as GW2

-- MAIN

main : Program JD.Value Model Msg
main =
    Browser.document
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


-- MODEL

type Status
    = Configure
    | ShowItems


-- Tag name isActive itemFilter
type ItemTagValue = TagValue Bool (GW2.ItemSpec -> Bool)

type alias ItemTags = Dict String ItemTagValue

type alias Model =
    { status : Status
    , apiKey : String
    , errors : List String
    , showInfo : Bool
    , bank : List GW2.ContainerItem
    , characters : List GW2.Character
    , items : Dict Int GW2.ItemSpec
    , loading : Int
    , shiny : String
    , activeTags : ItemTags
    }


init : JD.Value -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { status = Configure
            , apiKey = ""
            , errors = []
            , showInfo = False
            , bank = []
            , characters = []
            , items = Dict.empty
            , loading = 0
            , shiny = ""
            , activeTags = Dict.empty
            }
    in
        case JD.decodeValue (JD.field "apiKey" JD.string) flags of
            Ok apiKey_ ->
                ( { model | status = ShowItems, apiKey = apiKey_ } , Cmd.none )
                |> loadBank
                |> loadCharacters

            Err _ ->
                ( model , Cmd.none )



-- UPDATE


type Msg
    = ApiKeyInput String
    | ShinyInput String
    | GoConfigure
    | ClearErrors
    | SaveConfig
    | NoSaveConfig
    | ShowInfo
    | HideInfo
    | AddTag String ItemTagValue
    | RemoveTag String
    | ReceivedBank (Result Http.Error (List GW2.ContainerItem) )
    | ReceivedItemSpecs (Result Http.Error (List GW2.ItemSpec) )
    | ReceivedCharacters (Result Http.Error (List GW2.Character) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApiKeyInput s ->
            ( { model | apiKey = s }, Cmd.none )

        ShinyInput s ->
            ( { model | shiny = String.toLower s }, Cmd.none )

        GoConfigure ->
            ( { model | status = Configure }, Cmd.none )

        SaveConfig ->
            ( { model | status = ShowItems, errors = [] }
            , setStorage (JE.object [ ( "apiKey", JE.string model.apiKey ) ] )
            )
            |> loadBank
            |> loadCharacters

        NoSaveConfig ->
            ( { model | status = ShowItems }, Cmd.none )

        ShowInfo ->
            ( { model | showInfo = True }, Cmd.none )

        HideInfo ->
            ( { model | showInfo = False }, Cmd.none )

        AddTag s t ->
            ( { model | activeTags = Dict.insert s t model.activeTags }, Cmd.none)

        RemoveTag s ->
            ( { model | activeTags = Dict.remove s model.activeTags }, Cmd.none)

        ClearErrors ->
            ( { model | errors = [] }, Cmd.none )

        ReceivedBank (Err (Http.BadStatus 401)) ->
            -- This happens when the API key is invalid or does not have sufficient permissions.
            ( { model
                | status = Configure
                , errors = [ """Error 401 from the GW2 API while loading the bank.
Please check if the API key is valid and has bank permissions.""" ]
              } |> subRequest
              , Cmd.none )

        ReceivedBank (Err err) ->
            ( { model 
                  | errors = ( "Error loading the bank from the GW2 API: "
                      ++ httpErrString err)
                      :: model.errors
              }
              |> subRequest
            , Cmd.none )

        ReceivedBank (Ok bankSlots) ->
                ( { model | bank = bankSlots } |> subRequest, Cmd.none )
                |> loadItemNames bankSlots model.items

        ReceivedItemSpecs (Err (Http.BadStatus 404)) ->
            -- This happens when an item ID is unknown to the items endpoint.
            -- The item ID is of no use to the user, so we ignore this specific error code.
            ( model |> subRequest, Cmd.none )

        ReceivedItemSpecs (Err err) ->
            ( { model 
                | errors = ( "Error loading item names from the GW2 API: "
                            ++ httpErrString err)
                            :: model.errors
              }
              |> subRequest
            , Cmd.none )

        ReceivedItemSpecs (Ok specs) ->
            let
                items
                     = specs
                    |> List.map (\a -> (a.id, a) )
                    |> Dict.fromList
                    |> Dict.union model.items

            in
                ( { model
                    | items = items
                    }
                    |> subRequest
                 , Cmd.none )

        ReceivedCharacters (Err (Http.BadStatus 401)) ->
            -- This happens when the API key is invalid or does not have sufficient permissions.
            ( { model
                | status = Configure
                , errors = [ """Error 401 from the GW2 API while loading the characters.
Please check if the API key is valid and has characters permissions.""" ]
              } |> subRequest
              , Cmd.none )

        ReceivedCharacters (Err err) ->
            ( { model 
                | errors = ( "Error loading the characters from the GW2 API: "
                            ++ httpErrString err)
                            :: model.errors
              }
              |> subRequest
            , Cmd.none )

        ReceivedCharacters (Ok characters) ->
            let
                items =
                    characters
                    |> List.map .bags
                    |> List.concat

            in
                ( { model | characters = characters } |> subRequest, Cmd.none )
                |> loadItemNames items model.items
                


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> { title : String, body : List (Html Msg) }
view model =
    let
        title = "My Shiny Stuff"

        filter = itemFilter model.activeTags model.shiny model.items

        (itemTagsBank, htmlBank) = viewBank model.items filter model.bank

        (itemTagsChars, htmlChars) = 
            List.foldr
                (\c (ts0, hl0) ->
                    let
                        (ts, hl) = viewCharacter model.items filter c
                    in
                        (Dict.union ts ts0, hl :: hl0)
                )
                (Dict.empty, [])
                model.characters

        tags = Dict.union itemTagsBank itemTagsChars
            |> Dict.union model.activeTags

        content = 
            case model.status of

                Configure ->
                    viewConfigure model

                ShowItems ->
                    viewControls model.shiny tags (model.loading > 0)
                    :: htmlBank
                    :: List.append htmlChars
                        [ div [ class "section" ] [ text "Nothing else matters." ] ]

    in
        { title = title
        , body =
            [ hero title
            , viewErrors ClearErrors model.errors
            , div [ class "container" ]
                (case model.showInfo of
                    True ->
                        [ viewInfo
                        , div [ class "container", style "opacity" "0.2" ] content ]

                    False ->
                        [ div [ class "container", style "opacity" "1" ] content ]
                )
            ]
        }


viewConfigure : Model -> List (Html Msg)
viewConfigure model =
    let
        arena_url = "https://account.arena.net/applications"
    in
    [ div [ class "container" ]
        [ span [ class "icon is-pulled-right", style "padding" "2px", onClick ShowInfo ]
            [ img [ src "assets/images/help.svg" ] []
            ]
        ]
    , section [ class "section" ]
        [ div [ class "container" ]
            [ h3 [ class "title is-3" ] [ text "Settings" ]
            , div [ class "content" ]
                [ p [] [ text
                    """This application uses your Guild Wars 2 API key to read the items from your game account.
        The API key is stored in your browser only."""
                    ]
                , p [] [ text
                    "Create an API key at "
                    , a [ href arena_url ] [ text arena_url ]
                    , text """. The API key must have the permissions 'account', 'inventories' and 'characters'."""
                    ]
                ]
            , viewTextField "API key" model.apiKey ApiKeyInput
            , viewButton "Save" "is-info" SaveConfig
            , viewButton "Cancel" "is-info is-light" NoSaveConfig
            ]
        ]
    ]


viewControls : String -> ItemTags -> Bool -> Html Msg
viewControls shiny tags loading =
        div [ class "container is-fluid" ] [ div [ class "level" ]
            [ div [ class "level-item" ]
                [ viewTextField "My Shiny" shiny ShinyInput
                ]
            , div [ class "tags level-item" ]
                (Dict.foldr
                    (\t (TagValue active f) l ->
                        case active of
                            True ->
                                (a [ class "tag active is-info", onClick (RemoveTag t) ] [ text t ]) :: l
                            False ->
                                (a [ class "tag inactive is-info is-light", onClick (AddTag t (TagValue True f)) ] [ text t ]) :: l
                    ) []
                    tags
                )
            , div [ class "level-item" ]
                [ span [ class "icon", style "visibility" (case loading of
                    False -> "hidden"
                    True -> "visibility"
                    ) ]
                    [ i [ class "loader" ] [] ]
                , span [ class "icon", style "padding" "2px", onClick ShowInfo ] [ img [ src "assets/images/help.svg" ] [] ]
                , span [ class "icon" ] [ img [ onClick GoConfigure, src "assets/images/cog-line.svg" ] [] ]
                ]
        ] ]


viewInfo : Html Msg
viewInfo =
    div [ class "info container" ]
        [ div [ class "message" ]
            [ div [ class "message-header" ]
                [ text "About"
                , button [ class "delete", onClick HideInfo ] []
                ]
            , div [ class "message-body" ]
                [ p []
                    [ text "My Shiny Stuff shows, filters and searches in-game items of your Guild Wars 2 account bank and characters." ]
                , p [ style "margin-top" "0.3em" ]
                    [ text "It was created by 'Klef Pa.5418'. If you like this application, I'd be very happy about a donation of in-game shiny stuff, skins or gold to that account."
                    ]
                ,  p [ style "margin-top" "0.3em" ]
                    [ text "The "
                    , a [ href "https://elm-lang.org/" ] [ text "elm" ]
                    , text " source code is available on "
                    , a [ href "https://github.com/sujo/my-shiny-stuff" ] [ text "Github" ]
                    , text " under "
                    , a [ href "https://www.gnu.org/licenses/gpl-3.0.txt" ] [ text "the GNU GPLv3 license" ]
                    , text "."
                    ]
                ,  p [ style "margin-top" "0.3em" ]
                    [ a [ href "https://github.com/sujo/my-shiny-stuff/blob/master/CHANGES.md" ]
                        [ text "Release notes" ]
                    ]
                ,  p [ style "margin-top" "0.3em" ]
                    [ text "All images, the favicon and the item descriptions are under "
                    , a [ href "https://www.guildwars2.com/en/legal/guild-wars-2-content-terms-of-use/" ] [ text "copyright by ArenaNet, LLC" ]
                    , text "."
                    ]
                ]
            ]
        ]


extractTags : GW2.ItemSpec -> ItemTags
extractTags it =
    case it.iType.subType of
        Just (GW2.Armor weight slot) ->
            Dict.singleton slot
                ( TagValue False
                    (\i ->
                     case i.iType.subType of
                         Just (GW2.Armor _ slot2) -> slot == slot2
                         _ -> False
                    )
                )
            |> Dict.insert weight
                ( TagValue False
                    (\i ->
                     case i.iType.subType of
                         Just (GW2.Armor w _) -> weight == w
                         _ -> False
                    )
                )

        Just (GW2.Weapon w) ->
            Dict.singleton w
                ( TagValue False
                    (\i ->
                    case i.iType.subType of
                        Just (GW2.Weapon w2) -> w == w2
                        _ -> False
                    )
                )
                   
        _ ->
            Dict.singleton it.iType.name ( TagValue False (\i -> i.iType.name == it.iType.name) )


viewItem : GW2.ItemSpec -> (ItemTags, Html Msg)
viewItem item =
    let
        tags = extractTags item
    in
        ( tags
        , div [ class "item has-popup" ]
            [ img [  style "height" "64px", style "margin" "2px", src (Maybe.withDefault "" item.iconUrl) ] []
            , div [ class "popup" ]
                 [ p [ class "itemname" ] [ text item.name ]
                 , p [ class "description" ] [ text (Maybe.withDefault "" item.description) ]
                 , p [ class "item-tags" ]
                     (Dict.foldr
                         (\s (TagValue _ f) l ->
                             a [ onClick (AddTag s (TagValue True f) ) ] [ text s ]
                             :: ( text " " )
                             :: l)
                         [] tags)
                 ]
            ]
        )


-- viewMissingItem : Int -> Html Msg
-- viewMissingItem id =
--     span [ style "height" "64px", style "margin" "2px" ] [
--         text (String.fromInt id)
--     ]


viewItems : Dict Int GW2.ItemSpec -> (GW2.ContainerItem -> Bool) -> List GW2.ContainerItem -> (ItemTags, Html Msg, Int)
viewItems specs filter allItems =
    let
        items = List.filter filter allItems

        (itemTags, itemHtml, n) = items
            |> List.foldr (\ { id } (t, l, n0) ->
                case Dict.get id specs of
                    Just spec ->
                        let
                            (newTags, html) = viewItem spec
                        in
                            (Dict.union newTags t, html :: l, n0 + 1)

                    Nothing ->
                        (t, (span [ class ("missing " ++ String.fromInt id) ] []) :: l, n0)
                )
                (Dict.empty, [], 0)
    in
        (itemTags, div [ class "container", style "line-height" "0" ] itemHtml, n)


viewBank : Dict Int GW2.ItemSpec -> (GW2.ContainerItem -> Bool) -> List GW2.ContainerItem -> (ItemTags, Html Msg)
viewBank specs filter bank =
    let
        (itemTags, items, n) = viewItems specs filter bank
    in   
        (itemTags,
            (case n of
                0 ->
                    div [] []

                _ ->
                    section [ class "section" ] [ div [class "container" ]
                        [h3 [ class "title is-3" ] [ text "Bank" ]
                        , items
                        ]
                    ]
            )
        )

 
viewCharacter : Dict Int GW2.ItemSpec -> (GW2.ContainerItem -> Bool) -> GW2.Character -> (ItemTags, Html Msg)
viewCharacter specs filter c =
    let
        (itemTags, bags, nb) = viewItems specs filter c.bags
    in
        (itemTags,
            (case nb of
                0 ->
                    div [] []

                _ ->
                    section [ class "section" ] [ div [class "container" ]
                        [ h3 [ class "title is-3" ] [ text ("Character: " ++ c.name) ]
                        , h4 [ class "title is-4" ] [ text "Bags" ]
                        , bags
                        ] ]
            )
        )


-- PORTS

port setStorage : JE.Value -> Cmd msg


-- FUNCTIONS


addRequest : ( String, JD.Decoder a) -> (Result Http.Error a -> Msg) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addRequest endpoint msg ( model, cmd ) =
    ( { model | loading = model.loading + 1 }
    , Cmd.batch [ cmd, GW2.apiRequest endpoint msg ]
    )


subRequest : Model -> Model
subRequest model =
    { model | loading = model.loading - 1 }


loadBank : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
loadBank ( model, cmd ) =
    addRequest
        (GW2.bankEndpoint model.apiKey)
        ReceivedBank
        ( model, cmd )


loadCharacters : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
loadCharacters ( model, cmd ) =
    addRequest
        (GW2.charactersEndpoint model.apiKey "all")
        ReceivedCharacters
        ( model, cmd )


loadItemNames : List GW2.ContainerItem -> Dict Int GW2.ItemSpec -> ( Model , Cmd Msg ) -> ( Model, Cmd Msg )
loadItemNames items specs mc =
    let
        unique_missing = items
            |> List.map .id
            |> Set.fromList
            |> Set.toList
            |> List.filter (\id -> Dict.member id specs |> not)

    in
        loadItemNamesChunk unique_missing mc


loadItemNamesChunk : List Int -> ( Model , Cmd Msg ) -> ( Model , Cmd Msg )
loadItemNamesChunk items mc =
    let
        chunk = List.take 200 items
        remainder = List.drop 200 items
    in
        case chunk of
            [] -> mc
            
            _ ->
                mc
                |> addRequest (GW2.itemsEndpoint chunk) ReceivedItemSpecs
                |> loadItemNamesChunk remainder



httpErrString : Http.Error -> String
httpErrString err =
    case err of
        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error"

        Http.BadUrl url ->
            "bad url: " ++ url

        Http.BadStatus status ->
            "bad response status: " ++ String.fromInt status

        Http.BadBody msg ->
            "error parsing JSON body: " ++ msg


-- returns true if the item matches, false otherwise
-- The shiny string is matched as case-insensitive substring.
itemFilter : ItemTags -> String -> Dict Int GW2.ItemSpec -> GW2.ContainerItem -> Bool
itemFilter tags shiny specs it =
    let
        found = Dict.get it.id specs

        matchTags ts sp =
            case ts of
                [] ->
                    True

                ( TagValue False _ ) :: rest ->
                    matchTags rest sp

                ( TagValue True f ) :: rest ->
                    case f sp of
                        False ->
                            False
                        True ->
                            matchTags rest sp

    in
        case found of
            Nothing ->
                False

            Just spec ->
                case matchTags (Dict.values tags) spec of
                    False ->
                        False
                    True ->
                        case shiny of
                            "" ->
                                True
                            _ ->
                                -- match the shiny name
                                String.contains shiny (String.toLower spec.name)


-- vim: et sw=4 ts=4 tw=105
