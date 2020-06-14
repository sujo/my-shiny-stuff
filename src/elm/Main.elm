port module Main exposing (main)

import Html exposing (Html, div, text, img, h3, h4, section, button, span, a, p, i)
import Html.Attributes exposing (class, src, style, href)
import Html.Events exposing (onClick)
import Browser exposing (document)
import Http
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Set exposing (Set)

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

type Page
    = Configure
    | ShowItems


type alias StatsChoices = Dict Int GW2.ItemStats

type alias ItemSpec = GW2.ItemSpec Msg

-- Tag name isActive itemFilter
type ItemTagValue 
   = TagValue Bool (ItemSpec -> Bool)
   | StatsTagValue Bool -- single stat filter by the name of the tag, e.g. "Power"

type alias ItemTags = Dict String ItemTagValue

-- The FilterFunction return true if the item matches, and a map that contains the additional stats names
-- for matching statsChoices.
type alias FilterFunction = (GW2.ContainerItem -> (Bool, Set String))

type alias Model =
    { page : Page
    , apiKey : String
    , errors : List String
    , showInfo : Bool
    , bank : List GW2.ContainerItem
    , sharedInventory : List GW2.ContainerItem
    , characters : Dict String GW2.Character
    , itemSpecs : Dict Int ItemSpec
    , itemStatsMap : Dict Int GW2.ItemStats
    , loading : Int
    , shiny : String
    , itemTags : ItemTags
    , statsChoices : StatsChoices -- the possible itemStats matching the active item tags
    }


init : JD.Value -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { page = Configure
            , apiKey = ""
            , errors = []
            , showInfo = False
            , bank = []
            , sharedInventory = []
            , characters = Dict.empty
            , itemSpecs = Dict.empty
            , itemStatsMap = Dict.empty
            , loading = 0
            , shiny = ""
            , itemTags = Dict.empty
            , statsChoices = Dict.empty
            }
    in
        case JD.decodeValue (JD.field "apiKey" JD.string) flags of
            Ok apiKey_ ->
                ( { model | page = ShowItems, apiKey = apiKey_ } , Cmd.none )
                |> loadItemStats

            Err _ ->
                ( model , Cmd.none )
                |> loadItemStats



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
    | ReceivedItemStats (Result Http.Error (Dict Int GW2.ItemStats) )
    | ReceivedBank (Result Http.Error (List GW2.ContainerItem) )
    | ReceivedSharedInventory (Result Http.Error (List GW2.ContainerItem) )
    | ReceivedItemSpecs (Result Http.Error (List ItemSpec) )
    | ReceivedCharacters (Result Http.Error (List GW2.Character) )
    | ReceivedEquipment String (Result Http.Error (List GW2.ContainerItem) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        -- For each StatsTagValue, filter the remaining statsChoices.
        filterStatsChoices : ItemTags -> StatsChoices -> StatsChoices
        filterStatsChoices itemTags statsChoices
            = Dict.foldr (\stat tag scs ->
                case tag of
                    StatsTagValue True ->
                        Dict.filter
                            (\_ { stats } -> List.member stat stats)
                            scs

                    _ -> scs
                )
                statsChoices
                itemTags

    in
        case msg of
            ApiKeyInput s ->
                ( { model | apiKey = s }, Cmd.none )

            ShinyInput s ->
                ( { model | shiny = String.toLower s }, Cmd.none )

            GoConfigure ->
                ( { model | page = Configure }, Cmd.none )

            SaveConfig ->
                ( { model | page = ShowItems, errors = [] }
                , setStorage (JE.object [ ( "apiKey", JE.string model.apiKey ) ] )
                )
                |> loadBank
                |> loadSharedInventory
                |> loadCharacters

            NoSaveConfig ->
                ( { model | page = ShowItems }, Cmd.none )

            ShowInfo ->
                ( { model | showInfo = True }, Cmd.none )

            HideInfo ->
                ( { model | showInfo = False }, Cmd.none )

            AddTag name t ->
                let
                    itemTags = Dict.insert name t model.itemTags

                    statsChoices =
                        case t of
                            StatsTagValue True ->
                                filterStatsChoices itemTags model.itemStatsMap

                            _ ->
                                model.statsChoices

                in
                    ( { model
                        | itemTags = itemTags
                        , statsChoices = statsChoices
                        } , Cmd.none)

            RemoveTag s ->
                let
                    tag = Dict.get s model.itemTags

                    itemTags = Dict.remove s model.itemTags

                    statsChoices =
                        case tag of
                            Just (StatsTagValue True) ->
                                case hasActiveStatsTagValues itemTags of
                                    False ->
                                        model.itemStatsMap

                                    True ->
                                        filterStatsChoices itemTags model.itemStatsMap
                                
                            _ ->
                                -- no relevant change to the tags, so we don't need to update the statsChoices
                                model.statsChoices

                in
                    ( { model
                        | itemTags = itemTags
                        , statsChoices = statsChoices
                        } , Cmd.none)

            ClearErrors ->
                ( { model | errors = [] }, Cmd.none )

            ReceivedItemStats (Err err) ->
                ( { model 
                      | errors = ( "Error loading item stats from the GW2 API: "
                          ++ httpErrString err)
                          :: model.errors
                  }
                  |> subRequest
                , Cmd.none )

            ReceivedItemStats (Ok itemStatsMap) ->
                let
                    mc =
                        ( { model
                            | itemStatsMap = itemStatsMap
                            , statsChoices = itemStatsMap
                            } |> subRequest
                        , Cmd.none
                        )
                in
                    case model.page of
                        Configure ->
                            mc

                        ShowItems ->
                            mc
                            |> loadBank
                            |> loadSharedInventory
                            |> loadCharacters


            ReceivedBank (Err (Http.BadStatus 401)) ->
                -- This happens when the API key is invalid or does not have sufficient permissions.
                ( { model
                    | page = Configure
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
                    |> loadItemNames bankSlots model.itemSpecs

            ReceivedSharedInventory (Err (Http.BadStatus 401)) ->
                -- This happens when the API key is invalid or does not have sufficient permissions.
                ( { model
                    | page = Configure
                    , errors = [ """Error 401 from the GW2 API while loading the shared inventory.
    Please check if the API key is valid and has inventory permissions.""" ]
                  } |> subRequest
                  , Cmd.none )

            ReceivedSharedInventory (Err err) ->
                ( { model 
                      | errors = ( "Error loading the shared inventory from the GW2 API: "
                          ++ httpErrString err)
                          :: model.errors
                  }
                  |> subRequest
                , Cmd.none )

            ReceivedSharedInventory (Ok invSlots) ->
                    ( { model | sharedInventory = invSlots } |> subRequest, Cmd.none )
                    |> loadItemNames invSlots model.itemSpecs

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
                    itemSpecs
                         = specs
                        |> List.map (\a -> (a.id, a) )
                        |> Dict.fromList
                        |> Dict.union model.itemSpecs

                in
                    ( { model
                        | itemSpecs = itemSpecs
                        }
                        |> subRequest
                     , Cmd.none )

            ReceivedCharacters (Err (Http.BadStatus 401)) ->
                -- This happens when the API key is invalid or does not have sufficient permissions.
                ( { model
                    | page = Configure
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

                    newmodel = { model | characters =
                            characters
                            |> List.map (\ ch -> ( ch.name, ch ) )
                            |> Dict.fromList
                        }
                        |> subRequest

                    cnames = List.map .name characters
                in
                    List.foldl loadEquipment ( newmodel, Cmd.none ) cnames
                    |> loadItemNames items model.itemSpecs
                    
            ReceivedEquipment _ (Err (Http.BadStatus 401)) ->
                -- This happens when the API key is invalid or does not have sufficient permissions.
                ( { model
                    | page = Configure
                    , errors = [ """Error 401 from the GW2 API while loading equipment tabs.
    Please check if the API key is valid and has character permissions.""" ]
                  } |> subRequest
                  , Cmd.none )

            ReceivedEquipment _ (Err err) ->
                ( { model 
                      | errors = ( "Error loading equipment tabs from the GW2 API: "
                          ++ httpErrString err)
                          :: model.errors
                  }
                  |> subRequest
                , Cmd.none )

            ReceivedEquipment name (Ok equip) ->
                case Dict.get name model.characters of
                    Nothing ->
                        ( model |> subRequest, Cmd.none )

                    Just oldChar ->
                        let
                            char = { oldChar | equipmentTabs = equip }
                        in
                            ( { model | characters = Dict.insert name char model.characters }
                                |> subRequest
                            , Cmd.none )
                            |> loadItemNames equip model.itemSpecs



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> { title : String, body : List (Html Msg) }
view model =
    let
        title = "My Shiny Stuff"

        filter = itemFilter (Dict.toList model.itemTags) (hasActiveStatsTagValues model.itemTags) model.shiny model.itemSpecs model.statsChoices

        (itemTagsBank, htmlBank) = viewContainer "Bank" model.itemSpecs filter model.bank

        (itemTagsShared, htmlShared) = viewContainer "Shared Inventory" model.itemSpecs filter model.sharedInventory

        (itemTagsChars, htmlChars) = 
            List.foldr
                (\c (ts0, hl0) ->
                    let
                        (ts, hl) = viewCharacter model.itemSpecs filter c
                    in
                        (Dict.union ts ts0, hl :: hl0)
                )
                (Dict.empty, [])
                (Dict.values model.characters)

        tags = Dict.union itemTagsBank itemTagsChars
            |> Dict.union itemTagsShared
            |> Dict.union model.itemTags

        content = 
            case model.page of

                Configure ->
                    viewConfigure model

                ShowItems ->
                    viewControls model.shiny tags (model.loading > 0)
                    :: htmlBank
                    :: htmlShared
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
                    (\t tagValue l ->
                        case tagValue of
                            StatsTagValue True ->
                                (a [ class "tag active is-success", onClick (RemoveTag t) ] [ text t ]) :: l
                            StatsTagValue False ->
                                (a [ class "tag inactive is-success is-light", onClick (AddTag t (StatsTagValue True)) ] [ text t ]) :: l
                            TagValue True _ ->
                                (a [ class "tag active is-info", onClick (RemoveTag t) ] [ text t ]) :: l
                            TagValue False f ->
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


viewItem : ItemSpec -> (ItemTags, Html Msg)
viewItem item =
    let
        tags =
            case item.iType.subType of
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
                    Dict.singleton item.iType.name ( TagValue False (\i -> i.iType.name == item.iType.name) )

    in
        ( tags
        , div [ class "item has-popup" ]
            [ img [  style "height" "64px", style "margin" "2px", src (Maybe.withDefault "" item.iconUrl) ] []
            , div [ class "popup" ]
                 [ p [ class "itemname" ] [ text item.name ]
                 , p [ class "description" ] item.description
                 , p [ class "item-tags" ]
                     (Dict.foldr
                         (\s tagValue l ->
                             case tagValue of
                                 StatsTagValue _ ->
                                     a [ class "is-success", onClick (AddTag s (StatsTagValue True) ) ] [ text s ]
                                     :: ( text " " )
                                     :: l
                                 TagValue _ f ->
                                     a [ class "is-info", onClick (AddTag s (TagValue True f) ) ] [ text s ]
                                     :: ( text " " )
                                     :: l
                         )
                         [] tags)
                 ]
            ]
        )


-- viewMissingItem : Int -> Html Msg
-- viewMissingItem id =
--     span [ style "height" "64px", style "margin" "2px" ] [
--         text (String.fromInt id)
--     ]


-- Returns a tuple of
--   possible ItemTags
--   the HTML code for displaying the item
--   the number of items in this container that match the filter
viewItems : Dict Int ItemSpec -> FilterFunction -> (Html Msg -> Html Msg) -> List GW2.ContainerItem -> (ItemTags, Html Msg, Int)
viewItems specs filter wrapper allItems =
    let
        (items, statNames) =
            List.foldr
                (\ it ((items0, sn0) as x0) ->
                    case filter it of
                        ( False, _ ) ->
                            x0

                        ( True, sn ) ->
                            ( it :: items0, Set.union sn0 sn)
                )
                ( [], Set.empty )
                allItems

        (itemTags1, itemHtml, n) = items
            |> List.foldr (\ { id } (t, l, n0) ->
                case Dict.get id specs of
                    Just spec ->
                        let
                            (newTags, h) = viewItem spec
                        in
                            (Dict.union newTags t, h:: l, n0 + 1)

                    Nothing ->
                        (t, (span [ class ("missing " ++ String.fromInt id) ] []) :: l, n0)
                )
                (Dict.empty, [], 0)

        html = case n of
            0 -> div [] []
            _ ->
                wrapper
                ( div [ class "container", style "line-height" "0" ] itemHtml )

        itemTags
            = statNames
            |> Set.toList
            |> List.map (\ s -> ( s, StatsTagValue False ) )
            |> Dict.fromList
            |> Dict.union itemTags1

    in
        (itemTags, html, n)


viewContainer : String -> Dict Int ItemSpec -> FilterFunction -> List GW2.ContainerItem -> (ItemTags, Html Msg)
viewContainer title specs filter container =
    let
        wrapper items =
            section [ class "section" ]
            [ div [class "container" ]
                [ h3 [ class "title is-3" ] [ text title ]
                , items
                ]
            ]

        (itemTags, html, _) = viewItems specs filter wrapper container
    in   
        (itemTags, html)

 
viewCharacter : Dict Int ItemSpec -> FilterFunction -> GW2.Character -> (ItemTags, Html Msg)
viewCharacter specs filter c =
    let
        wrapper title items =
            section [ class "section" ]
                [ div [class "container" ]
                    [ h4 [ class "title is-4" ] [ text title ]
                    , items
                    ]
                ]

        (itemTags, bagsHtml, nb) = viewItems specs filter (wrapper "Bags") c.bags

        (equipTags, equipHtml, ne) = viewItems specs filter (wrapper "Equipment") c.equipmentTabs

        tags = Dict.union itemTags equipTags
    in
        (tags,
            (case nb + ne of
                0 ->
                    div [] []

                _ ->
                    section [ class "section" ] [ div [class "container" ]
                        [ h3 [ class "title is-3" ] [ text ("Character: " ++ c.name) ]
                        , equipHtml
                        , bagsHtml
                        ]
                    ]
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


loadSharedInventory : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
loadSharedInventory ( model, cmd ) =
    addRequest
        (GW2.sharedInventoryEndpoint model.apiKey)
        ReceivedSharedInventory
        ( model, cmd )


loadCharacters : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
loadCharacters ( model, cmd ) =
    addRequest
        (GW2.charactersEndpoint model.apiKey "all")
        ReceivedCharacters
        ( model, cmd )


loadEquipment : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
loadEquipment name ( model, cmd ) =
    addRequest
        (GW2.equipmentEndpoint model.apiKey name)
        ( ReceivedEquipment name )
        ( model, cmd )


loadItemNames : List GW2.ContainerItem -> Dict Int ItemSpec -> ( Model , Cmd Msg ) -> ( Model, Cmd Msg )
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
loadItemNamesChunk items (model, cmd) =
    let
        chunk = List.take 200 items
        remainder = List.drop 200 items
    in
        case chunk of
            [] -> (model, cmd)
            
            _ ->
                (model, cmd)
                |> addRequest (GW2.itemsEndpoint chunk model.itemStatsMap) ReceivedItemSpecs
                |> loadItemNamesChunk remainder


loadItemStats : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
loadItemStats mc =
    addRequest
        GW2.itemStatsEndpoint
        ReceivedItemStats
        mc


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
itemFilter : List (String, ItemTagValue) -> Bool -> String -> Dict Int ItemSpec -> StatsChoices -> GW2.ContainerItem
        -> (Bool, Set String)
itemFilter tags haveStatsTags shiny specs statsChoices it =
    let
        found = Dict.get it.id specs

        matchTags ts sp =
            case ts of
                [] ->
                    True

                -- no-op
                ( _, StatsTagValue _ ) :: rest ->
                    matchTags rest sp

                ( _, TagValue False _ ) :: rest ->
                    matchTags rest sp

                ( _, TagValue True f ) :: rest ->
                    case f sp of
                        False ->
                            False
                        True ->
                            matchTags rest sp

        matchStats : StatsChoices -> List Int -> ( Bool, Set String )
        matchStats choices sp =
           -- Find matching ItemStats, then extract the other stat names.
           sp
           |> List.filterMap (\ id -> Dict.get id statsChoices )
           |> List.foldr (\ { stats } ( _, other ) ->
                      ( True , Set.union other (Set.fromList stats) )
                  )
                  ( not haveStatsTags, Set.empty )

    in

        case found of
            Nothing ->
                ( False, Set.empty )

            Just spec ->
                case
                    ( String.contains shiny (String.toLower spec.name)
                    && matchTags tags spec
                    )
                of
                    False ->
                        ( False, Set.empty )

                    True ->
                        matchStats statsChoices spec.statsChoices


hasActiveStatsTagValues : ItemTags -> Bool
hasActiveStatsTagValues
    = Dict.values >> List.any (\ t ->
        case t of
            StatsTagValue True -> True
            _ -> False
        )


-- vim: et sw=4 ts=4 tw=105
