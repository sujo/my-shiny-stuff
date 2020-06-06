module GuildWars2 exposing (..)

import Http
import Url.Builder exposing (absolute, int, string)
import Json.Decode as JD
import Dict exposing (Dict)


-- Types

type alias ContainerItem =
    { id: Int
    , count: Int
    , binding : Maybe String
    , boundTo : Maybe String
    }

type alias Character =
    { name: String
    , bags: (List ContainerItem)
    , equipmentTabs : (List ContainerItem)
    }

type ItemSubType
    = Armor String String -- weight, slot
    | Weapon String

type alias ItemType =
    { name : String
    , subType : Maybe ItemSubType
    }

type alias ItemSpec =
    { id : Int
    , name : String
    , iconUrl : Maybe String
    , description : Maybe String
    , iType : ItemType
    , statsChoices : List String
    }

type alias ItemStats = Dict Int String


-- Functions

guildIdEndpoint : String -> ( String, JD.Decoder (List String) )
guildIdEndpoint name =
    ( absolute [ "v2", "guild", "search" ] [ string "name" name ]
    , JD.list JD.string
    )


characterNamesEndpoint : String -> ( String, JD.Decoder (List String) )
characterNamesEndpoint apiKey =
    ( absolute [ "v2", "characters" ] [ string "access_token" apiKey ]
    , JD.list JD.string
    )

charactersEndpoint : String -> String -> ( String, JD.Decoder (List Character) )
charactersEndpoint apiKey name =
    ( absolute [ "v2", "characters" ] [ string "ids" name, string "access_token" apiKey ]
    , JD.list characterD
    )


itemTypeD : JD.Decoder ItemType
itemTypeD =
    let
        typeD = JD.field "type" JD.string
    in
        typeD |> JD.andThen (\ t ->
            case t of
                "Armor" ->
                    JD.map (ItemType t)
                        (JD.map Just
                            (JD.field "details"
                                (JD.map2 Armor
                                    (JD.field "weight_class" JD.string)
                                    (JD.field "type" JD.string)
                                )
                            )
                        )
                    
                "Weapon" ->
                    JD.map (ItemType t)
                        (JD.map Just
                            (JD.map Weapon
                                (JD.field "details"
                                    (JD.field "type" JD.string)
                                )
                            )
                        )

                _ ->
                    JD.map (ItemType t) (JD.succeed Nothing)
            )


itemsEndpoint : List Int -> ItemStats -> (String, JD.Decoder (List ItemSpec))
itemsEndpoint ids stats =
    ( absolute [ "v2", "items" ] [ ids
        |> List.map String.fromInt
        |> String.join ","
        |> string "ids" ]
    , JD.list (JD.map6 ItemSpec
        (JD.field "id" JD.int)
        (JD.field "name" JD.string)
        (JD.maybe (JD.field "icon" JD.string) )
        (JD.maybe (JD.field "description" JD.string) )
        itemTypeD
        (JD.map
            (List.filterMap (\id -> Dict.get id stats))
            (JD.map2 (\inf ch ->
                case (inf, ch) of
                    (Nothing, Nothing) -> []
                    (Nothing, Just l) -> l
                    (Just s, Nothing) -> [s]
                    (Just s, Just l) -> s::l
                )
                (JD.maybe
                    (JD.field "infix_upgrade"
                        (JD.field "id" JD.int)
                    )
                )
                (JD.maybe
                    (JD.field "stat_choices" (JD.list JD.int))
                )
            )
        )
    ) )


itemStatsD : JD.Decoder (Dict Int String)
itemStatsD =
    JD.map Dict.fromList
        ( JD.list
            (JD.map2 Tuple.pair
                (JD.field "id" JD.int)
                (JD.field "name" JD.string)
            )
        )


itemStatsEndpoint : (String, JD.Decoder ItemStats)
itemStatsEndpoint =
    ( absolute [ "v2", "itemstats" ] [ string "ids" "all" ]
    , itemStatsD
    )


containerItemD : JD.Decoder ContainerItem
containerItemD =
    JD.map4 ContainerItem
    (JD.field "id" JD.int)
    (JD.field "count" JD.int)
    (JD.maybe (JD.field "binding" JD.string) )
    (JD.maybe (JD.field "bound_to" JD.string) )


containerD : JD.Decoder (List ContainerItem)
containerD =
    JD.map
        (List.filterMap (\a->a))
        (JD.list (JD.nullable containerItemD))


bankEndpoint : String -> (String, JD.Decoder (List ContainerItem) )
bankEndpoint apiKey =
    ( absolute [ "v2", "account", "bank" ] [ string "access_token" apiKey ]
    , containerD
    )


sharedInventoryEndpoint : String -> (String, JD.Decoder (List ContainerItem) )
sharedInventoryEndpoint apiKey =
    ( absolute [ "v2", "account", "inventory" ] [ string "access_token" apiKey ]
    , containerD
    )


characterD : JD.Decoder Character
characterD =
    JD.map3 Character
        (JD.field "name" JD.string)
        ( containerD
        |> JD.field "inventory"
        |> JD.list
        |> JD.map List.concat
        |> JD.field "bags"
        )
        (JD.succeed []) -- equipmentTabs, not filled here


-- parses one equipment tab
equipmentTabD : JD.Decoder (List ContainerItem)
equipmentTabD =
    JD.list
        ( JD.map4 ContainerItem
            (JD.field "id" JD.int)
            (JD.succeed 1) -- count
            (JD.maybe (JD.field "binding" JD.string) )
            (JD.maybe (JD.field "bound_to" JD.string) )
        )


equipmentEndpoint : String -> String -> ( String, JD.Decoder (List ContainerItem) )
equipmentEndpoint apiKey name =
    ( absolute [ "v2", "characters", name, "equipmenttabs" ] [ string "tabs" "all", string "access_token" apiKey ]
    , ( JD.list
            ( JD.field "equipment" equipmentTabD )
        ) |> JD.map List.concat
    )


apiRequest : ( String, JD.Decoder a) -> (Result Http.Error a -> msg) -> Cmd msg
apiRequest ( endpoint, expect ) msg =
    { method = "GET"
    , url = "https://api.guildwars2.com" ++ endpoint
    -- Preflighted requests are not supported by the GW2 API server as of 2020-05-24.
    -- Instead, requests that require authentication must send the API key in the query parameter
    -- "access_token".
    --, headers = [ Http.header "Authorization" ("Bearer " ++ apiKey)]
    , headers = []
    , body = Http.emptyBody
    , expect = Http.expectJson msg expect
    , timeout = Just 10000.0
    , tracker = Nothing
    }
    |> Http.request


-- vim: et sw=4 ts=4 tw=105
