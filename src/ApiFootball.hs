{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE RecordWildCards            #-}
module ApiFootball where

import Data.Aeson (FromJSON)
import Data.Aeson.Schema
import Network.HTTP.Simple
import Data.Time
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text , pack , unpack)
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Data.List (concat)
import Control.Monad (forM_)
import Database.Persist (Entity(..))
import Database.Persist.Sqlite (toSqlKey)


import Database



httpJsonObject request parser = do 
    response <- httpJSON request
    let body = getResponseBody response
    return $ parser body


(|>) = flip ($)
fixturesRequest :: Int64 -> Day -> Request 
fixturesRequest leagueId day = 
    (parseRequestThrow_ "https://v2.api-football.com/") 
        |> setRequestMethod "POST"
        |> setRequestPath (encodeUtf8 $ "fixtures/league/" <> (pack.show $ leagueId) <> "/" <> (pack.showGregorian $ day))
        |> addRequestHeader "X-RapidAPI-Key" "b2614e1ffac6017a0b67ae39e0104a1b"

type FixturesSchema = [schema|
  {
    api: {
        fixtures: List {
            fixture_id: Int64 ,
            league_id: LeagueId ,
            event_date: UTCTime ,
            statusShort: Text ,
            homeTeam: {
                team_name: Text 
            } ,
            awayTeam: {
                team_name: Text 
            } , 
            goalsHomeTeam: Maybe Int ,
            goalsAwayTeam: Maybe Int 
        }
    }

  }
|]


leagueRequest :: Int64 -> Request
leagueRequest leagueId = 
    (parseRequestThrow_ "https://v2.api-football.com/") 
        |> setRequestMethod "POST"
        |> setRequestPath (encodeUtf8 $ "leagues/league/" <> (pack.show $ leagueId))
        |> addRequestHeader "X-RapidAPI-Key" "b2614e1ffac6017a0b67ae39e0104a1b"
parseFixturesInfo :: Object FixturesSchema -> [Entity Fixture]
parseFixturesInfo obj = concat $ (flip map) [get| obj.api.fixtures |] $ \fixture -> 
    if [get| fixture.statusShort |] == "NS" 
        then [  Entity  (toSqlKey [get| fixture.fixture_id |])
                        Fixture {
                            fixtureHomeTeam = [get| fixture.homeTeam.team_name |] ,
                            fixtureAwayTeam = [get| fixture.awayTeam.team_name |] ,
                            fixtureDate = [get| fixture.event_date |] ,
                            fixtureLeague = [get| fixture.league_id |] ,
                            fixtureFtHome = 0 ,
                            fixtureFtDraw = 0 ,
                            fixtureFtAway = 0 ,
                            fixtureTgOver = 0 ,
                            fixtureTgUnder = 0 ,
                            fixtureBtsYes = 0 ,
                            fixtureBtsNo = 0
                        }
            ]
        else []

parseFixturesScore :: Object FixturesSchema -> [(Key FixtureScore , FixtureScore)]
parseFixturesScore obj = concat $ (flip map) [get| obj.api.fixtures |] $ \fixture -> 
    if [get| fixture.statusShort |] == "FT" 
        then [  ( toSqlKey [get| fixture.fixture_id |] 
                , FixtureScore {
                    fixtureScoreHome = fromJust $ [get| fixture.goalsHomeTeam |] ,
                    fixtureScoreAway = fromJust $ [get| fixture.goalsAwayTeam |]
                  }
                )
            ]
        else []



type LeagueSchema = [schema|
  {
    api: {
        leagues: List {
            league_id: LeagueId ,
            name: Text ,
            country: Text ,
            season_start: Day ,
            season_end: Day
        }
    }

  }
|]

parseLeague :: Object LeagueSchema -> Entity League 
parseLeague obj = 
    let league = head [get| obj.api.leagues |]
    in Entity [get| league.league_id |]
              League {
                leagueName = [get| league.name |] ,
                leagueCountry = [get| league.country |] ,
                leagueBegin = [get| league.season_start |] ,
                leagueEnd = [get| league.season_end |] ,
                leagueLastUpdate = [get| league.season_start |]
              }  







