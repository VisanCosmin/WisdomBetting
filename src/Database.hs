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
module Database where


import Database.Persist.Sqlite
import Database.Persist
import Database.Persist.TH
import Control.Monad.Reader (ReaderT , MonadIO , liftIO)
import Control.Monad (forM)

import Data.Time 
import Data.Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
League
    name Text 
    country Text
    begin Day
    end Day
    lastUpdate Day

Fixture
    homeTeam Text
    awayTeam Text 
    date UTCTime
    league LeagueId
    ftHome Int 
    ftDraw Int 
    ftAway Int 
    tgOver Int 
    tgUnder Int 
    btsYes Int 
    btsNo Int
    deriving Show

FixtureScore
    home Int 
    away Int 
|]


getFixturesByLeague :: (MonadIO m) => LeagueId -> Day -> ReaderT SqlBackend m [(Entity Fixture,Maybe FixtureScore)]
getFixturesByLeague league today = do 
    let today' = UTCTime today 0
        today'' = UTCTime (addDays 1 today) 0
    fixtures <- selectList [ FixtureLeague ==. league 
                           , FixtureDate >=. today'
                           , FixtureDate <=. today''
                           ]
                           [ Asc FixtureDate ]
    forM fixtures $ \(Entity fixtureId fixture) -> do
        maybeScore <- get (toSqlKey.fromSqlKey $ fixtureId) 
        return (Entity fixtureId fixture , maybeScore)


getFixtures :: (MonadIO m) => ReaderT SqlBackend m [(League,[(Entity Fixture,Maybe FixtureScore)])]
getFixtures = do 
    today <- liftIO (utctDay <$> getCurrentTime)
    leagues <- selectList [ LeagueBegin <=. today
                          , LeagueEnd >=. today
                          , LeagueLastUpdate ==. today 
                          ]
                          []
    forM leagues $ \(Entity leagueId league) -> do 
        fixtures <- getFixturesByLeague leagueId today
        return (league,fixtures)


