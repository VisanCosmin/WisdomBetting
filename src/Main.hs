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


import Data.Aeson.Schema
import Network.HTTP.Simple
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
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
import Control.Monad 
import Control.Concurrent.Thread.Delay
import Database.Persist.Sqlite
import Database.Persist
import Control.Monad.Reader (liftIO)
import Control.Concurrent
import Yesod
import Text.Printf
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T

import Database
import ApiFootball


data WisdomBetting = WisdomBetting ConnectionPool
mkYesod "WisdomBetting" [parseRoutes|
/ HomeR GET
/bet/#BetOption/#FixtureId AddBetR GET
/add/league/#Int64 AddLeagueR GET
|]
instance Yesod WisdomBetting where
     makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (24 * 60 * 60) 
        "client_session_key.aes"
instance YesodPersist WisdomBetting where
    type YesodPersistBackend WisdomBetting = SqlBackend
    runDB action = do
        WisdomBetting pool <- getYesod
        runSqlPool action pool

data BetOption = FtHome | FtDraw | FtAway | TgOver | TgUnder | BtsYes | BtsNo deriving (Eq , Show , Read , Ord)
data BetType = Fulltime | TotalGoals | Bts deriving (Eq , Show , Read , Ord)

instance PathPiece BetOption where
    toPathPiece x = T.pack $ show x
    fromPathPiece s =
        case reads $ T.unpack s of
            (i, ""):_ -> Just i
            [] -> Nothing

optionToType :: BetOption -> BetType
optionToType FtHome = Fulltime
optionToType FtDraw = Fulltime
optionToType FtAway = Fulltime
optionToType TgOver = TotalGoals
optionToType TgUnder = TotalGoals
optionToType BtsYes = Bts 
optionToType BtsNo = Bts

type UserSession = Map (FixtureId,BetType) BetOption
getUserSession :: Handler UserSession
getUserSession = do 
    maybeText <- lookupSession "session"
    case maybeText of 
        Nothing -> do 
            setSession "session" (pack.show $ (M.empty :: UserSession))
            return M.empty
        (Just text) -> do
            let string = unpack text 
            return $ read string

getAddBetR :: BetOption -> FixtureId -> Handler ()
getAddBetR betOption fixtureId = do 
    userSession <- getUserSession
    let userSession' = M.insert (fixtureId,optionToType betOption) betOption userSession
    setSession "session" (pack.show $ (userSession' :: UserSession))
    case betOption of 
        FtHome -> runDB $ update fixtureId [ FixtureFtHome +=. 1 ] 
        FtDraw -> runDB $ update fixtureId [ FixtureFtDraw +=. 1 ] 
        FtAway -> runDB $ update fixtureId [ FixtureFtAway +=. 1 ] 
        TgOver -> runDB $ update fixtureId [ FixtureTgOver +=. 1 ] 
        TgUnder -> runDB $ update fixtureId [ FixtureTgUnder +=. 1 ] 
        BtsYes -> runDB $ update fixtureId [ FixtureBtsYes +=. 1 ] 
        BtsNo -> runDB $ update fixtureId [ FixtureBtsNo +=. 1 ] 
    redirect HomeR

getHomeR :: Handler Html
getHomeR = do 
    userSession <- getUserSession
    now <- liftIO getCurrentTime
    let today = formatTime defaultTimeLocale "%d %b %Y" now
    leagues <- runDB getFixtures
    defaultLayout $ do 
        setTitle "Wisdom Betting"
        toWidgetHead [hamlet|
            <meta charset="utf-8"/>
            <link rel="stylesheet" href="https://wisdombetting.b-cdn.net/style.css">
            <link href="https://fonts.googleapis.com/css2?family=Raleway:wght@300&display=swap" rel="stylesheet"> 
            <link href="https://fonts.googleapis.com/css2?family=Montserrat&display=swap" rel="stylesheet"> 
        |]
        [whamlet|
            <nav>
                <div class="header">Wisdom Betting
                <div class="date">#{today}
            <main>
                $forall (league , fixtures) <- leagues
                    <h3 class="league-header">#{leagueName league}
                    $forall fixture <- fixtures
                        ^{fixtureWidget userSession fixture}


        |]

        toWidgetBody [julius|
            var elements = document.getElementsByClassName("hidden");

            var myFunction = function() {
                var betOption = this.getAttribute("data-option");
                var fixtureId = this.getAttribute("data-fixture");
                window.location.href = "http://localhost:3000/bet/" + betOption + "/"+fixtureId;
                console.log(attribute);
            };

            for (var i = 0; i < elements.length; i++) {
                elements[i].addEventListener('dblclick', myFunction, false);
            }

        |]

toStats :: Double -> Double -> (String , String)
toStats x 0 = ("0" , "0")
toStats 0 _ = ("0" , "0")
toStats x y = ( printf "%.2f" (y/x) , printf "%.1f" (100*x/y) )



fixtureWidget :: UserSession -> (Entity Fixture , Maybe FixtureScore) -> Widget 
fixtureWidget userSession (fixture@(Entity fixtureId Fixture{..}) , maybeScore) = 
    [whamlet|
        <div class="fixture">  
            <div class="spacer">
            <div class="teams">
                <span>#{fixtureHomeTeam}
                <span>#{fixtureAwayTeam}
            $maybe score <- maybeScore
                <div class="score">
                    <span>#{show (fixtureScoreHome score)}
                    <span>#{show (fixtureScoreAway score)}
            $nothing
                <div class="score">
                    <span>#{formatTime defaultTimeLocale "%R" fixtureDate}

            ^{fullTimeWidget fixture (M.lookup (fixtureId,Fulltime) userSession) maybeScore}
            <div class="spacer">
            ^{totalGoalsWidget fixture (M.lookup (fixtureId,TotalGoals) userSession) maybeScore}
    |]

ifFunction :: Bool -> Text -> Text -> Text
ifFunction True x _ = x
ifFunction False _ y = y

betResult :: BetOption -> FixtureScore -> Bool 
betResult FtHome FixtureScore{..} = fixtureScoreHome > fixtureScoreAway
betResult FtDraw FixtureScore{..} = fixtureScoreHome == fixtureScoreAway
betResult FtAway FixtureScore{..} = fixtureScoreHome < fixtureScoreAway
betResult TgOver FixtureScore{..} = fixtureScoreHome + fixtureScoreAway > 2 
betResult TgUnder FixtureScore{..} = fixtureScoreHome + fixtureScoreAway < 3 
betResult BtsYes FixtureScore{..} = fixtureScoreHome * fixtureScoreAway /= 0
betResult BtsNo FixtureScore{..} = fixtureScoreHome * fixtureScoreAway == 2 



maybeToBool :: Maybe a -> (a -> Bool) -> Bool 
maybeToBool Nothing _ = False
maybeToBool (Just a) f = f a 

fullTimeWidget :: Entity Fixture -> Maybe BetOption -> Maybe FixtureScore -> Widget
--fullTimeWidget fixture maybeSelection maybeResult 
fullTimeWidget (Entity fixtureId fixture) Nothing Nothing = 
    [whamlet|
        <div class="bet hidden" data-option="FtHome" data-fixture=#{show (fromSqlKey fixtureId)}>
            <span>Full Time 
            <span><b>#{fixtureHomeTeam fixture}</b>
        <div class="bet hidden" data-option="FtDraw" data-fixture=#{show (fromSqlKey fixtureId)}>
            <span>Full Time 
            <span><b>Draw</b>
        <div class="bet hidden" data-option="FtAway" data-fixture=#{show (fromSqlKey fixtureId)}>
            <span>Full Time 
            <span><b>#{fixtureAwayTeam fixture}</b>

    |]
fullTimeWidget (Entity fixtureId Fixture{..}) maybeOption maybeScore = 
    let ftSum = fromIntegral $ fixtureFtHome + fixtureFtDraw + fixtureFtAway
        (homeOdd , homePro) = toStats (fromIntegral fixtureFtHome) ftSum
        (drawOdd , drawPro) = toStats (fromIntegral fixtureFtDraw) ftSum
        (awayOdd , awayPro) = toStats (fromIntegral fixtureFtAway) ftSum
    in [whamlet|
        $with homeSelect <- ifFunction (maybeToBool maybeOption ((==) FtHome)) "selected" "" 
            $with homeResult <- ifFunction (maybeToBool maybeScore (betResult FtHome)) "result" ""
                <div class="bet #{homeSelect} #{homeResult} ">
                    <span>#{fixtureHomeTeam}
                    <span><b>#{homeOdd} / #{homePro}%</b>

        $with drawSelect <- ifFunction (maybeToBool maybeOption ((==) FtDraw)) "selected" "" 
            $with drawResult <- ifFunction (maybeToBool maybeScore (betResult FtDraw)) "result" ""
                <div class="bet #{drawSelect} #{drawResult} ">
                    <span>Draw
                    <span><b>#{drawOdd} / #{drawPro}%</b>

        $with awaySelect <- ifFunction (maybeToBool maybeOption ((==) FtAway)) "selected" "" 
            $with awayResult <- ifFunction (maybeToBool maybeScore (betResult FtAway)) "result" ""
               <div class="bet #{awaySelect} #{awayResult} ">
                    <span>#{fixtureAwayTeam}
                    <span><b>#{awayOdd} / #{awayPro}%</b>

    |]  

totalGoalsWidget :: Entity Fixture -> Maybe BetOption -> Maybe FixtureScore -> Widget
totalGoalsWidget (Entity fixtureId fixture) Nothing Nothing = 
    [whamlet|
        <div class="bet hidden" data-option="TgOver" data-fixture=#{show (fromSqlKey fixtureId)}>
            <span>Total Goals 
            <span><b>Over 2.5</b>
        <div class="bet hidden" data-option="TgUnder" data-fixture=#{show (fromSqlKey fixtureId)}>
            <span>Total Goals 
            <span><b>Under 2.5</b>

    |]
totalGoalsWidget (Entity fixtureId Fixture{..}) maybeOption maybeScore = 
    let tgSum = fromIntegral $ fixtureTgOver + fixtureTgUnder
        (overOdd , overPro) = toStats (fromIntegral fixtureTgOver) tgSum
        (underOdd , underPro) = toStats (fromIntegral fixtureTgUnder) tgSum
    in [whamlet|
        $with overSelect <- ifFunction (maybeToBool maybeOption ((==) TgOver)) "selected" "" 
            $with overResult <- ifFunction (maybeToBool maybeScore (betResult TgOver)) "result" ""
                <div class="bet #{overSelect} #{overResult} ">
                    <span>Over 2.5
                    <span><b>#{overOdd} / #{overPro}%</b>

        $with underSelect <- ifFunction (maybeToBool maybeOption ((==) TgUnder)) "selected" "" 
            $with underResult <- ifFunction (maybeToBool maybeScore (betResult TgUnder)) "result" ""
                <div class="bet #{underSelect} #{underResult} ">
                    <span>Under 2.5
                    <span><b>#{underOdd} / #{underPro}%</b>
    |]  


getAddLeagueR :: Int64 -> Handler Html
getAddLeagueR leagueId = do 
    Entity leagueKey league <- liftIO $ httpJsonObject (leagueRequest leagueId) parseLeague
    runDB $ insertKey leagueKey league
    redirect HomeR

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "test.db3" 10 $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
    -- Insert new fixtures
    forkIO $ forever $ do
        (flip runSqlPool) pool $ do
            today <- liftIO $ utctDay <$> getCurrentTime
            leagues <- selectList [ LeagueBegin <=. today
                                  , LeagueEnd >=. today 
                                  , LeagueLastUpdate <. today 
                                  ]
                                  []
            forM_ leagues $ \(Entity leagueId league) -> do
                fixtures <- liftIO $ httpJsonObject (fixturesRequest (fromSqlKey leagueId) today) parseFixturesInfo
                insertEntityMany fixtures
                update leagueId [ LeagueLastUpdate =. today ]
        delay (12*3600*1000000)

        -- Update the new fixtures
    forkIO $ forever $ do
        (flip runSqlPool) pool $ do
            today <- liftIO $ utctDay <$> getCurrentTime
            leagues <- selectList [ LeagueBegin <=. today
                                  , LeagueEnd >=. today 
                                  , LeagueLastUpdate ==. today 
                                  ]
                                  []
            forM_ leagues $ \(Entity leagueId league) -> do
                scores <- liftIO $ httpJsonObject (fixturesRequest (fromSqlKey leagueId) today) parseFixturesScore
                repsertMany scores
        delay (3600*1000000)

    warp 3000 $ WisdomBetting pool


    putStrLn "How are you ?"



