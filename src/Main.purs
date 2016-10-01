module Main where

import Prelude
import Control.Monad.Aff (Aff, makeAff, attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.Exception (Error, error, message)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef', readRef, newRef)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3)
import Data.Hash (gHash)
import Data.Ord (abs)
import Data.Maybe (Maybe(..))
import Node.Express.App (App, listenHttp, useOnError, get, useExternal, setProp)
import Node.Express.Handler (Handler, nextThrow)
import Node.Express.Request (getRouteParam)
import Node.Express.Response (sendJson, setStatus)
import Node.Express.Types (EXPRESS, ExpressM, Request, Response)
import Node.HTTP (Server())
import Node.FS (FS)
import Node.FS.Stream (createWriteStream)
import Node.ChildProcess (ChildProcess(), CHILD_PROCESS(), Exit(..))
import Node.ChildProcess as ChildProcess
import Node.Process (PROCESS)
import Node.Stream (Readable, pipe, onEnd, onError)

--- Model type definitions
type Video = { id :: String, url :: String, path :: String, state :: String}

-- Global state data
type AppStateData = Array Video
type AppState     = Ref AppStateData
type AppError     = String

-- FFI
-- Express middleware for CORS
foreign import cors :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)
-- Youtube downloader
foreign import data YTDL :: !
type Youtube a = forall e. Eff ( ytdl :: YTDL | e ) a
foreign import getInfo :: forall e. (Error -> Eff e Unit) -> (String -> Eff e Unit) -> String -> Youtube Unit
foreign import downloadFromInfo :: forall e. String -> (Youtube (Readable () e))

getInfo' :: forall e. String -> Aff ( ytdl :: YTDL | e ) String
getInfo' url = makeAff (\error success -> getInfo error success url)

initState :: forall e. Eff (ref :: REF| e) AppState
initState = newRef ([] :: AppStateData)

sliceYT
  :: forall e.
  String
  -> String
  -> Eff ( cp :: CHILD_PROCESS, console :: CONSOLE | e ) ChildProcess
sliceYT path hash = do
  liftEff $ log "Slicing video\n"
  ff <- ChildProcess.spawn
                    "ffmpeg"
                    ["-i", path, "-vf", "scale=128:128", ("images/" <> hash <> "-%03d.png")]
                    ChildProcess.defaultSpawnOptions { stdio = ChildProcess.inherit }
    -- Log exit code
  ChildProcess.onExit ff (\ex ->
    case ex of
         (Normally i) -> if (i == 0)
                            then log "\nDone slicing!"
                            else log "\nSomething went wrong - check ffmpeg output"
         (BySignal s) -> log $ "\nProcess stopped by signal: " <> (show s))
  pure ff

errorHandler :: forall e.  AppState -> Error -> Handler e
errorHandler state err = do
  setStatus 400
  sendJson $ message err

indexHandler :: forall e. AppState -> Handler e
indexHandler _ = do
  sendJson "Hello"

submitHandler
  :: forall e.
  AppState
  -> Handler ( fs :: FS , ytdl :: YTDL, cp :: CHILD_PROCESS , console :: CONSOLE | e )
submitHandler state = do
  urlParam <- getRouteParam "url"
  case urlParam of
       (Just url) -> do
          info <- liftAff $ attempt $ getInfo' url
          case info of
              (Right i) -> do
                  let hash = (show <<< abs <<< gHash) url
                      path = "videos/" <> hash <> ".flv"
                  yt <- liftEff $ downloadFromInfo i
                  fs <- liftEff $ createWriteStream path
                  liftEff $ yt `pipe` fs
                  liftEff $ onError yt (\e -> log $ show e)
                  liftEff $ onEnd yt $ void $ sliceYT path hash
                  sendJson {id: hash, path: path, state: "processing", url: url}
              (Left err) -> nextThrow err
       _ -> nextThrow $ error "Url parameter is required"

appSetup
  :: forall e.
  AppState
  -> App ( fs :: FS
         , ref :: REF
         , cp :: CHILD_PROCESS
         , ytdl :: YTDL
         , console :: CONSOLE | e )
appSetup state = do
  liftEff $ log "Setting up"
  setProp "json spaces" 4.0
  useExternal cors
  get "/"             (indexHandler  state)
  get "/submit/:url"  (submitHandler state)
  useOnError          (errorHandler  state)

main
  :: forall e.
  Eff ( fs :: FS
      , ref :: REF
      , express :: EXPRESS
      , console :: CONSOLE
      , cp :: CHILD_PROCESS
      , ytdl :: YTDL
      , process :: PROCESS | e )
  Server
main = do
  state <- initState
  let port = 8080
  listenHttp (appSetup state) port \_ ->
    log $ "Listening on " <> show port
