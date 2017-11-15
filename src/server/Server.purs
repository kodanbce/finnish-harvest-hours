module Server where

import Prelude hiding (apply)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (fromString)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.Exception (Error, error, message)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef', readRef, newRef)
import Node.Express.Types (EXPRESS)
-- import Node.Express.App (App, listenHttp, useOnError, get, use, setProp)
import Node.Express.App (App, get, listenHttp, setProp, useOnError)
import Node.Express.Handler (Handler, nextThrow, next)
import Node.Express.Request (getRouteParam, getQueryParam, getOriginalUrl,
                             setUserData, getUserData)
import Node.Express.Response (sendJson, setStatus)
import Node.HTTP (Server())
import Node.Process (PROCESS, lookupEnv)

type AppStateData = {}
type AppState     = Ref AppStateData

appSetup :: forall e. AppState -> App (ref :: REF, console :: CONSOLE | e)
appSetup state = do
  liftEff $ log "Setting up"
  setProp "json spaces" 4.0
  get "/"           (indexHandler      state)
  -- get "/login"      (loginHandler      state)
  -- get "/hello/:name" (updateTodoHandler state)
  useOnError        (errorHandler      state)

indexHandler :: forall e. AppState -> Handler e
indexHandler _ = do
  sendJson dummyData

dummyData :: { text :: String } 
dummyData = {
  text: "Hello."
}

errorHandler :: forall e. AppState -> Error -> Handler e
errorHandler state err = do
  setStatus 400
  sendJson {error: message err}

main :: forall e. Eff (ref :: REF, express :: EXPRESS, 
                       console :: CONSOLE, process :: PROCESS | e) 
                      Server
main = do
  state <- initState
  port <- (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
  listenHttp (appSetup state) port \_ ->
    log $ "Listening on " <> show port

initState :: forall e. Eff (ref :: REF|e) AppState
initState = newRef ({} :: AppStateData)

parseInt :: String -> Int
parseInt str = fromMaybe 0 $ fromString str