{-# LANGUAGE OverloadedStrings #-}
{- vim: set foldmethod=marker : -}

module Main where

-- Imports ------------------------------------------------------------{{{
import           Control.Monad          (liftM, when, join)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Except   (liftIO)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Text              ( pack
                                        , unpack
                                        , Text )
import qualified Data.Text as T         (concat)
import           Data.Function          (on)
import           Data.Maybe             (fromMaybe)
import           Data.List              (nub)
import           Data.XML.Types         (elementText, Element)
import           Data.ConfigFile        ( readfile
                                        , get
                                        , emptyCP)
import           Data.ConfigFile.Types  (CPError)
import           Network.Xmpp           ( pullMessage
                                        , sendMessage
                                        , jidFromText
                                        , def
                                        , sendPresence
                                        , scramSha1
                                        , session
                                        , messagePayload
                                        , Message )
import           Network.Xmpp.IM        (simpleIM)
import           System.Log.Logger      ( updateGlobalLogger
                                        , setLevel
                                        , Priority(DEBUG) )
import           System.Environment     (getArgs, getProgName, getEnv)
import           System.Console.GetOpt  ( getOpt
                                        , ArgDescr (NoArg, ReqArg)
                                        , ArgOrder (Permute)
                                        , OptDescr (Option)
                                        , usageInfo )
import           Text.Printf            (printf)
-----------------------------------------------------------------------}}}


-- Configuration ------------------------------------------------------{{{
data Config = Config { fileName   :: String
                     , defSection :: String
                     , hostSpec   :: String
                     , userSpec   :: String
                     , passSpec   :: String }

defaultConfig :: Config
defaultConfig = Config { fileName   = ".xmppcatrc"
                       , defSection = "DEFAULT"
                       , hostSpec   = "host"
                       , userSpec   = "user"
                       , passSpec   = "password" }
-----------------------------------------------------------------------}}}

type HostString = String
type UserString = String
type PassString = String
type RxIdString = String

data Option = Host HostString
            | User UserString
            | Pass PassString
            | RxId RxIdString
            | Help
            | Debug
            deriving (Eq,Ord,Show)

type Host = String
type User = Text
type Pass = Text
type RxId = Text

data MsgSpec = MsgSpec { host :: Host
                       , user :: User
                       , pass :: Pass
                       , rxId :: RxId
                       , text :: Text }
                       deriving (Eq,Show)

defaultMsgSpec :: MsgSpec
defaultMsgSpec = MsgSpec { host = ""
                         , user = ""
                         , pass = ""
                         , rxId = ""
                         , text = "" }

mergeMsgSpecs :: MsgSpec -> MsgSpec -> MsgSpec
mergeMsgSpecs specRec specDom =
    MsgSpec { host = (mergeSpecS `on` host) specDom specRec
            , user = (mergeSpec  `on` user) specDom specRec
            , pass = (mergeSpec  `on` pass) specDom specRec
            , rxId = (mergeSpec  `on` rxId) specDom specRec
            , text = (mergeSpec  `on` text) specDom specRec }
      where
        mergeSpec :: Text -> Text -> Text
        mergeSpec rec dom = case dom of
                              "" -> rec
                              _ -> dom
        mergeSpecS :: String -> String -> String
        mergeSpecS rec dom = unpack $ on mergeSpec pack rec dom


options :: [OptDescr Option]
options = [ Option "H" ["host"]      (ReqArg Host "<host>")
            "XMPP host domain name or IP address."

          , Option "U" ["user"]      (ReqArg User "<user>")
            "User to identify as to the XMPP host."

          , Option "P" ["password"]  (ReqArg Pass "<password>")
            "Password to authenticate 'user' to XMPP host."

          , Option "R" ["recipient"] (ReqArg RxId "<recipient>")
            "Intended recipient of message."

          , Option "h" ["help"]      (NoArg Help)
            "Print usage help."

          , Option "d" ["debug"]     (NoArg Debug)
            "Enable debugging output." ]


data RunFlags = RunFlags { debug :: Bool }
              deriving (Eq,Show)

defaultFlags :: RunFlags
defaultFlags = RunFlags { debug = False }


data CommandSpec = RunHelp
                 | RunXmppCat MsgSpec
                 | Error String
                 deriving (Eq,Show)


getCommandSpec :: [Option] -> [String] -> CommandSpec
getCommandSpec opts msgs = case cleanOpts of
      [Help] -> RunHelp
      _      -> RunXmppCat $ foldl setOption baseMsgSpec cleanOpts
    where
      cleanOpts :: [Option]
      cleanOpts = nub opts

      baseMsgSpec :: MsgSpec
      baseMsgSpec = defaultMsgSpec { text = pack $ concat msgs }

      setOption :: MsgSpec -> Option -> MsgSpec
      setOption msgSpec opt | (Host h) <- opt = msgSpec { host = h }
                            | (User u) <- opt = msgSpec { user = pack u }
                            | (Pass p) <- opt = msgSpec { pass = pack p }
                            | (RxId r) <- opt = msgSpec { rxId = pack r }
                            | otherwise          = msgSpec
  

getRunFlags :: [Option] -> RunFlags
getRunFlags opts | Debug `elem` opts = defaultFlags {debug = True}
                 | otherwise         = defaultFlags

parseArgs :: [String] -> (CommandSpec, RunFlags)
parseArgs argv = case getOpt Permute options argv of
    (opts, msgs, []) -> (getCommandSpec opts msgs, getRunFlags opts)
    (_, _, errs)     -> (Error $ concat errs, defaultFlags)

usageHeader :: String -> String
usageHeader = printf "Usage: %s [<options>] <message>"

extractResponseText :: Message -> String
extractResponseText = unpack . T.concat . map getPayloadText . messagePayload
  where
    getPayloadText :: Element -> Text
    getPayloadText = T.concat . elementText


parseConfig :: MonadIO m => FilePath -> m (Either CPError MsgSpec)
parseConfig configPath = runExceptT $ do
      let configParser = emptyCP
          section = defSection defaultConfig
          hSpec = hostSpec defaultConfig
          uSpec = userSpec defaultConfig
          pSpec = passSpec defaultConfig
      configReader <- join $ liftIO $ readfile configParser configPath

      configHost <- get configReader section hSpec
      configUser <- get configReader section uSpec
      configPass <- get configReader section pSpec
      return $ defaultMsgSpec { host = configHost
                              , user = pack configUser
                              , pass = pack configPass }

fetchReply :: MsgSpec -> IO ()
fetchReply msgSpec = do
    sessionResult <- session (host msgSpec)
        (Just (const [scramSha1 (user msgSpec) Nothing (pass msgSpec)]
                    , Nothing))
        def
    currentSession <- case sessionResult of
              Right s -> return s
              Left e -> error $ "XmppFailure: " ++ show e

    _ <- sendPresence def currentSession

    let recipient = fromMaybe (error "Error: Failed to find recipient.")
                              (jidFromText $ rxId msgSpec)
        message = simpleIM recipient $ text msgSpec

    _ <- sendMessage message currentSession
    response <- pullMessage currentSession

    case response of
      Left errMsg -> error $ "Error: " ++ show errMsg
      Right msg -> putStrLn $ extractResponseText msg

main :: IO ()
main = do
    (commandSpec, runFlags) <- liftM parseArgs getArgs

    when (debug runFlags) $ updateGlobalLogger "xmppcat" $ setLevel DEBUG

    case commandSpec of
      RunHelp   -> getProgName >>= putStrLn . flip usageInfo options . usageHeader
      Error err -> error err
      RunXmppCat optionMsgSpec -> do

        homeDir <- getEnv "HOME"
        let configPath = homeDir ++ "/" ++ fileName defaultConfig
        configParse <- parseConfig configPath

        case configParse of
          Left err      -> putStrLn $ "Config Error: " ++ show err
          Right configMsgSpec -> do
            let msgSpec = mergeMsgSpecs optionMsgSpec configMsgSpec
            fetchReply msgSpec
