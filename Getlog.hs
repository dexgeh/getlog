module Main where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Network.HTTP as HTTP
import Text.Regex (subRegex, mkRegex)
import Text.Regex.Posix ((=~))
import qualified System.IO as IO
import qualified Control.Monad as M
import qualified System.Directory as D

str_replace :: String -> String -> String -> String
str_replace pattern replacement text = subRegex (mkRegex pattern) text replacement

config :: Map.Map String [String]
config = Map.fromList [ ( "DEV", [ "http://10.0.0.1/logs/" ] )
                      , ( "PRO", [ "http://10.0.0.2/logs/", "http://10.0.0.3/logs/" ] )
                      ]

readUntilMatch :: String -> String -> String -> IO String
readUntilMatch msg regex defaultVal = do
    IO.hPutStr IO.stdout msg
    if not . null $ defaultVal
    then do
        IO.hPutStr IO.stdout " (default="
        IO.hPutStr IO.stdout defaultVal
        IO.hPutStr IO.stdout ") "
    else do
        IO.hPutStr IO.stdout " "
    IO.hFlush IO.stdout
    response <- getLine
    if null $ (response =~ regex :: String)
    then if (null response) && (not . null $ defaultVal)
         then return defaultVal
         else do
            IO.hPutStrLn IO.stdout "Invalid input."
            readUntilMatch msg regex defaultVal
    else return response

generateUrls :: String -> String -> Int -> [String]
generateUrls env filename backups =
    let urls = Maybe.fromJust . Map.lookup env $ config
    in List.concat . map withBackups $ urls
    where withBackups url = map withBackup [0..backups]
            where withBackup nBackup = if nBackup == 0
                                       then url ++ filename
                                       else url ++ filename ++ "." ++ (show nBackup)

urlToFile :: String -> String
urlToFile = str_replace "[:/]" "_"

data LogFile = LogFile { logfileUrl :: String
                       , logfilePath :: String
                       , logfileHandle :: Maybe IO.Handle
                       , logfileCurrentLine :: String
                       , logfileCurrentValue :: Integer
                       } deriving (Show, Eq)

mkLogFile :: String -> LogFile
mkLogFile url = LogFile url (urlToFile url) Nothing "" 0

savePage :: LogFile -> IO (Maybe LogFile)
savePage logfile = do
    let url = logfileUrl logfile
    response <- HTTP.simpleHTTP $ HTTP.getRequest url
    let code = fmap HTTP.rspCode response
    case code of
        Left _ -> do
            return Nothing
        Right status -> do
            if status == (2,0,0)
            then do
                body <- HTTP.getResponseBody response
                let path = logfilePath logfile
                writeFile path body
                h <- IO.openFile path IO.ReadMode
                IO.hSetBuffering h IO.LineBuffering
                line <- IO.hGetLine h
                return $ Just logfile { logfileHandle = Just h
                                      , logfileCurrentLine = line
                                      , logfileCurrentValue = timeValue line
                                      }
            else do
                return Nothing

savePages :: [LogFile] -> IO [LogFile]
savePages = fmap (map Maybe.fromJust) . fmap (filter $ not . (== Nothing)) . mapM savePage

parseTime :: String -> Maybe [String]
parseTime line =
    let matches = line =~ "^\\[([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2}):([0-9]{2}),([0-9]{3})\\].*$"
                    :: (String, String, String, [String])
        getGroups (before, match, after, groups) = groups
        groups = getGroups matches
    in if null groups
       then Nothing
       else Just groups

timeValue :: String -> Integer
timeValue line =
    let m_groups = parseTime line
    in case m_groups of
        Nothing -> 0
        Just groups -> read (List.concat groups) :: Integer

isnewline :: Char -> Bool
isnewline c = c == '\n' || c == '\r'

writeSorted :: IO.Handle -> [LogFile] -> Bool -> IO ()
writeSorted hOut lFiles withSourcePrefix = do
    if null lFiles
    then return ()
    else
        let headingLines = map logfileCurrentLine lFiles
            values = map logfileCurrentValue lFiles
            minval = minimum values
            candidateVal = head . filter (== minval) $ values
            candidateIdx = List.elemIndex candidateVal $ values
        in case candidateIdx of
                Nothing -> return ()
                Just idx -> do
                    let line = headingLines !! idx
                    if not . null $ line
                    then do
                        if withSourcePrefix
                        then IO.hPutStrLn hOut $ (logfileUrl $ lFiles !! idx) ++ " " ++ line
                        else IO.hPutStrLn hOut line
                    else return ()
                    new_lfiles <- generateNewLogFileList lFiles idx
                    writeSorted hOut new_lfiles withSourcePrefix

generateNewLogFileList lFiles idx =
    let lf = lFiles !! idx
        h = Maybe.fromJust . logfileHandle $ lf
    in do
        eof <- IO.hIsEOF h
        if eof
        then do
            IO.hClose h
            IO.hPutStrLn IO.stdout $ "Terminata lettura da file " ++ (logfilePath lf)
            let new_lfs = take idx lFiles ++ drop (idx+1) lFiles
            return new_lfs
        else do
            line <- IO.hGetLine h
            let val = timeValue line
            let new_lf = lf { logfileCurrentLine = line
                            , logfileCurrentValue = val
                            }
            return $ take idx lFiles ++ [new_lf] ++ drop (idx+1) lFiles

flagSN :: String -> Bool
flagSN s = (s == "S") || (s == "s")

main = do
    pwd <- D.getCurrentDirectory
    IO.hPutStrLn IO.stdout $ "I files temporanei e il file di output verrano salvati nella directory:"
    IO.hPutStrLn IO.stdout $ pwd
    let envRgx = "(" ++ (List.concat . List.intersperse "|" . List.map fst . Map.toList $ config) ++ ")"
    env <- readUntilMatch ("Ambiente " ++ envRgx ++"?") envRgx "ESE"
    IO.hPutStrLn IO.stdout $ "Ambiente selezionato: " ++ env
    IO.hPutStrLn IO.stdout $ show $ Maybe.fromJust . Map.lookup env $ config
    filename <- readUntilMatch "Logfile? (*.log)" "\\.log$" ""
    IO.hPutStrLn IO.stdout $ "Logfile: " ++ filename
    sBackups <- readUntilMatch "Backups(0-9)?" "[0-9]" "0"
    let backups = read sBackups :: Int
    IO.hPutStrLn IO.stdout $ "Backups: " ++ (show backups)
    let urls = generateUrls env filename backups
    IO.hPutStrLn IO.stdout $ "Urls: " ++ (show urls)
    let logfiles = map mkLogFile urls
    IO.hPutStrLn IO.stdout $ "Scaricamento files..."
    logfiles <- savePages logfiles
    IO.hPutStrLn IO.stdout $ "Files salvati: "
    IO.hPutStrLn IO.stdout $ show $ map logfilePath logfiles
    outputHandle <- IO.openBinaryFile filename IO.WriteMode
    IO.hPutStrLn IO.stdout $ "Scrittura output " ++ filename
    withSourcePrefix <- readUntilMatch "Prefisso url sorgente[SN]?" "^[sSnN]$" "N"
    writeSorted outputHandle logfiles (flagSN withSourcePrefix)
    IO.hClose outputHandle
    mapM D.removeFile $ map logfilePath logfiles
    IO.hPutStrLn IO.stdout $ "Cancellazione files temporanei conclusa."
    
