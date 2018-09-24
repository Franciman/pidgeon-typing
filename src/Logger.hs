module Logger where

-- Dead simple logger

data Severity = Info
              | Warning
              | Error

logMessage :: Severity -> String -> IO ()
logMessage Info msg    = putStrLn $ "LOG INFO: " ++ msg
logMessage Warning msg = putStrLn $ "LOG WARN: " ++ msg
logMessage Error msg   = putStrLn $ "LOG ERR: " ++ msg


logInfo, logWarning, logError :: String -> IO ()
logInfo    = logMessage Info
logWarning = logMessage Warning
logError   = logMessage Error
