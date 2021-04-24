
{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative
import Prelude


data LoginError = InvalidEmail | WrongPW | NoSuchUser
  deriving Show

users :: Map Text Text 
users = Map.fromList [ ("Sammy@bammy.com", "qwerty")
                     , ("h@google.com", "douchebag") ]

getDom :: Text -> Either LoginError Text
getDom xs = case splitOn "@" xs of 
   [name, dom] -> Right dom
   _              -> Left InvalidEmail

printText :: Either LoginError Text -> IO()
printText = T.putStrLn . either 
                (const "ERROR: not duh good stuff ye") (append "DOMAIN:")
 

getToken :: IO (Either LoginError Text)
getToken = do
  T.putStrLn "Please enter email adress:" 
  email <- T.getLine 
  T.putStrLn email
  return (getDom email)

      
userLogin :: IO (Either LoginError Text)
userLogin = do
    token <- getToken

    case token of
        Right domain ->
            case Map.lookup domain users of
                Just userpw -> do
                    T.putStrLn "Enter password:"
                    password <- T.getLine

                    if userpw == password then
                        return token
                    else
                        return (Left WrongPW)
                Nothing ->
                    return (Left NoSuchUser)
        left ->
            return left
