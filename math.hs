import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network.Connection
import Data.Default
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Loops

states = M.fromList [("Alabama","Montgomery"),("Alaska","Juneau"),("Arizona","Phoenix"),("Arkansas","Little Rock"),("California","Sacramento"),("Colorado","Denver"),("Connecticut","Hartford"),("Delaware","Dover"),("Florida","Tallahassee"),("Georgia","Atlanta"),("Hawaii","Honolulu"),("Idaho","Boise"),("Illinois","Springfield"),("Indiana","Indianapolis"),("Iowa","Des Moines"),("Kansas","Topeka"),("Kentucky","Frankfort"),("Louisiana","Baton Rouge"),("Maine","Augusta"),("Maryland","Annapolis"),("Massachusetts","Boston"),("Michigan","Lansing"),("Minnesota","Saint Paul"),("Mississippi","Jackson"),("Missouri","Jefferson City"),("Montana","Helena"),("Nebraska","Lincoln"),("Nevada","Carson City"),("New Hampshire","Concord"),("New Jersey","Trenton"),("New Mexico","Santa Fe"),("New York","Albany"),("North Carolina","Raleigh"),("North Dakota","Bismarck"),("Ohio","Columbus"),("Oklahoma","Oklahoma City"),("Oregon","Salem"),("Pennsylvania","Harrisburg"),("Rhode Island","Providence"),("South Carolina","Columbia"),("South Dakota","Pierre"),("Tennessee","Nashville"),("Texas","Austin"),("Utah","Salt Lake City"),("Vermont","Montpelier"),("Virginia","Richmond"),("Washington","Olympia"),("West Virginia","Charleston"),("Wisconsin","Madison"),("Wyoming","Cheyenne")]

calculate :: String -> Int -> Int -> String
calculate op num result = case op of
                            "+" -> show (result - num)
                            "-" -> show (result + num)
                            "*" -> show (result `div` num)
                            "/" -> show (result * num)

answer :: [String] -> (String, Bool)
answer [_,_,z] = case (M.lookup z states) of
                    Just x -> (x, True)
                    Nothing -> ("ooops, what happened?", False)
answer [_,_,z,y] = case (M.lookup (z ++ " " ++ y) states) of
                    Just x -> (x, True)
                    Nothing -> ("this is not a state", False)
answer [_,_,_,op,num,_,result] = (calculate op (read num :: Int) (read result :: Int), True)
answer [flag] = ("i think we found the flag: " ++ flag, False)

repl con = do
    response <- connectionGet con 40
    print response
    question <- connectionGet con 40
    print question
    let sol = answer . words $ C.unpack question
    print sol
    connectionPut con (C.pack $ fst sol)
    if (snd sol)
        then repl con
        else
            connectionClose con

main = do
    ctx <- initConnectionContext
    con <- connectTo ctx $ ConnectionParams
                              { connectionHostname  = "34.253.165.46"
                              , connectionPort      = 11223
                              , connectionUseSecure = Nothing
                              , connectionUseSocks  = Nothing
                              }
    repl con
    connectionClose con
