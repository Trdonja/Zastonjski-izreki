import Data.Maybe
import Parser
import Zastonjski

main = do
    putStrLn "Vpiši ime tipa:"
    ime <- getLine    -- Prebere ime predpisa.
    putStrLn "Vpiši predpis tipa:"
    predpis <- getLine  -- Prebere predpis, ki ga podamo v standardnem vhodu.
    putStrLn (odgovor (parse predpis) ime) -- Izpiše izrek oziroma sporoči napako.


odgovor :: Maybe Type -> String -> String
odgovor tip ime
   | isNothing tip = "Vnos je napačen."
   | otherwise   = "\nIzrek za " ++ ime ++ " :: " ++ (show (fromJust tip)) ++ ":\n\n" ++ (show izrek)
   where izrek = theorem ( fromJust tip) ime
