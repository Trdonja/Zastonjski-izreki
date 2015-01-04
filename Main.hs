import Parser
import Zastonjski

main = do
    putStrLn "Vpiši predpis tipa"
    predpis <- getLine  -- Prebere predpis, ki ga podamo v standardnem vhodu
    putStrLn "Vpiši ime tipa"
    ime <- getLine
    let izrek = theorem (parse predpis) ime
    putStrLn $ "\nIzrek za " ++ ime ++ " :: " ++ predpis ++ ":\n\n" ++ (show izrek)