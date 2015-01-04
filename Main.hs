import Parser
import Zastonjski

main = do
   	putStrLn "Vpiši predpis tipa"
	predpis <- getLine  -- Prebere predpis, ki ga podamo v standardnem vhodu
	
	let izrek = theorem (parse predpis) "app"
	
	putStrLn $ show izrek -- Izpis izreka (se bo še popravil)
    
