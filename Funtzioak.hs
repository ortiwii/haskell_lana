import Motak
import Adibideak 

hutsa_da :: (Eq t) => [t] -> Bool
hutsa_da [] = True
hutsa_da s  = False

af_da :: Af -> Bool
af_da (a,b,c,d,e,f) 
    |null(a)        = error "Egoerak ez daude definituta"
    |null(b)        = error "Alfabetoa ez dago definituta"
    |null(c)        = error "A_trantsizioak ez daude definituta"
    |null(f)        = error "AzkenEgoerak ez daude definituta"
    |otherwise      = not(errepikatzen_da(a)) && not(errepikatzen_da(b)) && badago e a && badaude a b c && badaude_egoerak a f

errepikatzen_da :: Eq t => [t] -> Bool 
errepikatzen_da [] = False
errepikatzen_da (x:s)
    | badago x s    = True
    | otherwise     = errepikatzen_da s

badago :: Eq t => t -> [t] -> Bool
badago x [] = False
badago x (s:w)
    | x == s        = True
    | otherwise     = badago x w

badaude_egoerak :: Egoerak -> Egoerak -> Bool
badaude_egoerak total egoerak
	|hutsa_da(egoerak)		= True
	|(head egoerak)`elem`total	= badaude_egoerak (tail egoerak) total
	|otherwise			= False


badago_egoera :: Egoerak -> A_trantsizioa -> Bool
badago_egoera x (a,b,c) = badago a x && badago c x

badago_sinbol :: Alfabetoa -> A_trantsizioa -> Bool
badago_sinbol x (a,b,c) = badago b x

badaude :: Egoerak -> Alfabetoa -> A_trantsizioak -> Bool
badaude s z u
    | (null s) || (null z)  = False
    | (null u)      = True 
    | badago_egoera s (head u) && badago_sinbol z (head u)  = badaude s z (tail u)
    | otherwise     = False 

---Lengoaiakoa da---
lengoaiakoa_da :: Af -> Hitza -> Bool
lengoaiakoa_da (a,b,c,d,e,f) hitza
    |not(af_da (a,b,c,d,e,f))              	= False
    |hutsa_da hitza                             = error "Hitz huts bat pasatu duzu"
    |otherwise                                  = lengoaiakoa_da_lag (a,b,c,d,e,f) hitza e []

lengoaiakoa_da_lag :: Af -> Hitza -> Egoera -> [(Egoera, Hitza)] -> Bool
lengoaiakoa_da_lag (a,b,c,d,e,f) hitza egungoEgoera erregistroa
    |hutsa_da(hitza) && (badago egungoEgoera f)      = True
    |hutsa_da(hitza) && not(badago egungoEgoera f)   = False
    |otherwise                                       = lengoaiakoa_da_lag (a,b,c,d,e,f) (tail hitza) 
				(get_trantsizioa c egungoEgoera (head hitza)) ((egungoEgoera, hitza):erregistroa)

get_trantsizioa :: A_trantsizioak -> Egoera -> Sinboloa -> Egoera
get_trantsizioa ((x,y,z):s) egoera sinboloa
    |null ((x,y,z):s)              = error "Sinboloa ez dago trantsizioen listan"
    |y == sinboloa && x == egoera  = z
    |otherwise                     = get_trantsizioa s egoera sinboloa

--sailkatu :: Af -> Af_motak
--sailkatu af
--    | 
--
--afed_bat_lortu :: Af -> Af
--afed_bat_lortu af = ...
--
--bitarra_da :: Af -> Bool
--bitarra_da af = ...
--
--beta_afed_bat_lortu :: Af -> Af
--beta_afed_bat_lortu af = ...
=======
import Motak
import Adibideak 

hutsa_da :: (Eq t) => [t] -> Bool
hutsa_da [] = True
hutsa_da s  = False

af_da :: Af -> Bool
af_da (a,b,c,d,e,f) 
    |null(a)        = error "Egoerak ez daude definituta"
    |null(b)        = error "Alfabetoa ez dago definituta"
    |null(c)        = error "A_trantsizioak ez daude definituta"
    |null(f)        = error "AzkenEgoerak ez daude definituta"
    |otherwise      = not(errepikatzen_da(a)) && not(errepikatzen_da(b)) && badago e a && badaude a b c
errepikatzen_da :: Eq t => [t] -> Bool
errepikatzen_da [] = False
errepikatzen_da (x:s)
    | badago x s    = True
    | otherwise     = errepikatzen_da s

badago :: Eq t => t -> [t] -> Bool
badago x [] = False
badago x (s:w)
    | x == s        = True
    | otherwise     = badago x w

badago_egoera :: Egoerak -> A_trantsizioa -> Bool
badago_egoera x (a,b,c) = badago a x && badago c x

badago_sinbol :: Alfabetoa -> A_trantsizioa -> Bool
badago_sinbol x (a,b,c) = badago b x

badaude :: Egoerak -> Alfabetoa -> A_trantsizioak -> Bool
badaude s z u
    | (null s) || (null z)  = False
    | (null u)      = True 
    | badago_egoera s (head u) && badago_sinbol z (head u)  = badaude s z (tail u)
    | otherwise     = False 

hitza_dago :: Af -> Hitza -> Bool
hitza_dago(a,b,c,d,e,f) hitza
    | (null hitza) = True
    | badago_sinbol b (0,(head hitza),0)    = lengoaiakoa_da (a,b,c,d,e,f) (tail hitza)
    | otherwise    = False

lengoaiakoa_da :: Af -> Hitza -> Bool
lengoaiakoa_da (a,b,c,d,e,f) hitza
    |not(af_da (a,b,c,d,e,f))              	= False
    |not(hitza_dago (a,b,c,d,e,f) hitza)   	= False
    |(null hitza)                       	= error "Hitz huts bat pasatu duzu"
    |otherwise                             	= lengoaiakoa_da_lag (a,b,c,d,e,f) hitza e []

lengoaiakoa_da_lag :: Af -> Hitza -> Egoera -> [(Egoera, Hitza)] -> Bool
lengoaiakoa_da_lag (a,b,c,d,e,f) hitza egungoEgoera erregistroa
    |hutsa_da(hitza) && (badago egungoEgoera f)      = True
    |hutsa_da(hitza) && not(badago egungoEgoera f)   = False
    |otherwise                                       = lengoaiakoa_da_lag (a,b,c,d,e,f) (tail hitza) 
				(get_trantsizioa c egungoEgoera (head hitza)) ((egungoEgoera, hitza):erregistroa)

get_trantsizioa :: A_trantsizioak -> Egoera -> Sinboloa -> Egoera
get_trantsizioa ((x,y,z):s) egoera sinboloa
    |null ((x,y,z):s)              = error "Sinboloa ez dago trantsizioen listan"
    |y == sinboloa && x == egoera  = z
    |otherwise                     = get_trantsizioa s egoera sinboloa

--sailkatu :: Af -> Af_motak
--sailkatu af
--    | 
--
--afed_bat_lortu :: Af -> Af
--afed_bat_lortu af = ...
--
--bitarra_da :: Af -> Bool
--bitarra_da af = ...
--
--beta_afed_bat_lortu :: Af -> Af
--beta_afed_bat_lortu af = ...
