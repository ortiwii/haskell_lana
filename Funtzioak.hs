import Motak

af_da :: Af -> Bool
af_da (a,b,c,d,e,f) 
    |null(Egoerak)		= error "Egoerak ez daude definituta"
    |null(Alfabetoa)	= error "Alfabetoa ez dago definituta"
    |null(A_trantsizioak)	= error "A_trantsizioak ez daude definituta"
    |null(E_trantsizioak)	= error "E_trantsizioak ez daude definituta" 
    |null(HasierakoEgoera)	= error "HasierakoEgoera ez dago definituta"
    |null(AzkenEgoerak)	= error "AzkenEgoerak ez daude definituta"
    |otherwise		= not(errepikatzen_da(a)) && not(errepikatzen_da(b)) && badago e a && badaude a b c

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

lengoaiakoa_da :: Af -> Hitza -> Bool
lengoaiakoa_da (a,b,c,d,e,f) hitza
    | (null hitza) = True
    | badago_sinbol b (0,(head hitza),0)    = lengoaiakoa_da (a,b,c,d,e,f) (tail hitza)
    | otherwise    = False

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
