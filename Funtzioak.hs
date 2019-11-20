import Motak

hutsa_da :: [t] -> Bool
hutsa_da [] = True
hutsa_da s = False

af_da :: Af -> Bool
af_da (a,b,c,d,e,f) = not(errepikatzen_da(a)) && not(errepikatzen_da(b)) && badago e a && badaude a b c

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
    | hutsa_da u    = True 
    | badago_egoera s (head u) && badago_sinbol z (head u)  = badaude s z (tail u)
    | otherwise     = False 

--lengoaiakoa_da :: Af -> Hitza -> Bool
--lengoaiakoa_da af hitza = ...
--
--sailkatu :: Af -> Af_motak
--sailkatu af = ...
--
--afed_bat_lortu :: Af -> Af
--afed_bat_lortu af = ...
--
--bitarra_da :: Af -> Bool
--bitarra_da af = ...
--
--beta_afed_bat_lortu :: Af -> Af
--beta_afed_bat_lortu af = ...
