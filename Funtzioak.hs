import Motak

hutsa_da :: [t] -> Bool
hutsa_da [] = True
hutsa_da s = False

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

badago_egoera :: Eq t => [t] -> (a,b,c) -> Bool
badago_egoera x (a,b,c) = badago a x && badago c x

badago_hitza :: Eq t => [t] -> (a,b,c) -> Bool
badago_hitza x (a,b,c) = badago b x

badaude :: Eq t => [t] -> [t] -> [t] -> Bool
badaude [] [] [] = False
badaude s z (u:w)
    | badago_egoera s u && badago_hitza z u = badaude s z w
    | hutsa_da (u:w)                        = True 
    | otherwise                             = False 
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
