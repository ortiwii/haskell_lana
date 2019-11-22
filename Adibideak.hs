module Adibideak where
import Motak

-- 1. irudiko automata
getAutomata1 :: Af
getAutomata1 = ([0,1,2,3],['a','b','c'],[(0,'a',1),(0,'b',0),(0,'c',0),(1,'a',2),(1,'b',1),(1,'c',1),(2,'a',2),(2,'b',3),(2,'c',3),(3,'a',2),(3,'b',3),(3,'c',3)],[],0,[1,2])

-- 2. irudiko automata
getAutomata2 :: Af
getAutomata2 = ([0,1,2,3,4],['a','b','c'],[(0,'a',2),(0,'a',3),(0,'a',4),(0,'b',1),(0,'b',3),(0,'c',1),(0,'c',3),(1,'a',2),(1,'b',1),(1,'c',1),(2,'b',2),(2,'c',2),(3,'a',3),(3,'a',4),(3,'b',3),(3,'c',3)],[],0,[2,4])

-- 3. irudiko automata
getAutomata3 :: Af
getAutomata3 = ([0,1,2,3,4],['a','b','c'],[(1,'a',2),(1,'b',1),(1,'c',1),(2,'b',2),(2,'c',2),(3,'a',3),(3,'a',4),(3,'b',3),(3,'c',3)],[(0,1),(0,3)],0,[2,4])

-- 4. irudiko automata
getAutomata4 :: Af
getAutomata4 = ([0,1,2,3,4,5],['a','b','c'],[(0,'a',1),(0,'a',4),(0,'b',0),(0,'b',4),(0,'c',0),(0,'c',4),(1,'a',2),(1,'a',4),(1,'b',1),(1,'b',4),(1,'c',1),(1,'c',4),(2,'a',2),(2,'a',5),(2,'b',3),(2,'b',5),(2,'c',3),(2,'c',5),(3,'a',2),(3,'a',5),(3,'b',3),(3,'b',5),(3,'c',3),(3,'c',5),(4,'a',4),(4,'a',5),(4,'b',4),(4,'b',5),(4,'c',4),(4,'c',5),(5,'a',4),(5,'a',5),(5,'b',4),(5,'b',5),(5,'c',4),(5,'c',5)],[],0,[1,2])

-- 5. irudiko automata
getAutomata5 :: Af
getAutomata5 = ([0,1,2,3,4,5,6,7],['a','b','c'],[(0,'a',2),(0,'a',5),(0,'b',1),(0,'b',3),(0,'c',1),(0,'c',3),(1,'a',2),(1,'a',7),(1,'b',1),(1,'b',7),(1,'c',1),(1,'c',7),(2,'a',6),(2,'a',7),(2,'b',2),(2,'b',7),(2,'c',2),(2,'c',7),(3,'a',3),(3,'a',4),(3,'b',4),(3,'b',6),(3,'c',3),(3,'c',6),(4,'a',6),(4,'a',7),(4,'b',6),(4,'b',7),(4,'c',6),(4,'c',7),(5,'a',6),(5,'a',7),(5,'b',6),(5,'b',7),(5,'c',6),(5,'c',7),(6,'a',6),(6,'a',7),(6,'b',6),(6,'b',7),(6,'c',6),(6,'c',7),(7,'a',6),(7,'a',7),(7,'b',6),(7,'b',7),(7,'c',6),(7,'c',7)],[(0,6),(0,7),(1,6),(1,7),(2,6),(2,7),(3,6),(3,7),(4,6),(4,7),(5,6),(5,7),(6,6),(6,7),(7,6),(7,7)],0,[2,4])

-- 6. irudiko automata
getAutomata6 :: Af
getAutomata6 = ([0,1,2,3,4,5,6],['a','b','c'],[(0,'a',5),(0,'a',6),(0,'b',5),(0,'b',6),(0,'c',5),(0,'c',6),(1,'a',1),(1,'a',6),(1,'b',1),(1,'b',6),(1,'c',1),(1,'c',6),(2,'a',5),(2,'a',6),(2,'b',2),(2,'b',6),(2,'c',2),(2,'c',6),(3,'a',3),(3,'a',4),(3,'b',3),(3,'b',5),(3,'c',3),(3,'c',5),(4,'a',5),(4,'a',6),(4,'b',5),(4,'b',6),(4,'c',5),(4,'c',6),(5,'a',5),(5,'a',6),(5,'b',5),(5,'b',6),(5,'c',5),(5,'c',6),(6,'a',5),(6,'a',6),(6,'b',5),(6,'b',6),(6,'c',5),(6,'c',6)],[(0,1),(0,3),(1,5),(1,6),(2,5),(2,5),(3,5),(3,6),(4,5),(4,6),(5,5),(5,6),(6,5),(6,6)],0,[2,4])

-- 15. irudiko automata
getAutomata15 :: Af
getAutomata15 = ([0,1,2,3,4],['a','b','c'],[(0,'a',2),(0,'a',3),(0,'a',4),(1,'a',2),(1,'b',1),(1,'c',1),(2,'b',2),(2,'c',2),(3,'a',3),(3,'a',4),(3,'b',3),(3,'c',3)],[(0,1)],0,[2,4])

-- 16. irudiko automata
getAutomata16 :: Af
getAutomata16 = ([0,1,2,3,4,5,6,7],['a','b','c'],[(0,'a',2),(0,'a',5),(0,'b',6),(0,'b',6),(0,'c',6),(0,'c',7),(1,'a',2),(1,'a',7),(1,'b',1),(1,'b',7),(1,'c',1),(1,'c',7),(2,'a',6),(2,'a',7),(2,'b',2),(2,'b',7),(2,'c',2),(2,'c',7),(3,'a',3),(3,'a',4),(3,'b',3),(3,'b',6),(3,'c',3),(3,'c',6),(4,'a',6),(4,'a',7),(4,'b',6),(4,'b',7),(4,'c',6),(4,'c',7),(5,'a',6),(5,'a',7),(5,'b',6),(5,'b',7),(5,'c',6),(5,'c',7),(6,'a',6),(6,'a',7),(6,'b',6),(6,'b',7),(6,'c',6),(6,'c',7),(7,'a',6),(7,'a',7),(7,'b',6),(7,'b',7),(7,'c',6),(7,'c',7)],[(0,1),(0,6),(1,6),(1,7),(2,6),(2,7),(3,6),(3,7),(4,6),(4,7),(5,3),(5,4),(6,6),(6,7),(7,6),(7,7)],0,[2,4])

-- 1. Automata txarto irudiko automata
	--trantsizio ez-huts batean sinboloa AFari dagokion alfabetokoa ez da
getAutomataT1 :: Af
getAutomataT1 = ([0,1,2,3],['a','b','c'],[(0,'a',1),(0,'b',0),(0,'c',0),(0,'d',0),(1,'a',2),(1,'b',1),(1,'c',1),(2,'a',2),(2,'b',3),(2,'c',3),(3,'a',2),(3,'b',3),(3,'c',3)],[],0,[1,2])

-- 2. Automata txarto irudiko automata
	--Trantsizio bateko egoera ez dago egoera multzoan
getAutomataT2 :: Af
getAutomataT2 = ([0,1,2,3],['a','b','c'],[(0,'a',1),(0,'b',0),(0,'c',0),(1,'a',2),(1,'b',1),(1,'c',1),(2,'a',2),(2,'b',3),(2,'c',3),(3,'a',2),(3,'b',3),(3,'c',3),(4,'a',4),(4,'b',4),(4,'c',4)],[],0,[1,2])

-- 3. Automata txarto irudiko automata
	--hasierako egoera AFari dagokion egoera multzokoa ez da
getAutomataT3 :: Af
getAutomataT3 = ([0,1,2,3,4],['a','b','c'],[(0,'a',2),(0,'a',3),(0,'a',4),(0,'b',1),(0,'b',3),(0,'c',1),(0,'c',3),(1,'a',2),(1,'b',1),(1,'c',1),(2,'b',2),(2,'c',2),(3,'a',3),(3,'a',4),(3,'b',3),(3,'c',3)],[],-1,[2,4])

-- 4. Automata txarto irudiko automata
	--amaierako egoerak AFari dagokion egoera multzokoak ez dira
getAutomataT4 :: Af
getAutomataT4 = ([0,1,2,3,4],['a','b','c'],[(0,'a',2),(0,'a',3),(0,'a',4),(0,'b',1),(0,'b',3),(0,'c',1),(0,'c',3),(1,'a',2),(1,'b',1),(1,'c',1),(2,'b',2),(2,'c',2),(3,'a',3),(3,'a',4),(3,'b',3),(3,'c',3)],[],0,[2,4,5])
