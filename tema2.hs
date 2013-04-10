import Data.List

-- Tip de date ce reprezinta starile prin care poate trece expresia.
-- Este un arbore binar. Nodul contine o litera, iar  [c] poate 
-- contine mai multe litere - este un fel de informatie aditionala 
-- folosita pentru expresiile cuprinse intre [].
data State c s = Start 
			| Token c [c] (State c s) (State c s) 
			| Redo
			| Empty
			| End	deriving (Show, Eq)

data Result s = NoMatch | Match s deriving (Show, Eq)
 
{- Functii folosite pentru parantezele patrate -}

-- Returneaza continutul dintr-o paranteza patrata. 
-- Continutul nu trebuie sa includa prima paranteza
-- patrata. De exemplu trebuie sa fie: "123]456"	
getSquared :: [Char] -> [Char] 
getSquared [] = [] 
getSquared (x:xs) 
	| x == ']' = []
	| otherwise = (x:(getSquared xs))

-- Returneaza continutul de dincolo de prima parateza 
-- patrata de inchidere ']' gasita

besideSquare :: [Char] -> Int -> [Char]
besideSquare [] i  = [] 
besideSquare (x:xs) i 
	| x == ']' = (besideSquare xs 1)
	| i == 1 = (x:(besideSquare xs i))
	| otherwise = (besideSquare xs i)

-- Determina daca un caracter se potriveste cu cel putin 
-- o litera din sir. Folosit pentru continutul normal dintre 
-- paranteze patrate
matchesSquared :: [Char] -> Char -> Bool 
matchesSquared x c = (elem c x)

-- Determina daca un caracter nu se potriveste cu nici unul
-- din sir. Folosit pentru continutul exclusiv (^) dintre 
-- paranteze patrate.
matchesExclusive :: [Char] -> Char -> Bool
matchesExclusive [] c = True 
matchesExclusive (x:xs) c 
	| x == '^' = (not (matchesSquared xs c))
	| otherwise = False	

-- Determina daca un caracter se potriveste cu intervalul/
-- intervalele date. Folosit pentru intervalele date intre
-- paranteze patrate.
matchesSquaredInterval :: [Char] -> Char -> Bool 
matchesSquaredInterval [] c = False 
matchesSquaredInterval (x:y:z:xs) c
	| y == '-' = (x <= c && c <= z) || (matchesSquaredInterval xs c)
	| otherwise = False

-- Determina tipul unei expresii dintre paranteze patrate: 
-- 1 - Normala; 2 - Exclusiva; 3 - Interval 
squaredTruth :: [Char] -> Char -> Bool 
squaredTruth s c
	| (elem '-' s) = (matchesSquaredInterval s c) 
	| (head s) == '^' = (matchesExclusive s c)
	| otherwise = (matchesSquared s c)

{-- Functii folosite pentru construirea expresiei regulate --}

-- Verifica daca sirul de caractere are un pipe
-- verificarea cauta doar pipeuri la nivelul la care se afla 
-- de exemplu daca se deschide o paranteza nu ne intereseaza
-- ca exista pipe acolo. De aceea se foloseste un parametru 
-- in plus
hasPipe :: [Char] -> Int -> Bool
hasPipe [] i = False
hasPipe (x:xs) i 
	| x == '(' = (hasPipe xs (i + 1))
	| x == ')' = (hasPipe xs (i - 1))
	| x == '|' && i == 0 = True
	| otherwise = (hasPipe xs i)
	
-- Returneaza partea stanga dintr-un string separat de pipe 
-- Parametrul intreg se foloseste din nou pentru a exclude 
-- pipeurile din interiorul altor paranteze
leftSidepipe :: [Char] -> Int -> [Char] 
leftSidepipe [] i = [] 
leftSidepipe (x:xs) i
	| x == '|' && i == 0 = []
	| x == '(' = (x:(leftSidepipe xs (i + 1)))
	| x == ')' = (x:(leftSidepipe xs (i - 1)))
	| otherwise = (x:(leftSidepipe xs i))
	
-- Functie auxiliara pentru rightSidepipe
rightSidepipeaux :: [Char] -> Int -> [Char] 
rightSidepipeaux [] i = []
rightSidepipeaux (x:xs) i
	| x == '(' && i <= 0 = (rightSidepipeaux xs (i-1))
	| x == ')' && i < 0 = (rightSidepipeaux xs (i+1))
	| x == '|' && i >= 0 = (rightSidepipeaux xs 1)
	| i == 1 = x:(rightSidepipeaux xs i)
	| otherwise = (rightSidepipeaux xs i)

-- Returneaza partea dreapta de dincolo de pipe aflata in interiorul 
-- setului curent de paranteze (daca acesta exista)
rightSidepipe :: [Char] -> Int -> [Char]
rightSidepipe s i = rightSidepipeaux (insideBrackets s 1) i

-- Returneaza continutul dintr-o pereche de paranteze. Parametrul 
-- este folosit pentru cazul in care se deschid paranteze '(' sa 
-- stim unde se inchid. Ne intereseaza prima paranteza care se 
-- potriveste cu inceputul. Nu doar prima paranteza de tipul ')'.
insideBrackets :: [Char] -> Int -> [Char]
insideBrackets [] i = []
insideBrackets (x:xs) i 
	| x == '(' && i == 0 = (insideBrackets xs (i + 1))
	| x == '(' && i > 0 = (x:(insideBrackets xs (i +1)))
	| x == ')' && i > 1 = (x:(insideBrackets xs (i - 1)))
	| x == ')' && i == 1 = []
 	| i >= 1 = (x:(insideBrackets xs i)) 
 	| otherwise = (insideBrackets xs i)


--Verifica daca exista un pipe in, nu tine cont de paranteze
checkPipe :: [Char] -> Bool 
checkPipe [] = False
checkPipe (x:xs)
	| x == '|' = True 
	| otherwise = (checkPipe xs)
	
-- Returneaza continutul de dupa o paranteza care sa nu mai 
-- functioneaza pentru siruri de genul "12(34)5)678" => 678
afterBracket :: [Char] -> Int -> [Char] 
afterBracket [] i = [] 
afterBracket (x:xs) i 
	| x == '(' = (afterBracket xs (i + 1))
	| x == ')' && i > 0 = (afterBracket xs (i - 1))
	| x == ')' && i == 0 = xs
	| otherwise = (afterBracket xs i)

-- Dandu-se un sir de caractere se elimina primele N caractere 
-- din sir. Se returneza sirul rezultat
removeFirstN :: [Char] -> Int -> [Char]
removeFirstN s 0 = s
removeFirstN (x:xs) i = (removeFirstN xs (i -1))

-- Verifica daca un sir contine pe prima pozitie o '*'. Sirul 
-- poate fi vid
isStar :: [Char] -> Bool 
isStar [] = False 
isStar s = (head s) == '*'


-- Dintr-o expresie regula data sub forma unui sir de 
-- caractere construieste arborele de stari corespunzator
-- acelui sir. 
plain :: [Char] -> State Char s -> State Char s
plain [] s = s
plain [x] s 
	| x == ')' = s
	| otherwise = (Token x [] s Empty)
	
plain (x:y:xs) s 
	| y == '*' = (Token '*' [] (Token x [] Redo Empty) (plain xs s))
    					  
	| (hasPipe (x:y:xs) 0) = (Token '|' [] (plain ((leftSidepipe (x:y:xs) 0) 
					++ (afterBracket (x:y:xs) 0)) s)
					(plain ((rightSidepipe (x:y:xs) 0) 
					++ (afterBracket (x:y:xs) 0)) s)) 
   
    | x == '(' && (isStar (afterBracket (y:xs) 0))  =
  			 (Token '*' [] (Token '(' [] (plain (insideBrackets (x:y:xs) 0)  Redo) End)
  			 (plain (tail (afterBracket (y:xs) 0)) s))													  
	
	| x == '[' = (Token '[' (getSquared (y:xs)) (plain (besideSquare (y:xs) 0) s) Empty)
	| otherwise = (Token x [] (plain (y:xs) s) Empty)

-- Determina maximul dintre doua numere Intregi
maX :: Int -> Int -> Int 
maX x y 
	| x > y = x
	| otherwise = y

-- Pentru un string dat se duce pana la nivelul la care gaseste
-- Redo. Daca ajunge pana la acel nivel atfel incat sa respecte 
-- expresia returneaza nr. de caractere comparate. Daca pe drum
-- caracterele difera atunci va returna -1. Functia merge doar 
-- pentru o singura '*'. Nu suporta * in *.
sampleMatches :: [Char] -> State Char s -> Int -> Int
sampleMatches x Redo i = i
sampleMatches (x:xs) (Token c s s1 s2) i
	| c == '*' || c == '(' = (sampleMatches (x:xs) s1 i)
	| c == '[' && (not (squaredTruth s x)) = -1
	| x == c = (sampleMatches xs s1 (i+1))
	| x /= c && c /= '*' && c/= '|' && c /= '[' = -1
	| c == '|' = (maX (sampleMatches (x:xs) s1 i) (sampleMatches (x:xs) s2 i))
	| otherwise = (sampleMatches xs s1 (i+1))

-- Determina daca un sir de caractere corespunde unei Stari
matches :: [Char] -> [Char] -> State Char s -> Bool
matches [] bind End = True 
matches [] bind (Token c s s1 s2)
	| c == '*'  = True
	| otherwise = False
matches s bind End = False
matches (x:xs) bind (Token c s s1 s2)
	| c == '[' && (not (squaredTruth s x)) = False   
	| c == '(' || c == ')' = (matches (x:xs) bind s1)
	| c == '|' = (matches (x:xs) bind s1) || (matches (x:xs) bind s2)
	| (c == '*' && probe > 0 && bind == []) ||  
	  (c == '*' && probe > 0 && bind /= [] && (take probe (x:xs)) == bind) = 
	  									(matches (removeFirstN (x:xs) probe) 
										(take probe (x:xs)) {- [] -} (Token c [] s1 s2))
										
	| (c == '*' && probe > 0 && bind /= [] && (take probe (x:xs)) /= bind) = 
										(matches (x:xs) [] s2)
											 
	| c == '*' && probe < 0 = (matches (x:xs) [] s2)
	| c /= x && c /= '(' && c /= ')' && 
	  c /= '*' && c /= '.' && c /= '[' = False
	| otherwise = (matches xs bind s1) 
	 	where probe = (sampleMatches (x:xs) s1 0)
	 	
-- Pentru o expresie regulata data si un string returneaza 
-- NoMatch sau Match stringul dat
-- Primul parametru este un regex al doilea este stringul 
-- pe care dorim sa facem match  
myRegex :: [Char] -> [Char] -> Result [Maybe [Char]]
myRegex reg str
	| (matches str [] (plain reg End)) == True = Match [Nothing]
	| otherwise = NoMatch







