{-#OPTIONS_GHC -fno-warn-tabs #-}
module Imp where
import Prelude
import Data.List

-- IMP: Lenguaje imperativo minimal


-- Expresiones -- 

data Exp = V String | C String [Exp]
	deriving Show

-- Programas -- 

-- Rama
type Rama = (String, [String], Instr)

-- Programa / Instruccion

data Instr = Asig [String] [Exp] | Sec Instr Instr | Sel String [Rama] | Rep String [Rama]
	deriving Show 

-- Valores --

data Valor = Val String [Valor]
	deriving Show

-- Memoria --

type Mem = [(String, Valor)]

-- vacia
emptyM :: Mem
emptyM = [] -- Null [] <= Null = [] <= [] [] <= []:[] <= [[]]

-- actualizar 
actualizarM :: Mem -> [(String, Valor)] -> Mem
actualizarM mem [] = mem
actualizarM mem (xv:xvs) = xv:actualizarM mem xvs -- se guardan los cambios al principio

-- buscar
buscar :: Mem -> String -> Valor
buscar mem x = case lookup x mem of
{
	Just value -> value;
	Nothing -> error "La variable ingresada no existe en la memoria"
}

-- Evaluacion de expresiones --

evaluacionExp :: Mem -> Exp -> Valor
evaluacionExp mem (V x) = buscar mem x
evaluacionExp mem (C x es) = Val x (map (evaluacionExp mem) es)	

-- Ejecucion de programas / instrucciones --
			
ejecutarInstruccion :: Mem -> Instr -> Mem
ejecutarInstruccion mem (Asig xs es) = actualizarM mem (zip xs (map (evaluacionExp mem) es))
ejecutarInstruccion mem (Sel x rms) = case (buscar mem x) of 
{
	(Val y vls) -> case (lookupTriple y rms) of 
	{
		Nothing -> error "no existe una variable en ninguna de las ramas";
		Just (xs, i) -> ejecutarInstruccion (actualizarM mem (zip xs vls)) i
	}
}
ejecutarInstruccion mem (Sec i1 i2) = ejecutarInstruccion (ejecutarInstruccion mem i1) i2
ejecutarInstruccion mem (Rep x rms) = case (buscar mem x) of
{
	(Val y vls) -> case (lookupTriple y rms) of
	{
		Nothing -> mem;
		Just (xs, i) -> ejecutarInstruccion (ejecutarInstruccion (actualizarM mem (zip xs vls)) i) 
			(Rep x (drop (head (elemIndices y (map (primElemTerna) rms))) rms))
	}
}	

-- lookup para ternas
lookupTriple :: Eq a => a -> [(a, b, c)] -> Maybe (b, c)
lookupTriple w [] = Nothing
lookupTriple w ((x, ys, z):ls) |w == x = Just (ys, z)
							   |otherwise = lookupTriple w ls

-- devuelve el primer elemento de una terna
primElemTerna :: (a,b,c) -> a
primElemTerna (x,y,z) = x

-- Codificacion de programas --

--Negación booleana: (b := !b)
--Case b of {False → b := True |
--			 True → b := False}

-- Negacion booleana
notImp :: Instr
notImp = Sel "b"[
	("True", [], Asig ["b"] [C "False" []]),
	("False", [], Asig ["b"] [C "True" []])
]
--------------------------
--Par: (b := Par(n))
--m := n;
--b := true;
--while m do {S[x] → m:=x ; (b:=!b)}

-- Evaluacion de paridad sin modificar el numero
parImp :: Instr
parImp = Sec (Asig ["b", "m"] [C "True" [], V "n"])
		     (Rep ("m") [("S", ["x"], Sec (Asig ["m"] [V "x"]) 
										   notImp)])
--------------------------
--Length: (n := Length(l))
--lt := l;
--n := 0;
--while lt do {:[x, xs] → lt:= [xs] ; (n := S n)}

-- Largo de una lista sin modificarla
lengthImp :: Instr
lengthImp = Sec (Asig ["n", "lt"] [C "O" [], V "l"])
			    (Rep ("lt") [(":", ["x", "xs"], Sec (Asig ["lt"] [V "xs"]) 
												    (Asig ["n"] [C "S" [V "n"]]))])
--------------------------
--Reverse: (l := Reverse(l))
--lr := [];
--while l do {:[x, xs] → l:= [xs] ; (lr := :[x, lr])};
--l := lr

-- Invertir una lista
reverseImp :: Instr
reverseImp = Sec (Sec (Asig ["lr"] [C "Null" []]) 
			          (Rep ("l") [(":", ["x", "xs"], Sec (Asig ["l"] [V "xs"]) 
													     (Asig ["lr"] [C ":" [V "x", V "lr"]]))]))
				 (Asig ["l"] [V "lr"])										 
--------------------------
-- booleanos en Imp

true :: Valor
true = Val "True" []

false :: Valor
false = Val "False" []

-- numeros en Imp

cero :: Valor -- O
cero = Val "O" []

uno :: Valor -- S O
uno = Val "S" [cero]	

dos :: Valor -- S (S O)
dos = Val "S" [uno]

tres :: Valor -- S(S(S O))
tres = Val "S" [dos]

cuatro :: Valor
cuatro = Val "S" [tres]

cinco :: Valor
cinco =  Val "S" [cuatro]

-- listas en Imp

listaVacia :: Valor -- []
listaVacia = Val "Null" []

lista3 :: Valor -- [S(S(S O))] = S(S(S O)):[]
lista3 =  Val ":" [tres, listaVacia]

lista23 :: Valor -- [S(S O), S(S(S O))] = S(S O):S(S(S O)):[]
lista23 = Val ":" [dos, lista3]

lista123 :: Valor -- [S O, S(S O), S(S(S O))] = S O:S(S O):S(S(S O)):[] 
lista123 = Val ":" [uno, lista23]

lista0123 :: Valor -- [O, S O, S(S O), S(S(S O))] = O:S O:S(S O):S(S(S O)):[]
lista0123 = Val ":" [cero, lista123]
--------------------------
-- Pruebas --	

evalNotImp :: Mem -> Mem
evalNotImp m = ejecutarInstruccion m notImp

-- evalNotImp [("b", true)] -- Ejecucion: [("b",Val "False" []),("b",Val "True" [])]
-- es decir b = False

evalParImp :: Mem -> Mem
evalParImp m = ejecutarInstruccion m parImp

-- evalParImp [("n", tres)] -- Ejecucion: [("b",Val "False" []),("m",Val "O" []),("x",Val "O" []),("b",Val "True" []),
-- 										   ("m",Val "S" [Val "O" []]),("x",Val "S" [Val "O" []]),("b",Val "False" []),
-- 										   ("m",Val "S" [Val "S" [Val "O" []]]),("x",Val "S" [Val "S" [Val "O" []]]),("b",Val "True" []),
-- 										   ("m",Val "S" [Val "S" [Val "S" [Val "O" []]]]),("n",Val "S" [Val "S" [Val "S" [Val "O" []]]])]
-- es decir n es False

evalLenghtImp :: Mem -> Mem
evalLenghtImp m = ejecutarInstruccion m lengthImp

-- evalLenghtImp [("l", lista123)] -- Ejecucion: [("n",Val "S" [Val "S" [Val "S" [Val "O" []]]]),("lt",Val "Null" []),("x",Val "S" [Val "S" [Val "S" [Val "O" []]]]),
--                                                ("xs",Val "Null" []),("n",Val "S" [Val "S" [Val "O" []]]), ("lt",Val ":" [Val "S" [Val "S" [Val "S" [Val "O" []]]],Val "Null" []]),
--                                                ("x",Val "S" [Val "S" [Val "O" []]]),("xs",Val ":" [Val "S" [Val "S" [Val "S" [Val "O" []]]],
--                                                 Val "Null" []]),("n",Val "S" [Val "O" []]),("lt",Val ":" [Val "S" [Val "S" [Val "O" []]],
--                                                 Val ":" [Val "S" [Val "S" [Val "S" [Val "O" []]]],Val "Null" []]]),("x",Val "S" [Val "O" []]),
--                                                ("xs",Val ":" [Val "S" [Val "S" [Val "O" []]],Val ":" [Val "S" [Val "S" [Val "S" [Val "O" []]]],Val "Null" []]]),
--                                                ("n",Val "O" []),("lt",Val ":" [Val "S" [Val "O" []],Val ":" [Val "S" [Val "S" [Val "O" []]],
--                                                 Val ":" [Val "S" [Val "S" [Val "S" [Val "O" []]]],Val "Null" []]]]),("l",Val ":" [Val "S" [Val "O" []],
-- 												   Val ":" [Val "S" [Val "S" [Val "O" []]],Val ":" [Val "S" [Val "S" [Val "S" [Val "O" []]]],Val "Null" []]]])]
-- es decir el largo de l es S(S(S O)) = 3

evalReverseImp :: Mem -> Mem
evalReverseImp m = ejecutarInstruccion m reverseImp

-- evalReverseImp [("l", lista0123)] -- Ejecucion: [("l",Val ":" [Val "S" [Val "S" [Val "S" [Val "O" []]]],
--													Val ":" [Val "S" [Val "S" [Val "O" []]],Val ":" [Val "S" [Val "O" []],Val ":" [Val "O" [],Val "Null" []]]]]),
--													("lr",Val ":" [Val "S" [Val "S" [Val "S" [Val "O" []]]],Val ":" [Val "S" [Val "S" [Val "O" []]],Val ":" [Val "S" [Val "O" []],
--													Val ":" [Val "O" [],Val "Null" []]]]]),("l",Val "Null" []),("x",Val "S" [Val "S" [Val "S" [Val "O" []]]]),
--													("xs",Val "Null" []),("lr",Val ":" [Val "S" [Val "S" [Val "O" []]],Val ":" [Val "S" [Val "O" []],
--													Val ":" [Val "O" [],Val "Null" []]]]),("l",Val ":" [Val "S" [Val "S" [Val "S" [Val "O" []]]],Val "Null" []]),
--													("x",Val "S" [Val "S" [Val "O" []]]),("xs",Val ":" [Val "S" [Val "S" [Val "S" [Val "O" []]]],Val "Null" []]),
--													("lr",Val ":" [Val "S" [Val "O" []],Val ":" [Val "O" [],Val "Null" []]]),("l",Val ":" [Val "S" [Val "S" [Val "O" []]],
--													Val ":" [Val "S" [Val "S" [Val "S" [Val "O" []]]],Val "Null" []]]),("x",Val "S" [Val "O" []]),
--													("xs",Val ":" [Val "S" [Val "S" [Val "O" []]],Val ":" [Val "S" [Val "S" [Val "S" [Val "O" []]]],Val "Null" []]]),
--													("lr",Val ":" [Val "O" [],Val "Null" []]),("l",Val ":" [Val "S" [Val "O" []],Val ":" [Val "S" [Val "S" [Val "O" []]],
--													Val ":" [Val "S" [Val "S" [Val "S" [Val "O" []]]],Val "Null" []]]]),("x",Val "O" []),("xs",Val ":" [Val "S" [Val "O" []],
--													Val ":" [Val "S" [Val "S" [Val "O" []]],Val ":" [Val "S" [Val "S" [Val "S" [Val "O" []]]],
--													Val "Null" []]]]),("lr",Val "Null" []),("l",Val ":" [Val "O" [],Val ":" [Val "S" [Val "O" []],
--													Val ":" [Val "S" [Val "S" [Val "O" []]],Val ":" [Val "S" [Val "S" [Val "S" [Val "O" []]]],Val "Null" []]]]])] 
-- es decir l = [S(S(S O)),S(S O),S O, O] 