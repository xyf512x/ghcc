data Var = Var {varNum::Int, includeFun::Fun, free::Bool} deriving (Show,Read,Eq,Ord)
--变元与项，带有内置函数
var :: Int -> Var
var a = Var a FunSelf True

con :: Int -> Var
con a = Var a Con True
--创建常元
data Fun = Con | FunSelf | Fun {funNum::Int, useVar::[Var]} deriving (Show,Read,Eq,Ord)
--函数，包括常元函数和自身函数
fun :: Int -> [Var] -> Fun
fun a b = Fun a b

fun2var :: Int -> Fun -> Var
fun2var a b = Var a b True
--函数转变为项
data Rel = Rel {relNum::Int, includeVar::[Var]} | No { norel::Rel} | Imply {rel1::Rel, rel2::Rel} | Univer {uniVar::Var, uniRel::Rel} deriving (Show,Read,Eq,Ord)
--谓词，包括运算符
changefree :: Var -> [Var] -> [Var]
changefree a b 
 | b == [] = b
 | head b == a && includeFun (head b) == FunSelf = (Var (varNum a) FunSelf False) : (changefree a (tail b))
 | head b /= a && includeFun (head b) /= FunSelf && includeFun (head b) /= Con = (Var (varNum (head b)) (Fun (funNum (includeFun (head b))) (changefree a (useVar (includeFun (head b))))) True) : (changefree a (tail b))
 | otherwise = (head b) : (changefree a (tail b))
--计算是否自由
univer :: Var -> Rel -> Rel
univer a (Rel b c) = Univer (Var (varNum a) (includeFun a) False) (Rel b (changefree a c))
univer a (No b) = Univer (Var (varNum a) (includeFun a) False) (No (uniRel (univer a b)))
univer a (Imply b c) = Univer (Var (varNum a) (includeFun a) False) (Imply (uniRel (univer a b)) (uniRel (univer a c)))
univer a (Univer b c) = Univer (Var (varNum a) (includeFun a) False) (univer b (uniRel (univer a c)))
--在进行量词计算时修改自由
rel :: Int -> [Var] -> Rel
rel a b = Rel a b

no :: Rel -> Rel
no a = No a

imply :: Rel -> Rel -> Rel
imply a b = Imply a b

replace :: Rel -> Var -> Var -> Rel
replace (Rel a b) c d = Rel a [if varNum x==varNum c && includeFun x == includeFun c then d else x | x <- b]
replace (Univer a b) c d = univer a (replace b c d)
replace (No a) b c = No (replace a b c)
replace (Imply a b) c d = Imply (replace a c d) (replace b c d)
--替换项(非替换变元)

getVars :: Rel -> [Var]
getVars (Rel a b)
 | b == [] = []
 | includeFun (head b) == FunSelf || includeFun (head b) == Con = (head b) : getVars (Rel a (tail b))
 | otherwise = (head b) : getVars (Rel a (useVar (includeFun (head b)))) ++ getVars (Rel a (tail b))

getVars (Univer a b) = a:(getVars b)
getVars (No a) = getVars a
getVars (Imply a b) = getVars a ++ getVars b



isFree :: Rel -> Var -> Var -> Bool
isFree (Rel a b) c d 
 | free c == False = False
 | otherwise = True

isFree (Univer a b) c d 
 | isFree b c d == False = False
 | elem c (getVars b) == False = True
 | elem (Var (varNum a) (includeFun a) True) (getVars (Rel 1 [d])) == True = False
 | otherwise = True

isFree (No a) c d= isFree a c d
isFree (Imply a b) c d = (isFree a c d) && (isFree b c d)




