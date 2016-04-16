data Ast = Ast {operator::Operator, operands::[Ast]} |
    Var {name::String} |
    Lit {value::Int} |
    Empty deriving (Show)
data Operator = If | While | DoWhile | Block | Break Int | Continue Int | Return | Member | Assign | Add | Sub | Mult | Divide deriving (Show)
data Type = NumType | Array Type | Function Type Type | Struct [(String, Type)] deriving (Show)

data AstCrumb = AstCrumb Operator [Ast] [Ast] deriving (Show)
type AstZipper = (Ast, [AstCrumb])

astFirst :: AstZipper -> AstZipper
astFirst x@(Empty, _) = x
astFirst x@(Var _, _) = x
astFirst x@(Lit _, _) = x
astFirst x@(Ast (Break _) _, _) = x
astFirst x@(Ast (Continue _) _, _) = x
astFirst x@(Ast Return _, _) = x
astFirst (Ast Assign [Ast Member [obj, index], rhs], xs) = astFirst (obj, AstCrumb Member [] [index]:AstCrumb Assign [] [rhs]:xs)
astFirst (Ast Assign [lhs, rhs], xs) = astFirst (rhs, AstCrumb Assign [lhs] []:xs)
astFirst (Ast operator (first:ys), xs) = astFirst (first, AstCrumb operator [] ys:xs)

reverseAppend :: [a] -> [a] -> [a]
reverseAppend [] rs = rs
reverseAppend (l:ls) rs = reverseAppend ls (l:rs)

astUp :: AstZipper -> AstZipper
astUp x@(ast, []) = x
astUp (ast, AstCrumb operator ls rs:xs) = (Ast operator (reverseAppend ls (ast:rs)), xs)

astUpN :: Int -> AstZipper -> AstZipper
astUpN 0 x = x
astUpN n x = astUpN (n-1) (astUp x)

astRoot :: AstZipper -> AstZipper
astRoot x@(_, []) = x
astRoot x = astRoot $ astUp x

-- cse :: AstZipper -> AstZipper
-- dce :: AstZipper -> AstZipper
hasNext :: AstZipper -> Bool
hasNext (_, []) = False
hasNext _ = True

getNext :: AstZipper -> [AstZipper]
getNext (cond, AstCrumb If [] [seq]:xs) = [
    astFirst (seq, AstCrumb If [cond] []:xs),
    (Ast If [cond, seq], xs)]
getNext (cond, AstCrumb If [] [seq, alt]:xs) = [
    astFirst (seq, AstCrumb If [cond] [alt]:xs),
    astFirst (alt, AstCrumb If [cond, seq] []:xs)]
getNext (seq, AstCrumb If [cond] [alt]:xs) = [(Ast If [cond, seq, alt], xs)]
getNext (alt, AstCrumb If [cond, seq] []:xs) = [(Ast If [cond, seq, alt], xs)]
getNext (cond, AstCrumb While [] [body]:xs) = [
    astFirst (body, AstCrumb While [cond] []:xs),
    (Ast While [cond, body], xs)]
getNext (body, AstCrumb While [cond] []:xs) = [astFirst (cond, AstCrumb While [] [body]:xs)]
getNext (body, AstCrumb DoWhile [] [cond]:xs) = [astFirst (cond, AstCrumb DoWhile [body] []:xs)]
getNext (cond, AstCrumb DoWhile [body] []:xs) = [
    astFirst (body, AstCrumb DoWhile [] [cond]:xs),
    (Ast DoWhile [body, cond], xs)]
getNext x@(Ast Return val, xs) = [astRoot x]
getNext x@(Ast (Break depth) _, xs) = [astUpN depth x]
getNext x@(Ast (Continue depth) _, xs) = let (block, ys) = astUpN depth x
                                         in case block of (Ast While [cond, body]) -> [astFirst (cond, AstCrumb While [] [body]:ys)]
                                                          (Ast DoWhile [body, cond]) -> [astFirst (cond, AstCrumb DoWhile [body] []:ys)]
getNext (x, AstCrumb op ls []:xs) = [(Ast op (reverseAppend ls [x]), xs)]
getNext (x, AstCrumb op ls (r:rs):xs) = [astFirst (r, AstCrumb op (x:ls) rs:xs)]

unrollLoop :: Int -> AstZipper -> AstZipper
unrollLoop 1 x = x
unrollLoop n (Ast While [cond, body], xs) | n > 1 = (Ast While [cond, repeatBody 1], xs)
    where repeatBody i | i == n = body
          repeatBody i = Ast Block [body, Ast If [cond, repeatBody (i+1), Ast (Break (i + i)) []]]

main = putStrLn "hello"
