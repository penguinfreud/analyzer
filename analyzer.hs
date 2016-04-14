data Ast = Block [Ast] | If Ast Ast | IfElse Ast Ast Ast | While Ast Ast | Assign LHS Ast | CompoundAssign LHS BinaryOp Ast | BinaryExpr Ast BinaryOp Ast | Lit Int | Primary LHS deriving (Show)
data LHS = Var String | Member LHS Ast deriving (Show)
data BinaryOp = Add | Mult | Divide | Less | Greater deriving (Show)

data AstCrumb = BlockCrumb [Ast] [Ast] | IfCrumb Int Ast | IfElseCrumb Int Ast Ast | WhileCrumb Int Ast | AssignCrumb LHS | CompoundAssignCrumb LHS BinaryOp | BinaryExprCrumb Int BinaryOp Ast deriving (Show)
type AstZipper = (Ast, [AstCrumb])

nthStmt :: Int -> AstZipper -> AstZipper
nthStmt n (Block body, xs) = (body !! n, BlockCrumb (take n body) (drop (n+1) body):xs)

gotoCond :: AstZipper -> AstZipper
gotoCond (If cond seq, xs) = (cond, IfCrumb 0 seq:xs)
gotoCond (IfElse cond seq alt, xs) = (cond, IfElseCrumb 0 seq alt:xs)
gotoCond (While cond body, xs) = (cond, WhileCrumb 0 body:xs)

gotoSeq :: AstZipper -> AstZipper
gotoSeq (If cond seq, xs) = (seq, IfCrumb 1 cond:xs)
gotoSeq (IfElse cond seq alt, xs) = (seq, IfElseCrumb 1 cond alt:xs)

gotoAlt :: AstZipper -> AstZipper
gotoAlt (IfElse cond seq alt, xs) = (alt, IfElseCrumb 2 cond seq:xs)

gotoBody :: AstZipper -> AstZipper
gotoBody (While cond body, xs) = (body, WhileCrumb 1 cond:xs)

gotoAssign :: AstZipper -> AstZipper
gotoAssign (Assign lhs rhs, xs) = (rhs, AssignCrumb lhs:xs)
gotoAssign (CompoundAssign lhs op rhs, xs) = (rhs, CompoundAssignCrumb lhs op:xs)

gotoFirstOperand :: AstZipper -> AstZipper
gotoFirstOperand (BinaryExpr first op second, xs) = (first, BinaryExprCrumb 0 op second:xs)

gotoSecondOperand :: AstZipper -> AstZipper
gotoSecondOperand (BinaryExpr first op second, xs) = (second, BinaryExprCrumb 1 op first:xs)

astUp :: AstZipper -> AstZipper
astUp (ast, BlockCrumb ls rs:xs) = (Block (ls ++ [ast] ++ rs), xs)
astUp (cond, IfCrumb 0 seq:xs) = (If cond seq, xs)
astUp (seq, IfCrumb 1 cond:xs) = (If cond seq, xs)
astUp (cond, IfElseCrumb 0 seq alt:xs) = (IfElse cond seq alt, xs)
astUp (seq, IfElseCrumb 1 cond alt:xs) = (IfElse cond seq alt, xs)
astUp (alt, IfElseCrumb 2 cond seq:xs) = (IfElse cond seq alt, xs)
astUp (cond, WhileCrumb 0 body:xs) = (While cond body, xs)
astUp (body, WhileCrumb 1 cond:xs) = (While cond body, xs)
astUp (rhs, AssignCrumb lhs:xs) = (Assign lhs rhs, xs)
astUp (rhs, CompoundAssignCrumb lhs op:xs) = (CompoundAssign lhs op rhs, xs)
astUp (first, BinaryExprCrumb 0 op second:xs) = (BinaryExpr first op second, xs)
astUp (second, BinaryExprCrumb 1 op first:xs) = (BinaryExpr first op second, xs)


