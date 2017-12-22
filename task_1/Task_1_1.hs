-- Task 1.1

data BinaryOperatorType = Addition -- binary operation
			| Subtraction 
			| Multiplication deriving(Show,Eq)

data UnaryOperatorType = UnarySubtraction deriving(Show,Eq)-- unary operation

data Term = IntConstant{ intValue :: Int }    
            | Variable{ varName :: String }
            | BinaryTerm{ opBinary :: BinaryOperatorType, lhv :: Term, rhv :: Term } 
            | UnaryTerm{ opUnary :: UnaryOperatorType, hv :: Term } deriving(Show,Eq)
			
-- Binary Addition
infixl 3 <+>
(<+>) (IntConstant a) (IntConstant b) = IntConstant (a + b)
(<+>) a b = BinaryTerm Addition a b

-- Binary Subtraction
infixl 3 <->
(<->) (IntConstant a) (IntConstant b) = IntConstant (a - b)
(<->) a b = BinaryTerm Subtraction a b

-- Binary Multiplication
infixl 4 <*>
(<*>) (IntConstant a) (IntConstant b) = IntConstant (a * b)
(<*>) a b = BinaryTerm Multiplication a b

-- Binary Subtraction
infixl 5 <-->
(<-->) (IntConstant a) = IntConstant (-a)
(<-->) a = UnaryTerms UnarySubtraction a

replaceVar :: Term -> String -> Term -> Term
replaceVar (IntConstant const) _ _ = IntConstant const
replaceVar (Variable var) findVar newTerm = if var == findVar then newTerm else Variable var
replaceVar (BinaryTerm op l r) findVar newTerm = BinaryTerm op (replaceVar l findVar newTerm) (replaceVar r findVar newTerm)
replaceVar (UnaryTerm op v) findVar newTerm = UnaryTerm op (replaceVar v findVar newTerm)
