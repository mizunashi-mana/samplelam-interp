module Language.SampleLam.Syntax where

import SampleLam.Prelude

import Data.HigherOrder
import Data.HFunctor.Cofree
import Data.HFunctor.OpenUnion


data AstTag
  = ExprTag
  | DeclTag
  | VarTag
  | LitTag
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)


type AstF = '[ExprF, DeclF, VarF, LitF]
type AstWithAnnF = HCofreeF (HUnion AstF)

injectAstF :: Member AstF f => f r :~> AstWithAnnF (Const ()) r
injectAstF = Nat (HCofreeF $ Const ()) . injectU


data ExprF r i where
  LamAbsF :: [r 'VarTag] -> r 'ExprTag -> ExprF r 'ExprTag
  AppF :: r 'ExprTag -> [r 'ExprTag] -> ExprF r 'ExprTag
  InfixAppF :: r 'ExprTag -> r 'VarTag -> r 'ExprTag -> ExprF r 'ExprTag
  LetF :: [r 'DeclTag] -> r 'ExprTag -> ExprF r 'ExprTag
  IfF :: r 'ExprTag -> r 'ExprTag -> r 'ExprTag -> ExprF r 'ExprTag
  VarExprF :: r 'VarTag -> ExprF r 'ExprTag
  LitExprF :: r 'LitTag -> ExprF r 'ExprTag

deriving instance (forall i. Eq (r i)) => Eq (ExprF r j)
deriving instance (forall i. Ord (r i)) => Ord (ExprF r j)
deriving instance (forall i. Show (r i)) => Show (ExprF r j)

instance HFunctor ExprF where
  hfmap (Nat f) = Nat \case
    LamAbsF vs e      -> LamAbsF (f <$> vs) (f e)
    AppF e es         -> AppF (f e) (f <$> es)
    InfixAppF e1 v e2 -> InfixAppF (f e1) (f v) (f e2)
    LetF ds e         -> LetF (f <$> ds) (f e)
    IfF c e1 e2       -> IfF (f c) (f e1) (f e2)
    VarExprF v        -> VarExprF (f v)
    LitExprF l        -> LitExprF (f l)


data DeclF r i where
  DeclF :: r 'VarTag -> [r 'VarTag] -> r 'ExprTag -> DeclF r 'DeclTag

deriving instance (forall i. Eq (r i)) => Eq (DeclF r j)
deriving instance (forall i. Ord (r i)) => Ord (DeclF r j)
deriving instance (forall i. Show (r i)) => Show (DeclF r j)

instance HFunctor DeclF where
  hfmap (Nat f) = Nat \case
    DeclF v as e -> DeclF (f v) (f <$> as) (f e)


data VarF r i where
  VarF :: String -> VarF r 'VarTag

deriving instance Eq (VarF r j)
deriving instance Ord (VarF r j)
deriving instance Show (VarF r j)

instance HFunctor VarF where
  hfmap _ = Nat \case
    VarF v -> VarF v


data LitF r i where
  BoolLitF :: Bool -> LitF r 'LitTag
  IntLitF :: Integer -> LitF r 'LitTag

deriving instance Eq (LitF r j)
deriving instance Ord (LitF r j)
deriving instance Show (LitF r j)

instance HFunctor LitF where
  hfmap _ = Nat \case
    BoolLitF b -> BoolLitF b
    IntLitF  i -> IntLitF i
