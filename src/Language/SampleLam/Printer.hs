{-# LANGUAGE OverloadedStrings #-}

module Language.SampleLam.Printer where

import SampleLam.Prelude

import Language.SampleLam.Syntax
import Data.Text.Prettyprint.Doc
import Data.HFunctor.Cofree
import Data.HFunctor.OpenUnion
import Data.HigherOrder
import Data.Constraint
import Data.Membership
import Data.HFunctor.Identity
import Data.HFunctor.Unsafe


newtype SourceOutput f r i = SourceOutput (f r i)


prettySyntax :: HFix (AstWithAnnF a) i -> Doc ann
prettySyntax = pretty . SourceOutput . HIdentity

instance (forall i. Pretty (r i)) => Pretty (SourceOutput ExprF r j) where
  pretty (SourceOutput m) = case m of
    LamAbsF vs e      -> "(" <> encloseSep "\\" "->" space (pretty <$> vs) <+> pretty e <> ")"
    AppF e es         -> "(" <> concatWith (<+>) (pretty <$> (e:es)) <> ")"
    InfixAppF e1 v e2 -> "(" <> pretty e1 <+> pretty v <+> pretty e2 <> ")"
    LetF ds e         -> "(" <> vsep ["let" <+> align (vsep $ pretty <$> ds), "in" <+> pretty e] <> ")"
    IfF c e1 e2       -> "(" <> "if" <+> pretty c <+> "then" <+> pretty e1 <+> "else" <+> pretty e2 <> ")"
    VarExprF v        -> pretty v
    LitExprF l        -> pretty l

instance (forall i. Pretty (r i)) => Pretty (SourceOutput DeclF r j) where
  pretty (SourceOutput m) = case m of
    DeclF v vs e -> concatWith (<+>) (pretty <$> (v:vs)) <+> "=" <+> pretty e

instance (forall i. Pretty (r i)) => Pretty (SourceOutput VarF r j) where
  pretty (SourceOutput m) = case m of
    VarF s -> pretty s

instance (forall i. Pretty (r i)) => Pretty (SourceOutput LitF r j) where
  pretty (SourceOutput m) = case m of
    BoolLitF b -> pretty b
    IntLitF  i -> pretty i

instance (forall i. Pretty (r i)) => Pretty (SourceOutput (AstWithAnnF a) r j) where
  pretty (SourceOutput m) = case m of
    HCofreeF _ (HUnion n x) ->
      case toMemberCtxT @Pretty (SourceOutput x) n of Dict -> pretty $ SourceOutput x

instance (forall r j. (forall i. Pretty (r i)) => Pretty (SourceOutput f r j), HFunctor f)
    => Pretty (SourceOutput HIdentity (HFix f) k) where
  pretty (SourceOutput (HIdentity (HIn m))) = pretty . SourceOutput $ unNat toSourceOutputIdentity m
    where
      toSourceOutputIdentity :: f (HFix f) :~> f (SourceOutput HIdentity (HFix f))
      toSourceOutputIdentity = hfmapCoerce
