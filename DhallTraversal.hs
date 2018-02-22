{-# LANGUAGE LambdaCase #-}

module DhallTraversal where

import Dhall.Core

subExpr
  :: Applicative f
  => ( Expr s a -> f ( Expr s a ) ) -> Expr s a -> f ( Expr s a )
subExpr f = \case
  Lam a b c ->
    Lam a <$> f b <*> f c

  Pi a b c ->
    Pi a <$> f b <*> f c

  App a b ->
    App <$> f a <*> f b

  Let a b c d ->
    Let a <$> traverse f b <*> f c <*> f d

  Annot a b ->
    Annot <$> f a <*> f b

  BoolAnd a b ->
    BoolAnd <$> f a <*> f b

  BoolOr a b ->
    BoolOr <$> f a <*> f b

  BoolEQ a b ->
    BoolEQ <$> f a <*> f b

  BoolNE a b ->
    BoolNE <$> f a <*> f b

  BoolIf a b c ->
    BoolIf <$> f a <*> f b <*> f c

  NaturalPlus a b ->
    NaturalPlus <$> f a <*> f b

  NaturalTimes a b ->
    NaturalTimes <$> f a <*> f b

  TextAppend a b ->
    TextAppend <$> f a <*> f b

  ListLit a b ->
    ListLit <$> traverse f a <*> traverse f b

  ListAppend a b ->
    ListAppend <$> f a <*> f b

  OptionalLit a b ->
    OptionalLit <$> f a <*> traverse f b

  Record a ->
    Record <$> traverse f a

  RecordLit a ->
    RecordLit <$> traverse f a

  Union a ->
    Union <$> traverse f a

  UnionLit a b c ->
    UnionLit a <$> f b <*> traverse f c

  Combine a b ->
    Combine <$> f a <*> f b

  Prefer a b ->
    Prefer <$> f a <*> f b

  Merge a b t ->
    Merge <$> f a <*> f b <*> traverse f t

  Field a b ->
    Field <$> f a <*> pure b

  Note a b ->
    Note a <$> f b

  e ->
    pure e
