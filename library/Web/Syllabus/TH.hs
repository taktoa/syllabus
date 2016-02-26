{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

-- | FIXME: doc
module Web.Syllabus.TH ( module Exported
                       , deriveClasses
                       , deriveClasses'
                       , deriveAll
                       ) where

import           Control.Lens.TH            as Exported
import           Data.Acid                  as Exported (makeAcidic)
import           Data.Default               as Exported (Default (..))
import           Data.SafeCopy              as Exported (base, deriveSafeCopy)

import           GHC.Generics               (Generic)

import           Control.Applicative
import           Data.Monoid

import           Data.Functor               ((<$>))

import           Control.Lens

import           Data.DeriveTH


import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Lens   as TH
import qualified Language.Haskell.TH.Syntax as TH

type Deriver = TH.Name -> TH.Q [TH.Dec]

class MonoidApp m where
  memptyApp  :: m
  mappendApp :: m -> m -> m

instance MonoidApp [m] where
  memptyApp  = []
  mappendApp = (++)

instance (MonoidApp m) => MonoidApp (a -> m) where
  memptyApp  = const memptyApp
  mappendApp = liftA2 mappendApp

instance (MonoidApp x) => MonoidApp (TH.Q x) where
  memptyApp  = pure memptyApp
  mappendApp = liftA2 mappendApp

(<@>) :: (MonoidApp m) => m -> m -> m
(<@>) = mappendApp

mconcatApp :: (Foldable f, MonoidApp m) => f m -> m
mconcatApp = foldr mappendApp memptyApp

data DataType = DataType { _ddClasses  :: [Derivation]
                         , _stdClasses :: [TH.Name]
                         }

makeLenses ''DataType

instance Default DataType where
  def = DataType { _ddClasses  = [ makeEq, makeShow, makeRead ]
                 , _stdClasses = [ ''Generic ]
                 }

deriveDD :: DataType -> Deriver
deriveDD cfg name = concat <$> mapM (`derive` name) (cfg ^. ddClasses)

deriveStd :: DataType -> Deriver
deriveStd cfg = mconcatApp $ map deriveStandalone $ cfg ^. stdClasses

deriveStandalone :: TH.Name -> Deriver
deriveStandalone tcN name = fmap pure
                            $ TH.standaloneDerivD (return [])
                            $ TH.appT tc (TH.conT name)
  where
    notTCError = "deriveStandalone: name was not a typeclass"
    tc = do info <- TH.reify tcN
            case info ^? TH._ClassI of Just _  -> TH.conT tcN
                                       Nothing -> fail notTCError

deriveClasses' :: DataType -> TH.Name -> TH.Q [TH.Dec]
deriveClasses' config =     deriveStd config
                        <@> deriveDD config
                        <@> deriveSafeCopy 0 'base
                        <@> makeLenses

deriveClasses :: TH.Name -> TH.Q [TH.Dec]
deriveClasses = deriveClasses' def

deriveAll :: [TH.Name] -> TH.Q [TH.Dec]
deriveAll names = mconcat <$> mapM deriveClasses names

