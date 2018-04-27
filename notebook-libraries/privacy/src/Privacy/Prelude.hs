module Privacy.Prelude
  (module PrivacyPrelude
  ,ifThenElse)
  where

import           Algebra.Bounded                         as PrivacyPrelude
import           Algebra.Rig                             as PrivacyPrelude
import           Algebra.Ring                            as PrivacyPrelude
import           Algebra.Semirig                         as PrivacyPrelude
import           Control.Applicative                     as PrivacyPrelude
import           Control.Monad                           as PrivacyPrelude
import           Data.Bool                               as PrivacyPrelude (bool)
import           Data.Coerce                             as PrivacyPrelude
import           Data.Coerce.Utilities                   as PrivacyPrelude
import           Data.Foldable                           as PrivacyPrelude
import           Data.Function                           as PrivacyPrelude
import           Data.List.NonEmpty                      as PrivacyPrelude (NonEmpty (..))
import           Data.Ord                                as PrivacyPrelude
import           Data.Ratio                              as PrivacyPrelude
import           Data.Semigroup                          as PrivacyPrelude
import           Data.Semigroup.Foldable                 as PrivacyPrelude
import           Data.Traversable                        as PrivacyPrelude
import           Data.String.NonEmpty                    as PrivacyPrelude
import           Data.List.NonEmpty.Static               as PrivacyPrelude
import           Graphics.Rendering.Chart.Plot.Instances as PrivacyPrelude ()
import           Numeric.Literals                        as PrivacyPrelude
import           Numeric.Natural                         as PrivacyPrelude
import           Prelude                                 as PrivacyPrelude hiding
                                                                            (Bounded (..),
                                                                            Num (..),
                                                                            fromRational)

ifThenElse :: Bool -> a -> a -> a
ifThenElse True t _  = t
ifThenElse False _ f = f
