{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TypeFamilies, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Models
  ( Todo(..)
  , TodoId
  , migrateAll
  ) where

import Database.Persist.TH
import Data.Text (Text)
import GHC.Generics (Generic)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Todo
    title Text
    done  Bool
    deriving Show Generic

User
    username Text
    deriving Show Generic
|]
