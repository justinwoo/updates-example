module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Record as Record
import Data.Variant (Variant, inj, prj)
import Type.Prelude (class IsSymbol, class RowToList, RLProxy(..), SProxy(..))
import Type.Row (Cons, Nil, kind RowList)

vrUpdate
  :: forall row rl
   . RowToList row rl
  => VariantRecordUpdate rl row
  => Variant row
  -> { | row }
  -> { | row }
vrUpdate = vrUpdateImpl (RLProxy :: RLProxy rl)

class VariantRecordUpdate (rl :: RowList) (r :: # Type) | rl -> r where
  vrUpdateImpl :: RLProxy rl -> Variant r -> { | r } -> { | r }

instance nilVRU :: VariantRecordUpdate Nil row where
  vrUpdateImpl _ _ r = r

instance consVRU ::
  ( IsSymbol name
  , RowCons name ty row' row
  , VariantRecordUpdate tail row
  ) => VariantRecordUpdate (Cons name ty tail) row where
  vrUpdateImpl _ v r =
    case prj nameP v of
      Just a -> Record.set nameP a r
      Nothing -> vrUpdateImpl (RLProxy :: RLProxy tail) v r
    where
      nameP = SProxy :: SProxy name

type MyRecordFields =
  ( apple :: Int
  , banana :: String
  , cherry :: Boolean
  )

type MyRecord = { | MyRecordFields }

type MyVariant = Variant MyRecordFields

data Query =
  Update MyVariant

queries ::  Array Query
queries = Update <$>
  [ inj (SProxy :: SProxy "apple") 123
  , inj (SProxy :: SProxy "banana") "hello"
  , inj (SProxy :: SProxy "cherry") true
  ]

initial :: MyRecord
initial =
  { apple: 0
  , banana: ""
  , cherry: false
  }

updateRecord :: MyRecord -> Query -> MyRecord
updateRecord r (Update v) = vrUpdate v r

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let result = foldl updateRecord initial queries

  log "before: " *> showState initial
  log "after: " *> showState result

  where
    showState r = do
      log $ "apple: " <> show r.apple
      log $ "banana: " <> show r.banana
      log $ "cherry: " <> show r.cherry
