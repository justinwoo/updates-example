module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (foldl)
import Data.Record as Record
import Data.Variant (Variant, case_, inj, on)
import Type.Prelude (class IsSymbol, class RowToList, RLProxy(RLProxy), SProxy(SProxy))
import Type.Row (Cons, Nil, kind RowList)

-- by monoidmusician: https://purescript-users.ml/t/generating-lenses-from-variants/54/7
class RecordVariantUpdate r where
  vrUpdate :: Variant r -> Record r -> Record r

instance recordVariantUpdate ::
  ( RowToList r rl
  , RecordVariantUpdateRL rl r r
  ) => RecordVariantUpdate r where
    vrUpdate = rvUpdateRL (RLProxy :: RLProxy rl)

class RecordVariantUpdateRL rl v r | rl -> v where
  rvUpdateRL :: RLProxy rl -> Variant v -> Record r -> Record r

instance rvUpdateNil :: RecordVariantUpdateRL Nil () r where
  rvUpdateRL _ = case_

instance rvUpdateCons ::
  ( IsSymbol s
  , RecordVariantUpdateRL rl v r
  , RowCons s t r' r
  , RowCons s t v v'
  ) => RecordVariantUpdateRL (Cons s t rl) v' r where
    rvUpdateRL _ =
      let s = SProxy :: SProxy s
      in on s (Record.set s) (rvUpdateRL (RLProxy :: RLProxy rl))

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
