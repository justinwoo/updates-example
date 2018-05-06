module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (foldl)
import Data.Record as Record
import Data.Record.Builder (Builder)
import Data.Record.Builder as Builder
import Data.Variant (class VariantMatchCases, Variant, inj, match)
import Type.Prelude (class IsSymbol, class RowLacks, class RowToList, RLProxy(..), SProxy(..))
import Type.Row (Cons, Nil, kind RowList)

vrUpdate
  :: forall row rl matches ml
   . RowToList row rl
  => RowToList matches ml
  => VariantRecordMatch rl row () matches
  => VariantMatchCases ml row ({ | row } -> { | row })
  => Union row () row
  => Variant row
  -> { | row }
  -> { | row }
vrUpdate v = match matches v
  where
    matches :: { | matches }
    matches = Builder.build (vrRecordMatchImpl (RLProxy :: RLProxy rl)) {}

class VariantRecordMatch (rl :: RowList) (row :: # Type) (i :: # Type) (o :: # Type)
  | rl -> row i o where
  vrRecordMatchImpl :: RLProxy rl -> Builder { | i } { | o }

instance nilVRU :: VariantRecordMatch Nil row () () where
  vrRecordMatchImpl _ = id

instance consVRU ::
  ( IsSymbol name
  , VariantRecordMatch tail row from from'
  , RowCons name ty row' row
  , RowCons name (ty -> { | row } -> { | row }) from' to
  , RowLacks name from'
  ) => VariantRecordMatch (Cons name ty tail) row from to where
  vrRecordMatchImpl _ =
    let
      nameP = SProxy :: SProxy name
      update :: ty -> { | row } -> { | row }
      update x r = Record.set nameP x r
      first :: Builder { | from' } { | to }
      first = Builder.insert nameP update
      rest :: Builder { | from } { | from' }
      rest = vrRecordMatchImpl (RLProxy :: RLProxy tail)
    in
      first <<< rest

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
