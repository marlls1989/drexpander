module Main (main) where

import           Control.Monad.Reader       (runReader)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.List                  (isInfixOf, isPrefixOf)
import qualified Data.Set                   as Set

import           DRExpander
import qualified Language.Verilog           as V

import           Test.Tasty
import           Test.Tasty.Golden          (goldenVsString)
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck      as QC

opts :: PrgOptions
opts = PrgOptions { verilogFiles = [], resetName = "reset", clkName = "clk" }

parse1 :: String -> V.Module
parse1 = head . V.parseFile [] "<test>"

-- | Equality assertion for values without a 'Show' instance (e.g. 'Wire').
infix 1 @=?=
(@=?=) :: Eq a => a -> a -> Assertion
x @=?= y = assertBool "values are not equal" (x == y)

main :: IO ()
main = defaultMain $ testGroup "drexpander"
  [ wireTests
  , introspectionTests
  , adaptorTests
  , processTests
  , properties
  , goldenTests
  ]

--------------------------------------------------------------------------------
-- Pure bus / wire helpers
--------------------------------------------------------------------------------

wireTests :: TestTree
wireTests = testGroup "bus expansion"
  [ testCase "expandBusWireName appends _idx" $
      expandBusWireName "a" 3 @?= "a_3"

  , testCase "bitBlastWire on a scalar wire is identity" $
      bitBlastWire (Wire "x") @=?= [Wire "x"]

  , testCase "bitBlastWire on a [3:0] bus expands low..high" $
      bitBlastWire (Bus 3 0 "a")
        @=?= [Wire "a_0", Wire "a_1", Wire "a_2", Wire "a_3"]

  , testCase "bitBlastWire is orientation-independent ([0:3] == [3:0])" $
      bitBlastWire (Bus 0 3 "a") @=?= bitBlastWire (Bus 3 0 "a")

  , testCase "vlogDRWirePort yields _t/_f/_ack" $
      vlogDRWirePort "a" @?= ["a_t", "a_f", "a_ack"]

  , testCase "wireName projects the name" $ do
      wireName (Wire "n")    @?= "n"
      wireName (Bus 7 0 "m") @?= "m"
  ]

--------------------------------------------------------------------------------
-- AST introspection over a parsed module
--------------------------------------------------------------------------------

introspectionTests :: TestTree
introspectionTests = testGroup "module introspection"
  [ testCase "inputs (scalar + bus) collected as Wire/Bus" $
      vlogModuleInputs m @=?= Set.fromList [Wire "a", Bus 1 0 "b"]

  , testCase "outputs collected" $
      vlogModuleOutputs m @=?= Set.fromList [Wire "y"]

  , testCase "declared wires collected" $
      vlogModuleWires m @=?= Set.fromList [Wire "w"]

  , testCase "internal wires exclude ports" $
      vlogModuleIntWires m @=?= Set.fromList [Wire "w"]

  , testCase "withoutWires strips port/wire declarations" $
      let kept = case vlogModuleWithoutWires m of V.Module _ _ is -> is; _ -> []
          decls =  [() | V.Input  _ _ <- kept]
                ++ [() | V.Output _ _ <- kept]
                ++ [() | V.Wire   _ _ <- kept]
      in decls @?= []
  ]
  where
    m = parse1 "module m (a, b, y); input a; input [1:0] b; output y; wire w; endmodule"

--------------------------------------------------------------------------------
-- Dual-rail IO adaptor emission
--------------------------------------------------------------------------------

adaptorTests :: TestTree
adaptorTests = testGroup "dual-rail adaptors"
  [ testCase "scalar input adaptor declares _t/_f in, _ack out, drinput inst" $ do
      let rendered = unlines (map show (vlogDRWireInputInst (Wire "a")))
      assertBool "_t/_f input"  ("input  a_t, a_f;" `isInfixOf` rendered)
      assertBool "_ack output"  ("output a_ack;"    `isInfixOf` rendered)
      assertBool "drinput inst" ("drinput"          `isInfixOf` rendered)

  , testCase "scalar output adaptor declares _t/_f out, _ack in, droutput inst" $ do
      let rendered = unlines (map show (vlogDRWireOutputInst (Wire "y")))
      assertBool "_t/_f output"  ("output y_t, y_f;" `isInfixOf` rendered)
      assertBool "_ack input"    ("input  y_ack;"    `isInfixOf` rendered)
      assertBool "droutput inst" ("droutput"         `isInfixOf` rendered)

  , testCase "adaptor binds the t/f/ack/drw ports" $ do
      let rendered = show (vlogDRAdaptor "drinput" "a" Nothing)
      mapM_ (\p -> assertBool (p ++ " port") (p `isInfixOf` rendered))
            [".t(", ".f(", ".ack(", ".drw("]

  , testCase "bus input adaptor emits one drinput per bit" $ do
      let rendered = unlines (map show (vlogDRWireInputInst (Bus 1 0 "b")))
      length (filter (isInfixOf "drinput") (lines rendered)) @?= 2
  ]

--------------------------------------------------------------------------------
-- End-to-end processModule (Reader pipeline)
--------------------------------------------------------------------------------

processed :: V.Module
processed = runReader (processModule (parse1 design)) opts
  where
    design = "module d (clk, reset, a, y); input clk, reset; input a; "
          ++ "output y; wire w; assign w = a; dff r0 (.q(y), .d(w)); endmodule"

processTests :: TestTree
processTests = testGroup "processModule"
  [ testCase "module port list carries dual-rail + clk/reset" $
      let ports = case processed of V.Module _ ps _ -> ps; _ -> []
      in mapM_ (\p -> assertBool (p ++ " in port list") (p `elem` ports))
               ["a_t", "a_f", "a_ack", "y_t", "y_f", "y_ack", "clk", "reset"]

  , testCase "emits drinput and droutput instances" $ do
      let r = show processed
      assertBool "drinput"  ("drinput"  `isInfixOf` r)
      assertBool "droutput" ("droutput" `isInfixOf` r)

  , testCase "dff gets a reset pin injected" $
      assertBool ".reset(reset) on dff" (".reset(reset)" `isInfixOf` show processed)

  , testCase "continuous assign is rewritten to a buff instance" $
      assertBool "buff instance from assign" ("buff" `isInfixOf` show processed)
  ]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

properties :: TestTree
properties = testGroup "properties"
  [ QC.testProperty "bitBlastWire bus width == |hi-lo|+1" $
      \(QC.NonNegative hi) (QC.NonNegative lo) ->
        length (bitBlastWire (Bus (toInteger (hi :: Int)) (toInteger (lo :: Int)) "s"))
          == abs (hi - lo) + 1

  , QC.testProperty "every blasted bit keeps the base name as a prefix" $
      \(QC.NonNegative hi) (QC.NonNegative lo) ->
        let base = "busname"
            bits = bitBlastWire (Bus (toInteger (hi :: Int)) (toInteger (lo :: Int)) base)
        in all (\w -> (base ++ "_") `isPrefixOf` wireName w) bits

  , QC.testProperty "dual-rail port set is exactly _t/_f/_ack" $
      \s -> not (null (s :: String)) QC.==>
        vlogDRWirePort s == [s ++ "_t", s ++ "_f", s ++ "_ack"]
  ]

--------------------------------------------------------------------------------
-- Golden: full processed module text (mirrors `drexpander design.v`)
-- Generate/update with:  stack test --test-arguments=--accept
--------------------------------------------------------------------------------

goldenTests :: TestTree
goldenTests = testGroup "golden"
  [ goldenVsString "design.v processed" "test/golden/design.processed.v" $ do
      s <- readFile "test/fixtures/design.v"
      let ms  = V.parseFile [] "design.v" s
          out = concatMap (show . (\mm -> runReader (processModule mm) opts)) ms
      pure (BL.pack out)
  ]
