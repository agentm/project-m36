module ProjectM36.AtomFunctions.SelfTest where
import ProjectM36.Base
import ProjectM36.AtomFunctionError
import qualified Data.HashSet as HS
import Control.Concurrent (threadDelay)
import System.IO.Unsafe (unsafePerformIO)
import Crypto.KDF.BCrypt (bcrypt)
import qualified Data.Text.Encoding as TE
import Debug.Trace

-- functions which should only exist for testing Project:M36
selfTestAtomFunctions :: AtomFunctions
selfTestAtomFunctions = HS.fromList [
  Function { funcName = "test_expensive" --returns the first argument after pausing X microseconds in the second argument to simulate a time-consuming function
           , funcType = [TypeVariableType "a", IntegerAtomType, TypeVariableType "a"]
           , funcBody = FunctionBuiltInBody (
             \case
               atom:(IntegerAtom microseconds):_ ->
                 unsafePerformIO $ do
                   traceShowM ("test_expensive threadDelay"::String, fromIntegral microseconds::Int)
                   threadDelay (fromIntegral microseconds)
                   pure (Right atom)
               _ -> Left AtomFunctionTypeMismatchError)
           , funcACL = ()
           },
    Function { funcName = "test_bcrypt", -- pass a value through but calculate something expensive, not for actual encryption use since it uses a fixed seed
               funcType = [IntegerAtomType, TextAtomType, TypeVariableType "a", TypeVariableType "a"],
               funcBody = FunctionBuiltInBody (
                 \case
                   (IntegerAtom costVal):(TextAtom plaintextPassword):atom:_ ->
                     let hashed = bcrypt (fromIntegral costVal) (TE.encodeUtf8 "1234567890123456") (TE.encodeUtf8 plaintextPassword) in
                       Right (ByteStringAtom hashed `seq` atom)
                   _ -> Left AtomFunctionTypeMismatchError)
             , funcACL = ()
             }
  ]
