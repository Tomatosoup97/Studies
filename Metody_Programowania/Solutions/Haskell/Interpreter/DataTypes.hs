{-# LANGUAGE Safe #-}
module DataTypes where

type ErrorMessage = String

data TypeCheckResult p
  = Ok
  -- Error on position p with message ErrorMessage
  | Error p ErrorMessage
  deriving (Eq, Show)

data EvalResult
  = Value Integer
  | RuntimeError
  deriving (Eq, Show)

data ProgramSource
  = SrcString String
  | SrcFile   FilePath
  deriving (Eq, Show)

data TestAnswer
  = TypeError
  | Eval [Integer] EvalResult
  deriving (Eq, Show)

data Test = Test
  { testName    :: String
  , testProgram :: ProgramSource
  , testAnswer  :: TestAnswer -- Expected result
  }
  deriving (Eq, Show)
