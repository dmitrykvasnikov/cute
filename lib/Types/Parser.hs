module Types.Parser where

type ParserPos = Int

data Precedence = Default | Comma | Assignment | Logical | Relational | Additive | Multiplicative | Unary | Call | Member | Primary deriving
  ( Enum
  , Eq
  , Ord
  , Show
  )
