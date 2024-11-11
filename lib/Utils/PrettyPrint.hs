module Utils.PrettyPrint where

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

data Style = Bold | Italic | Underline

instance Show Color where
  show Black   = "\x1b[30m"
  show Red     = "\x1b[31m"
  show Green   = "\x1b[32m"
  show Yellow  = "\x1b[33m"
  show Blue    = "\x1b[34m"
  show Magenta = "\x1b[35m"
  show Cyan    = "\x1b[36m"
  show White   = "\x1b[37m"

instance Show Style where
  show Bold      = "\x1b[1m"
  show Italic    = "\x1b[3m"
  show Underline = "\x1b[4m"

color :: Color -> String -> String
color cl str = show cl <> str <> "\x1b[0m"

style :: Style -> String -> String
style st str = show st <> str <> "\x1b[0m"
