module Servises.Impl.MD5.Internal where

--
import Data.Char (isDigit)

idAdmFromToken :: String -> Maybe (String, String)
idAdmFromToken tok =
  case takeWhile isDigit tok of
    [] -> Nothing
    xs -> Just (xs, adm)
  where
    adm =
      case dropWhile isDigit tok of
        ('.':'1':_) -> "1"
        _ -> "0"

timeFromToken :: String -> String
timeFromToken tok =
  case dropWhile isDigit tok of
    ('.':_:xs) -> take 14 xs
    _ -> []
