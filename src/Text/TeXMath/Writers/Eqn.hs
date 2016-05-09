{-# LANGUAGE GeneralizedNewtypeDeriving, ViewPatterns, GADTs #-}
{-
Copyright (C) 2014 Matthew Pickering <matthewtpickering@gmail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

-}

module Text.TeXMath.Writers.Eqn (writeEqn) where

import Data.List (intercalate)
import Data.Char (isAscii, ord)
import Text.Printf (printf)
import Text.TeXMath.Types
import Text.TeXMath.Unicode.ToUnicode (fromUnicode)
import qualified Text.TeXMath.Shared as S
import Data.Generics (everywhere, mkT)
import Control.Applicative ((<$>), Applicative)
import Control.Monad (when, unless, foldM_)
import Control.Monad.Reader (MonadReader, runReader, Reader, asks, local)
import Control.Monad.Writer( MonadWriter, WriterT,
                             execWriterT, tell, censor)
import Data.Ratio ((%))

-- import Debug.Trace
-- tr' x = trace (show x) x

-- | Transforms an expression tree to equivalent Eqn with the default
-- packages (amsmath and amssymb)
writeEqn :: [Exp] -> String
writeEqn = intercalate " " . map writeExp

-- like writeExp but inserts {} if contents contain a space
writeExp' :: Exp -> String
writeExp' e = if ' ' `elem` s
                 then "{" ++ s ++ "}"
                 else s
               where s = writeExp e

writeExp :: Exp -> String
writeExp (ENumber s) = s
writeExp (EGrouped es) = "{" ++ writeEqn es ++ "}"
writeExp (EDelimited open close es) =
  "left " ++ mbQuote open ++ " " ++ intercalate " " (map fromDelimited es) ++
  " right " ++ mbQuote close
  where fromDelimited (Left e)  = "\"" ++ e ++ "\""
        fromDelimited (Right e) = writeExp e
        mbQuote "" = "\"\""
        mbQuote s  = s
writeExp (EMathOperator s) =
  if s `elem` ["sin", "cos", "tan", "sinh", "cosh",
               "tanh", "arc", "max", "min", "lim",
               "log", "ln", "exp"]
     then s
     else "\"" ++ s ++ "\""
writeExp (ESymbol Ord [c])  -- do not render "invisible operators"
  | c `elem` ['\x2061'..'\x2064'] = "" -- see 3.2.5.5 of mathml spec
writeExp (EIdentifier s) = writeExp (ESymbol Ord s)
writeExp (ESymbol t s) =
  case s of
    "\8805" -> ">="
    "\8804" -> "<="
    "\8801" -> "=="
    "\8800" -> "!="
    "\177"  -> "+-"
    "\8594" -> "->"
    "\8592" -> "<-"
    "\8810" -> "<<"
    "\8811" -> ">>"
    "\8734" -> "inf"
    "\8706" -> "partial"
    "\189"  -> "half"
    "\8242" -> "prime"
    "\8776" -> "approx"
    "\183"  -> "cdot"
    "\215"  -> "times"
    "\8711" -> "grad"
    "\8230" -> "..."
    "\8721" -> "sum"
    "\8747" -> "int"
    "\8719" -> "prod"
    "\8898" -> "union"
    "\8899" -> "inter"
    "\945" -> "alpha"
    "\946" -> "beta"
    "\967" -> "chi"
    "\948" -> "delta"
    "\916" -> "DELTA"
    "\1013" -> "epsilon"
    "\951" -> "eta"
    "\947" -> "gamma"
    "\915" -> "GAMMA"
    "\953" -> "iota"
    "\954" -> "kappa"
    "\955" -> "lambda"
    "\923" -> "LAMBDA"
    "\956" -> "mu"
    "\957" -> "nu"
    "\969" -> "omega"
    "\937" -> "OMEGA"
    "\981" -> "phi"
    "\966" -> "varphi"
    "\934" -> "PHI"
    "\960" -> "pi"
    "\928" -> "PI"
    "\968" -> "psi"
    "\936" -> "PSI"
    "\961" -> "rho"
    "\963" -> "sigma"
    "\931" -> "SIGMA"
    "\964" -> "tau"
    "\952" -> "theta"
    "\920" -> "THETA"
    "\965" -> "upsilon"
    "\933" -> "UPSILON"
    "\958" -> "xi"
    "\926" -> "XI"
    "\950" -> "zeta"
    _ | all isAscii s -> s
    otherwise -> "\\[" ++ intercalate " " (map toUchar s) ++ "]"
  where toUchar c = printf "u%04x" (ord c)
writeExp (ESpace d) =
  case d of
      _ | d > 0 && d < (2 % 9) -> "^"
        | d >= (2 % 9) && d < (3 % 9) -> "~"
        | d < 0     -> "back " ++ show (floor $ -1 * d * 100)
        | otherwise -> "fwd " ++ show (floor $ d * 100)
writeExp (ESpace width) =
  let halfs = floor $ width * 9
  in  replicate (halfs `div` 2) '~' ++ replicate (halfs `mod` 2) '^'
writeExp (EFraction fractype e1 e2) = writeExp' e1 ++ op ++ writeExp' e2
  where op = if fractype == NoLineFrac
                then " / "
                else " over "
writeExp (ESub b e1) = writeExp' b ++ " sub " ++ writeExp' e1
writeExp (ESuper b e1) = writeExp' b ++ " sup " ++ writeExp' e1
writeExp (ESubsup b e1 e2) =
  writeExp' b ++ " sub " ++ writeExp' e1 ++ " sup " ++ writeExp' e2
writeExp (EOver convertible b e1) = undefined
writeExp (EUnder convertible b e1) = undefined
writeExp (EUnderover convertible b e1@(ESymbol Accent _) e2) =
 writeExp (EUnder convertible (EOver False b e2) e1)
writeExp (EUnderover convertible b e1 e2@(ESymbol Accent _)) =
 writeExp (EOver convertible (EUnder False b e1) e2)
writeExp (EUnderover convertible b e1 e2)
  = writeExp (EUnder convertible (EOver convertible b e2) e1)
writeExp (ESqrt e) = "sqrt " ++ writeExp' e
writeExp (ERoot i e) = undefined
writeExp (EPhantom e) = undefined
writeExp (EBoxed e) = undefined
writeExp (EScaled size e) = undefined
writeExp (EText ttype s) =
  let quoted = "\"" ++ s ++ "\""
  in case ttype of
       TextNormal -> "roman " ++ quoted
       TextItalic -> quoted
       TextBold   -> "bold " ++ quoted
       TextBoldItalic -> "bold italic " ++ quoted
       _   -> quoted
writeExp (EStyled ttype es) =
  let contents = "{" ++ writeEqn es ++ "}"
  in case ttype of
       TextNormal -> "roman " ++ contents
       TextItalic -> "italic " ++ contents
       TextBold   -> "bold " ++ contents
       TextBoldItalic -> "bold italic " ++ contents
       _   -> contents
writeExp (EArray [AlignRight, AlignLeft] rows) = undefined
writeExp (EArray aligns rows) = undefined

table :: String -> [Alignment] -> [ArrayLine] -> String
table name aligns rows = undefined

