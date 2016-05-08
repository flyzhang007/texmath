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
import Text.TeXMath.Types
import Text.TeXMath.Unicode.ToUnicode (fromUnicode)
import qualified Text.TeXMath.Shared as S
import Data.Generics (everywhere, mkT)
import Control.Applicative ((<$>), Applicative)
import Control.Monad (when, unless, foldM_)
import Control.Monad.Reader (MonadReader, runReader, Reader, asks, local)
import Control.Monad.Writer( MonadWriter, WriterT,
                             execWriterT, tell, censor)
-- import Debug.Trace
-- tr' x = trace (show x) x

-- | Transforms an expression tree to equivalent Eqn with the default
-- packages (amsmath and amssymb)
writeEqn :: [Exp] -> String
writeEqn = intercalate " " . map writeExp

writeExp :: Exp -> String
writeExp (ENumber s) = undefined
writeExp (EGrouped es) = undefined
writeExp (EDelimited open close [Right (EArray aligns rows)]) = undefined
writeExp (EDelimited open close es) =  undefined
writeExp (EIdentifier s) = undefined
writeExp o@(EMathOperator s) = undefined
writeExp (ESymbol Ord [c])  -- do not render "invisible operators"
  | c `elem` ['\x2061'..'\x2064'] = "" -- see 3.2.5.5 of mathml spec
writeExp (ESymbol t s) = undefined
writeExp (ESpace width) = undefined
writeExp (EFraction fractype e1 e2) = undefined
writeExp (ESub b e1) = undefined
writeExp (ESuper b e1) = undefined
writeExp (ESubsup b e1 e2) = undefined
writeExp (EOver convertible b e1) = undefined
writeExp (EUnder convertible b e1) = undefined
writeExp (EUnderover convertible b e1@(ESymbol Accent _) e2) =
 writeExp (EUnder convertible (EOver False b e2) e1)
writeExp (EUnderover convertible b e1 e2@(ESymbol Accent _)) =
 writeExp (EOver convertible (EUnder False b e1) e2)
writeExp (EUnderover convertible b e1 e2)
  = writeExp (EUnder convertible (EOver convertible b e2) e1)
writeExp (ESqrt e) = undefined
writeExp (ERoot i e) = undefined
writeExp (EPhantom e) = undefined
writeExp (EBoxed e) = undefined
writeExp (EScaled size e) = undefined
writeExp (EText ttype s) = undefined
writeExp (EStyled ttype es) = undefined
writeExp (EArray [AlignRight, AlignLeft] rows) = undefined
writeExp (EArray aligns rows) = undefined

table :: String -> [Alignment] -> [ArrayLine] -> String
table name aligns rows = undefined

