{-# LANGUAGE NoImplicitPrelude #-}

module Abstract.Main(
    module Abstract.Base
) where

import Abstract.Base hiding (
    Bool(..), BoolInt(..), Int(..),
    BBBi(..), BBUn(..), BIBi(..), IIBi(..), IIUn(..))
import Abstract.Base (Bool(True, False), Int())
import Abstract.Util()
