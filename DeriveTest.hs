{-# LANGUAGE CPP #-}
{-# OPTIONS_DERIVE --output=file.h #-}
module DeriveTest where
import Data.DateTime
import Data.Binary
import Test.QuickCheck

#include "file.h"

data Color = RGB Int Int Int
           | CMYK Int Int Int Int
           deriving ({-! Arbitrary, Binary !-})
