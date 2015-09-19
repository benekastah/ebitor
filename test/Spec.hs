{-# OPTIONS_GHC -F -pgmF htfpp #-}
import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} Ebitor.Rope.CursorTest
import {-@ HTF_TESTS @-} Ebitor.Rope.RegexTest
import {-@ HTF_TESTS @-} Ebitor.RopeTest
import {-@ HTF_TESTS @-} Ebitor.WindowTest

main = htfMain htf_importedTests
