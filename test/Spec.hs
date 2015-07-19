{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} Ebitor.AATreeTest

main = htfMain htf_importedTests
