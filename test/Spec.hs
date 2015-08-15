import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} Ebitor.AATreeTest
import {-@ HTF_TESTS @-} Ebitor.Rope.RegexTest
import {-@ HTF_TESTS @-} Ebitor.RopeTest

main = htfMain htf_importedTests
