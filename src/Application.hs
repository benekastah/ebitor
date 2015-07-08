module Application
    ( Application(..)
    ) where

import Graphics.Vty.Widgets.All

data Application a b = EmptyApplication
                     | Application
                       { editor :: Widget a
                       , commander :: Widget b
                       , focusGroup :: Widget FocusGroup
                       , collection :: Collection
                       }
