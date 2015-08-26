module Ebitor.Edit
    ( Editor(..)
    ) where

import Ebitor.Rope

data Editor = Editor
    { filePath :: Maybe FilePath
    , rope :: Rope
    , cursor :: Cursor
    }
