{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Html where

import qualified Data.Text.Lazy as Text

-- We could use a template language instead

listify :: [Text.Text] -> Text.Text
listify elems =
  Text.intercalate "\n" list
  where
    elemsList :: [Text.Text] =
      map
        (\e -> Text.append "<li>" $ Text.append e "</li>")
        elems
    list = ["<ul>"] ++ elemsList ++ ["</ul>"]
