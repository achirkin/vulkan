{-# LANGUAGE QuasiQuotes #-}
module Main where

import ProcessVkXml
import Path

main :: IO ()
main = processVkXmlFile
        [relfile|vulkan-docs/src/spec/vk.xml|]
        [reldir|out|]
