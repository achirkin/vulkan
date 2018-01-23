module Main where

import ProcessVkXml
import Path.IO

main :: IO ()
main = do
  vkXml <- resolveFile' "../vulkan-docs/src/spec/vk.xml"
  outDir <- resolveDir' "../vulkan-api/src-gen"
  processVkXmlFile vkXml outDir
