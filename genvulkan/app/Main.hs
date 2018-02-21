module Main where

import ProcessVkXml
import Path.IO

main :: IO ()
main = do
  inVkH <- resolveFile' "../vulkan-docs/src/vulkan/vulkan.h"
  outVkH <- resolveFile' "../vulkan-api/include/vulkan/vulkan.h"
  processVulkanHFile inVkH outVkH

  vkXml <- resolveFile' "../vulkan-docs/src/spec/vk.xml"
  outDir <- resolveDir' "../vulkan-api/src-gen"
  outCabalFile <- resolveFile' "../vulkan-api/vulkan-api.cabal"
  processVkXmlFile vkXml outDir outCabalFile
