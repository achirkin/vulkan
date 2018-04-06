module Main where

import ProcessVkXml
import Path
import Path.IO

main :: IO ()
main = do
  inVkHFolder <- resolveDir' "../vulkan-docs/include/vulkan"
  outVkHFolder <- resolveDir' "../vulkan-api/include/vulkan"
  removeDirRecur outVkHFolder
  createDir outVkHFolder

  (_, fnames ) <- listDir inVkHFolder
  mapM_
    (\inVkH -> copyFile inVkH (outVkHFolder </> filename inVkH))
    (filter ((".h" == ) . fileExtension) fnames)

  vkXml <- resolveFile' "../vulkan-docs/xml/vk.xml"
  outDir <- resolveDir' "../vulkan-api/src-gen"
  outCabalFile <- resolveFile' "../vulkan-api/vulkan-api.cabal"
  processVkXmlFile vkXml outDir outCabalFile
