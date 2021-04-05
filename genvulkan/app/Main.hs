module Main where

import ProcessVkXml
import Path
import Path.IO
import qualified System.Process.Typed as P

main :: IO ()
main = do
  vulkanDocsFolder <- resolveDir' "../vulkan-docs"
  vulkanDocsXmlFolder <- (vulkanDocsFolder </>) <$> parseRelDir "xml"
  vulkanDocsIncludes <- (vulkanDocsFolder </>) <$> parseRelDir "gen/include/vulkan"

  -- generate headers
  P.runProcess_ $ P.setWorkingDir (toFilePath vulkanDocsXmlFolder) (P.proc "make" ["install", "test"])

  outVkHFolder <- resolveDir' "../vulkan-api/include/vulkan"
  removeDirRecur outVkHFolder
  createDir outVkHFolder

  (_, fnames ) <- listDir vulkanDocsIncludes
  mapM_
    (\inVkH -> copyFile inVkH (outVkHFolder </> filename inVkH))
    (filter ((".h" == ) . fileExtension) fnames)

  vkXml <- resolveFile' "../vulkan-docs/xml/vk.xml"
  outDir <- resolveDir' "../vulkan-api/src-gen"
  outCabalFile <- resolveFile' "../vulkan-api/vulkan-api.cabal"
  processVkXmlFile vkXml outDir outCabalFile
