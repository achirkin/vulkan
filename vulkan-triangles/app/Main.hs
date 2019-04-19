module Main (main) where

import Lib

main :: IO ()
-- demos: Squares or Chalet. Chalet needs obj and texture files from https://vulkan-tutorial.com/Loading_models
main = runVulkanProgram Squares
