module Main (main) where

import Lib

main :: IO ()
-- demos: Squares or VikingRoom. VikingRoom needs obj and texture files from https://vulkan-tutorial.com/Loading_models
-- (Chalet also works, for the files from older versions of the vulkan tutorial)
main = runVulkanProgram Squares
