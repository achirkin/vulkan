{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
module Write.Feature
  ( genFeature
  ) where

import           Control.Monad                        (forM_)
import           Control.Monad.Reader.Class
import qualified Data.Map                             as Map
import           Data.Semigroup
import qualified Data.Text                            as T

import           VkXml.CommonTypes
import           VkXml.Sections
import           VkXml.Sections.Types as Ts
import           VkXml.Sections.Commands as Cs
import           VkXml.Sections.Feature

import           Write.Commands
import           Write.ModuleWriter
import           Write.Types



genFeature :: Monad m => ModuleWriter m ()
genFeature = pushSecLvl $ \curlvl -> do
    vkXml <- ask
    let VkFeature {..} = unInorder $ globFeature vkXml
        tps = Map.fromList
               . map (\t -> ((Ts.name :: VkType -> VkTypeName) t, t))
               . items . types . unInorder $ globTypes vkXml
        cmds = Map.fromList
               . map (\c -> ((Cs.name :: VkCommand -> VkCommandName) c, c))
               . commands . unInorder $ globCommands vkXml
    writeSection curlvl $ T.unlines
      [ comment
      , ""
      , "@api = " <> api <> "@"
      , ""
      , "@name = " <> name <> "@"
      , ""
      , "@number = " <> number <> "@"
      ]
    pushSecLvl $ \lvl -> mapM_ (genRequire lvl tps cmds) (baseReq:reqList)


genRequire :: Monad m
          => Int
          -> Map.Map VkTypeName VkType
          -> Map.Map VkCommandName VkCommand
          -> VkRequire
          -> ModuleWriter m ()
genRequire curlvl tps cmds VkRequire {..} = do
  writeSection curlvl comment
  forM_ requireTypes $ \tname -> case Map.lookup tname tps of
        Nothing -> pure ()
        Just t  -> genType t
  forM_ requireComms $ \cname -> case Map.lookup cname cmds of
        Nothing -> pure ()
        Just t  -> genCommand t

baseReq :: VkRequire
baseReq = VkRequire
  { comment      = "Not referenced, but required definitions"
  , requireTypes = map VkTypeName
     [ ]
  , requireEnums = []
  , requireComms = []
  }
