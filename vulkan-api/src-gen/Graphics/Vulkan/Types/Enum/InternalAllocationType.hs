{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.InternalAllocationType
       (VkInternalAllocationType(VkInternalAllocationType,
                                 VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE))
       where
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkInternalAllocationType VkInternalAllocationType registry at www.khronos.org>
newtype VkInternalAllocationType = VkInternalAllocationType Int32
                                     deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkInternalAllocationType where
        showsPrec _ VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE
          = showString "VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE"
        showsPrec p (VkInternalAllocationType x)
          = showParen (p >= 11)
              (showString "VkInternalAllocationType " . showsPrec 11 x)

instance Read VkInternalAllocationType where
        readPrec
          = parens
              (choose
                 [("VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE",
                   pure VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE)]
                 +++
                 prec 10
                   (expectP (Ident "VkInternalAllocationType") >>
                      (VkInternalAllocationType <$> step readPrec)))

pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE ::
        VkInternalAllocationType

pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE =
        VkInternalAllocationType 0
