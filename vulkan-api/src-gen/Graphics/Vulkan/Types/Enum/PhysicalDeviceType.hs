{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.PhysicalDeviceType
       (VkPhysicalDeviceType(VkPhysicalDeviceType,
                             VK_PHYSICAL_DEVICE_TYPE_OTHER,
                             VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU,
                             VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU,
                             VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU, VK_PHYSICAL_DEVICE_TYPE_CPU))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceType VkPhysicalDeviceType registry at www.khronos.org>
newtype VkPhysicalDeviceType = VkPhysicalDeviceType Int32
                                 deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkPhysicalDeviceType where
        showsPrec _ VK_PHYSICAL_DEVICE_TYPE_OTHER
          = showString "VK_PHYSICAL_DEVICE_TYPE_OTHER"
        showsPrec _ VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
          = showString "VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU"
        showsPrec _ VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
          = showString "VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU"
        showsPrec _ VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU
          = showString "VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU"
        showsPrec _ VK_PHYSICAL_DEVICE_TYPE_CPU
          = showString "VK_PHYSICAL_DEVICE_TYPE_CPU"
        showsPrec p (VkPhysicalDeviceType x)
          = showParen (p >= 11)
              (showString "VkPhysicalDeviceType " . showsPrec 11 x)

instance Read VkPhysicalDeviceType where
        readPrec
          = parens
              (choose
                 [("VK_PHYSICAL_DEVICE_TYPE_OTHER",
                   pure VK_PHYSICAL_DEVICE_TYPE_OTHER),
                  ("VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU",
                   pure VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU),
                  ("VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU",
                   pure VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU),
                  ("VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU",
                   pure VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU),
                  ("VK_PHYSICAL_DEVICE_TYPE_CPU", pure VK_PHYSICAL_DEVICE_TYPE_CPU)]
                 +++
                 prec 10
                   (expectP (Ident "VkPhysicalDeviceType") >>
                      (VkPhysicalDeviceType <$> step readPrec)))

pattern VK_PHYSICAL_DEVICE_TYPE_OTHER :: VkPhysicalDeviceType

pattern VK_PHYSICAL_DEVICE_TYPE_OTHER = VkPhysicalDeviceType 0

pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU ::
        VkPhysicalDeviceType

pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU =
        VkPhysicalDeviceType 1

pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU ::
        VkPhysicalDeviceType

pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU =
        VkPhysicalDeviceType 2

pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU :: VkPhysicalDeviceType

pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU =
        VkPhysicalDeviceType 3

pattern VK_PHYSICAL_DEVICE_TYPE_CPU :: VkPhysicalDeviceType

pattern VK_PHYSICAL_DEVICE_TYPE_CPU = VkPhysicalDeviceType 4
