{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.DriverId
       (VkDriverId(VkDriverId, VK_DRIVER_ID_AMD_PROPRIETARY,
                   VK_DRIVER_ID_AMD_OPEN_SOURCE, VK_DRIVER_ID_MESA_RADV,
                   VK_DRIVER_ID_NVIDIA_PROPRIETARY,
                   VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS,
                   VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA,
                   VK_DRIVER_ID_IMAGINATION_PROPRIETARY,
                   VK_DRIVER_ID_QUALCOMM_PROPRIETARY, VK_DRIVER_ID_ARM_PROPRIETARY,
                   VK_DRIVER_ID_GOOGLE_SWIFTSHADER, VK_DRIVER_ID_GGP_PROPRIETARY,
                   VK_DRIVER_ID_BROADCOM_PROPRIETARY, VK_DRIVER_ID_MESA_LLVMPIPE,
                   VK_DRIVER_ID_MOLTENVK),
        VkDriverIdKHR(..))
       where
import Data.Bits                       (Bits, FiniteBits)
import Data.Coerce                     (coerce)
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (Int32)
import Graphics.Vulkan.Types.BaseTypes (VkFlags)
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkDriverId VkDriverId registry at www.khronos.org>
newtype VkDriverId = VkDriverId Int32
                     deriving (Eq, Ord, Enum, Storable)

instance Show VkDriverId where
    showsPrec _ VK_DRIVER_ID_AMD_PROPRIETARY
      = showString "VK_DRIVER_ID_AMD_PROPRIETARY"
    showsPrec _ VK_DRIVER_ID_AMD_OPEN_SOURCE
      = showString "VK_DRIVER_ID_AMD_OPEN_SOURCE"
    showsPrec _ VK_DRIVER_ID_MESA_RADV
      = showString "VK_DRIVER_ID_MESA_RADV"
    showsPrec _ VK_DRIVER_ID_NVIDIA_PROPRIETARY
      = showString "VK_DRIVER_ID_NVIDIA_PROPRIETARY"
    showsPrec _ VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS
      = showString "VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS"
    showsPrec _ VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA
      = showString "VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA"
    showsPrec _ VK_DRIVER_ID_IMAGINATION_PROPRIETARY
      = showString "VK_DRIVER_ID_IMAGINATION_PROPRIETARY"
    showsPrec _ VK_DRIVER_ID_QUALCOMM_PROPRIETARY
      = showString "VK_DRIVER_ID_QUALCOMM_PROPRIETARY"
    showsPrec _ VK_DRIVER_ID_ARM_PROPRIETARY
      = showString "VK_DRIVER_ID_ARM_PROPRIETARY"
    showsPrec _ VK_DRIVER_ID_GOOGLE_SWIFTSHADER
      = showString "VK_DRIVER_ID_GOOGLE_SWIFTSHADER"
    showsPrec _ VK_DRIVER_ID_GGP_PROPRIETARY
      = showString "VK_DRIVER_ID_GGP_PROPRIETARY"
    showsPrec _ VK_DRIVER_ID_BROADCOM_PROPRIETARY
      = showString "VK_DRIVER_ID_BROADCOM_PROPRIETARY"
    showsPrec _ VK_DRIVER_ID_MESA_LLVMPIPE
      = showString "VK_DRIVER_ID_MESA_LLVMPIPE"
    showsPrec _ VK_DRIVER_ID_MOLTENVK
      = showString "VK_DRIVER_ID_MOLTENVK"
    showsPrec p (VkDriverId x)
      = showParen (p >= 11) (showString "VkDriverId " . showsPrec 11 x)

instance Read VkDriverId where
    readPrec
      = parens
          (choose
             [("VK_DRIVER_ID_AMD_PROPRIETARY",
               pure VK_DRIVER_ID_AMD_PROPRIETARY),
              ("VK_DRIVER_ID_AMD_OPEN_SOURCE",
               pure VK_DRIVER_ID_AMD_OPEN_SOURCE),
              ("VK_DRIVER_ID_MESA_RADV", pure VK_DRIVER_ID_MESA_RADV),
              ("VK_DRIVER_ID_NVIDIA_PROPRIETARY",
               pure VK_DRIVER_ID_NVIDIA_PROPRIETARY),
              ("VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS",
               pure VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS),
              ("VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA",
               pure VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA),
              ("VK_DRIVER_ID_IMAGINATION_PROPRIETARY",
               pure VK_DRIVER_ID_IMAGINATION_PROPRIETARY),
              ("VK_DRIVER_ID_QUALCOMM_PROPRIETARY",
               pure VK_DRIVER_ID_QUALCOMM_PROPRIETARY),
              ("VK_DRIVER_ID_ARM_PROPRIETARY",
               pure VK_DRIVER_ID_ARM_PROPRIETARY),
              ("VK_DRIVER_ID_GOOGLE_SWIFTSHADER",
               pure VK_DRIVER_ID_GOOGLE_SWIFTSHADER),
              ("VK_DRIVER_ID_GGP_PROPRIETARY",
               pure VK_DRIVER_ID_GGP_PROPRIETARY),
              ("VK_DRIVER_ID_BROADCOM_PROPRIETARY",
               pure VK_DRIVER_ID_BROADCOM_PROPRIETARY),
              ("VK_DRIVER_ID_MESA_LLVMPIPE", pure VK_DRIVER_ID_MESA_LLVMPIPE),
              ("VK_DRIVER_ID_MOLTENVK", pure VK_DRIVER_ID_MOLTENVK)]
             +++
             prec 10
               (expectP (Ident "VkDriverId") >> (VkDriverId <$> step readPrec)))

-- | Advanced Micro Devices, Inc.
pattern VK_DRIVER_ID_AMD_PROPRIETARY :: VkDriverId

pattern VK_DRIVER_ID_AMD_PROPRIETARY = VkDriverId 1

-- | Advanced Micro Devices, Inc.
pattern VK_DRIVER_ID_AMD_OPEN_SOURCE :: VkDriverId

pattern VK_DRIVER_ID_AMD_OPEN_SOURCE = VkDriverId 2

-- | Mesa open source project
pattern VK_DRIVER_ID_MESA_RADV :: VkDriverId

pattern VK_DRIVER_ID_MESA_RADV = VkDriverId 3

-- | NVIDIA Corporation
pattern VK_DRIVER_ID_NVIDIA_PROPRIETARY :: VkDriverId

pattern VK_DRIVER_ID_NVIDIA_PROPRIETARY = VkDriverId 4

-- | Intel Corporation
pattern VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS :: VkDriverId

pattern VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS = VkDriverId 5

-- | Intel Corporation
pattern VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA :: VkDriverId

pattern VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA = VkDriverId 6

-- | Imagination Technologies
pattern VK_DRIVER_ID_IMAGINATION_PROPRIETARY :: VkDriverId

pattern VK_DRIVER_ID_IMAGINATION_PROPRIETARY = VkDriverId 7

-- | Qualcomm Technologies, Inc.
pattern VK_DRIVER_ID_QUALCOMM_PROPRIETARY :: VkDriverId

pattern VK_DRIVER_ID_QUALCOMM_PROPRIETARY = VkDriverId 8

-- | Arm Limited
pattern VK_DRIVER_ID_ARM_PROPRIETARY :: VkDriverId

pattern VK_DRIVER_ID_ARM_PROPRIETARY = VkDriverId 9

-- | Google LLC
pattern VK_DRIVER_ID_GOOGLE_SWIFTSHADER :: VkDriverId

pattern VK_DRIVER_ID_GOOGLE_SWIFTSHADER = VkDriverId 10

-- | Google LLC
pattern VK_DRIVER_ID_GGP_PROPRIETARY :: VkDriverId

pattern VK_DRIVER_ID_GGP_PROPRIETARY = VkDriverId 11

-- | Broadcom Inc.
pattern VK_DRIVER_ID_BROADCOM_PROPRIETARY :: VkDriverId

pattern VK_DRIVER_ID_BROADCOM_PROPRIETARY = VkDriverId 12

-- | Mesa
pattern VK_DRIVER_ID_MESA_LLVMPIPE :: VkDriverId

pattern VK_DRIVER_ID_MESA_LLVMPIPE = VkDriverId 13

-- | MoltenVK
pattern VK_DRIVER_ID_MOLTENVK :: VkDriverId

pattern VK_DRIVER_ID_MOLTENVK = VkDriverId 14

newtype VkDriverIdKHR = VkDriverIdKHR VkFlags
                        deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkDriverIdKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkDriverIdKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
