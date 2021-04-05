{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VendorId
       (VkVendorId(VkVendorId, VK_VENDOR_ID_VIV, VK_VENDOR_ID_VSI,
                   VK_VENDOR_ID_KAZAN, VK_VENDOR_ID_CODEPLAY, VK_VENDOR_ID_MESA))
       where
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (Int32)
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVendorId VkVendorId registry at www.khronos.org>
newtype VkVendorId = VkVendorId Int32
                     deriving (Eq, Ord, Enum, Storable)

instance Show VkVendorId where
    showsPrec _ VK_VENDOR_ID_VIV = showString "VK_VENDOR_ID_VIV"
    showsPrec _ VK_VENDOR_ID_VSI = showString "VK_VENDOR_ID_VSI"
    showsPrec _ VK_VENDOR_ID_KAZAN = showString "VK_VENDOR_ID_KAZAN"
    showsPrec _ VK_VENDOR_ID_CODEPLAY
      = showString "VK_VENDOR_ID_CODEPLAY"
    showsPrec _ VK_VENDOR_ID_MESA = showString "VK_VENDOR_ID_MESA"
    showsPrec p (VkVendorId x)
      = showParen (p >= 11) (showString "VkVendorId " . showsPrec 11 x)

instance Read VkVendorId where
    readPrec
      = parens
          (choose
             [("VK_VENDOR_ID_VIV", pure VK_VENDOR_ID_VIV),
              ("VK_VENDOR_ID_VSI", pure VK_VENDOR_ID_VSI),
              ("VK_VENDOR_ID_KAZAN", pure VK_VENDOR_ID_KAZAN),
              ("VK_VENDOR_ID_CODEPLAY", pure VK_VENDOR_ID_CODEPLAY),
              ("VK_VENDOR_ID_MESA", pure VK_VENDOR_ID_MESA)]
             +++
             prec 10
               (expectP (Ident "VkVendorId") >> (VkVendorId <$> step readPrec)))

-- | Vivante vendor ID
pattern VK_VENDOR_ID_VIV :: VkVendorId

pattern VK_VENDOR_ID_VIV = VkVendorId 65537

-- | VeriSilicon vendor ID
pattern VK_VENDOR_ID_VSI :: VkVendorId

pattern VK_VENDOR_ID_VSI = VkVendorId 65538

-- | Kazan Software Renderer
pattern VK_VENDOR_ID_KAZAN :: VkVendorId

pattern VK_VENDOR_ID_KAZAN = VkVendorId 65539

-- | Codeplay Software Ltd. vendor ID
pattern VK_VENDOR_ID_CODEPLAY :: VkVendorId

pattern VK_VENDOR_ID_CODEPLAY = VkVendorId 65540

-- | Mesa vendor ID
pattern VK_VENDOR_ID_MESA :: VkVendorId

pattern VK_VENDOR_ID_MESA = VkVendorId 65541
