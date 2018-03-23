{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkShaderInfoTypeAMD
       (VkShaderInfoTypeAMD(VkShaderInfoTypeAMD,
                            VK_SHADER_INFO_TYPE_STATISTICS_AMD, VK_SHADER_INFO_TYPE_BINARY_AMD,
                            VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD))
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkShaderInfoTypeAMD.html VkShaderInfoTypeAMD registry at www.khronos.org>
newtype VkShaderInfoTypeAMD = VkShaderInfoTypeAMD Int32
                                deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkShaderInfoTypeAMD where
        showsPrec _ VK_SHADER_INFO_TYPE_STATISTICS_AMD
          = showString "VK_SHADER_INFO_TYPE_STATISTICS_AMD"
        showsPrec _ VK_SHADER_INFO_TYPE_BINARY_AMD
          = showString "VK_SHADER_INFO_TYPE_BINARY_AMD"
        showsPrec _ VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD
          = showString "VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD"
        showsPrec p (VkShaderInfoTypeAMD x)
          = showParen (p >= 11)
              (showString "VkShaderInfoTypeAMD " . showsPrec 11 x)

instance Read VkShaderInfoTypeAMD where
        readPrec
          = parens
              (choose
                 [("VK_SHADER_INFO_TYPE_STATISTICS_AMD",
                   pure VK_SHADER_INFO_TYPE_STATISTICS_AMD),
                  ("VK_SHADER_INFO_TYPE_BINARY_AMD",
                   pure VK_SHADER_INFO_TYPE_BINARY_AMD),
                  ("VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD",
                   pure VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD)]
                 +++
                 prec 10
                   (expectP (Ident "VkShaderInfoTypeAMD") >>
                      (VkShaderInfoTypeAMD <$> step readPrec)))

pattern VK_SHADER_INFO_TYPE_STATISTICS_AMD :: VkShaderInfoTypeAMD

pattern VK_SHADER_INFO_TYPE_STATISTICS_AMD = VkShaderInfoTypeAMD 0

pattern VK_SHADER_INFO_TYPE_BINARY_AMD :: VkShaderInfoTypeAMD

pattern VK_SHADER_INFO_TYPE_BINARY_AMD = VkShaderInfoTypeAMD 1

pattern VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD :: VkShaderInfoTypeAMD

pattern VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD = VkShaderInfoTypeAMD 2
