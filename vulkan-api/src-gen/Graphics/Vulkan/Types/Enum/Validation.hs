{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.Validation
       (VkValidationCacheHeaderVersionEXT(VkValidationCacheHeaderVersionEXT,
                                          VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT),
        VkValidationCheckEXT(VkValidationCheckEXT,
                             VK_VALIDATION_CHECK_ALL_EXT, VK_VALIDATION_CHECK_SHADERS_EXT),
        VkValidationFeatureDisableEXT(VkValidationFeatureDisableEXT,
                                      VK_VALIDATION_FEATURE_DISABLE_ALL_EXT,
                                      VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT,
                                      VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT,
                                      VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT,
                                      VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT,
                                      VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT,
                                      VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT),
        VkValidationFeatureEnableEXT(VkValidationFeatureEnableEXT,
                                     VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT,
                                     VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT,
                                     VK_VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT,
                                     VK_VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT,
                                     VK_VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT))
       where
import Foreign.Storable                (Storable)
import GHC.Read                        (choose, expectP)
import Graphics.Vulkan.Marshal         (Int32)
import Text.ParserCombinators.ReadPrec (prec, step, (+++))
import Text.Read                       (Read (..), parens)
import Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkValidationCacheHeaderVersionEXT VkValidationCacheHeaderVersionEXT registry at www.khronos.org>
newtype VkValidationCacheHeaderVersionEXT = VkValidationCacheHeaderVersionEXT Int32
                                            deriving (Eq, Ord, Enum, Storable)

instance Show VkValidationCacheHeaderVersionEXT where
    showsPrec _ VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT
      = showString "VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT"
    showsPrec p (VkValidationCacheHeaderVersionEXT x)
      = showParen (p >= 11)
          (showString "VkValidationCacheHeaderVersionEXT " . showsPrec 11 x)

instance Read VkValidationCacheHeaderVersionEXT where
    readPrec
      = parens
          (choose
             [("VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT",
               pure VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT)]
             +++
             prec 10
               (expectP (Ident "VkValidationCacheHeaderVersionEXT") >>
                  (VkValidationCacheHeaderVersionEXT <$> step readPrec)))

pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT ::
        VkValidationCacheHeaderVersionEXT

pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT =
        VkValidationCacheHeaderVersionEXT 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkValidationCheckEXT VkValidationCheckEXT registry at www.khronos.org>
newtype VkValidationCheckEXT = VkValidationCheckEXT Int32
                               deriving (Eq, Ord, Enum, Storable)

instance Show VkValidationCheckEXT where
    showsPrec _ VK_VALIDATION_CHECK_ALL_EXT
      = showString "VK_VALIDATION_CHECK_ALL_EXT"
    showsPrec _ VK_VALIDATION_CHECK_SHADERS_EXT
      = showString "VK_VALIDATION_CHECK_SHADERS_EXT"
    showsPrec p (VkValidationCheckEXT x)
      = showParen (p >= 11)
          (showString "VkValidationCheckEXT " . showsPrec 11 x)

instance Read VkValidationCheckEXT where
    readPrec
      = parens
          (choose
             [("VK_VALIDATION_CHECK_ALL_EXT", pure VK_VALIDATION_CHECK_ALL_EXT),
              ("VK_VALIDATION_CHECK_SHADERS_EXT",
               pure VK_VALIDATION_CHECK_SHADERS_EXT)]
             +++
             prec 10
               (expectP (Ident "VkValidationCheckEXT") >>
                  (VkValidationCheckEXT <$> step readPrec)))

pattern VK_VALIDATION_CHECK_ALL_EXT :: VkValidationCheckEXT

pattern VK_VALIDATION_CHECK_ALL_EXT = VkValidationCheckEXT 0

pattern VK_VALIDATION_CHECK_SHADERS_EXT :: VkValidationCheckEXT

pattern VK_VALIDATION_CHECK_SHADERS_EXT = VkValidationCheckEXT 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkValidationFeatureDisableEXT VkValidationFeatureDisableEXT registry at www.khronos.org>
newtype VkValidationFeatureDisableEXT = VkValidationFeatureDisableEXT Int32
                                        deriving (Eq, Ord, Enum, Storable)

instance Show VkValidationFeatureDisableEXT where
    showsPrec _ VK_VALIDATION_FEATURE_DISABLE_ALL_EXT
      = showString "VK_VALIDATION_FEATURE_DISABLE_ALL_EXT"
    showsPrec _ VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT
      = showString "VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT"
    showsPrec _ VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT
      = showString "VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT"
    showsPrec _ VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT
      = showString "VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT"
    showsPrec _ VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT
      = showString "VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT"
    showsPrec _ VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT
      = showString "VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT"
    showsPrec _ VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT
      = showString "VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT"
    showsPrec p (VkValidationFeatureDisableEXT x)
      = showParen (p >= 11)
          (showString "VkValidationFeatureDisableEXT " . showsPrec 11 x)

instance Read VkValidationFeatureDisableEXT where
    readPrec
      = parens
          (choose
             [("VK_VALIDATION_FEATURE_DISABLE_ALL_EXT",
               pure VK_VALIDATION_FEATURE_DISABLE_ALL_EXT),
              ("VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT",
               pure VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT),
              ("VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT",
               pure VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT),
              ("VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT",
               pure VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT),
              ("VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT",
               pure VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT),
              ("VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT",
               pure VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT),
              ("VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT",
               pure VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT)]
             +++
             prec 10
               (expectP (Ident "VkValidationFeatureDisableEXT") >>
                  (VkValidationFeatureDisableEXT <$> step readPrec)))

pattern VK_VALIDATION_FEATURE_DISABLE_ALL_EXT ::
        VkValidationFeatureDisableEXT

pattern VK_VALIDATION_FEATURE_DISABLE_ALL_EXT =
        VkValidationFeatureDisableEXT 0

pattern VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT ::
        VkValidationFeatureDisableEXT

pattern VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT =
        VkValidationFeatureDisableEXT 1

pattern VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT ::
        VkValidationFeatureDisableEXT

pattern VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT =
        VkValidationFeatureDisableEXT 2

pattern VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT ::
        VkValidationFeatureDisableEXT

pattern VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT =
        VkValidationFeatureDisableEXT 3

pattern VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT ::
        VkValidationFeatureDisableEXT

pattern VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT =
        VkValidationFeatureDisableEXT 4

pattern VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT ::
        VkValidationFeatureDisableEXT

pattern VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT =
        VkValidationFeatureDisableEXT 5

pattern VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT ::
        VkValidationFeatureDisableEXT

pattern VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT =
        VkValidationFeatureDisableEXT 6

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkValidationFeatureEnableEXT VkValidationFeatureEnableEXT registry at www.khronos.org>
newtype VkValidationFeatureEnableEXT = VkValidationFeatureEnableEXT Int32
                                       deriving (Eq, Ord, Enum, Storable)

instance Show VkValidationFeatureEnableEXT where
    showsPrec _ VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT
      = showString "VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT"
    showsPrec _
      VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT
      = showString
          "VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT"
    showsPrec _ VK_VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT
      = showString "VK_VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT"
    showsPrec _ VK_VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT
      = showString "VK_VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT"
    showsPrec _
      VK_VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT
      = showString
          "VK_VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT"
    showsPrec p (VkValidationFeatureEnableEXT x)
      = showParen (p >= 11)
          (showString "VkValidationFeatureEnableEXT " . showsPrec 11 x)

instance Read VkValidationFeatureEnableEXT where
    readPrec
      = parens
          (choose
             [("VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT",
               pure VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT),
              ("VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT",
               pure
                 VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT),
              ("VK_VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT",
               pure VK_VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT),
              ("VK_VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT",
               pure VK_VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT),
              ("VK_VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT",
               pure VK_VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT)]
             +++
             prec 10
               (expectP (Ident "VkValidationFeatureEnableEXT") >>
                  (VkValidationFeatureEnableEXT <$> step readPrec)))

pattern VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT ::
        VkValidationFeatureEnableEXT

pattern VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT =
        VkValidationFeatureEnableEXT 0

pattern VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT
        :: VkValidationFeatureEnableEXT

pattern VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT
        = VkValidationFeatureEnableEXT 1

pattern VK_VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT ::
        VkValidationFeatureEnableEXT

pattern VK_VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT =
        VkValidationFeatureEnableEXT 2

pattern VK_VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT ::
        VkValidationFeatureEnableEXT

pattern VK_VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT =
        VkValidationFeatureEnableEXT 3

pattern VK_VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT
        :: VkValidationFeatureEnableEXT

pattern VK_VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT
        = VkValidationFeatureEnableEXT 4
