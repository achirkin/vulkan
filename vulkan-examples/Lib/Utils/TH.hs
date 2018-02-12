{-# LANGUAGE TemplateHaskell #-}
module Lib.Utils.TH
    ( compileGLSL
    ) where

import           Control.Monad         (unless, when)
import           Foreign.Marshal.Array
import           GHC.Ptr               (Ptr (..))
import           Language.Haskell.TH
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process

-- | Get GLSL shader from file and compile it using @glslangValidator@
--   tool if it is available. Panic otherwise :)
--
--   Type of expression is @Integral a => (a, Ptr Word32)@
--   where first value is the code size, second value is a pointer to SPIR-V.
--
--   Code size it checked to be multiple of 4.
compileGLSL :: FilePath -> ExpQ
compileGLSL fpath = do
    contents <- runIO $ do

      tmpDir <- getTemporaryDirectory
      curDir <- getCurrentDirectory
      createDirectoryIfMissing True tmpDir
      let spirvCodeFile = tmpDir </> "haskell-spirv.tmp"
          shaderFile = curDir </> fpath
          shaderDir = takeDirectory shaderFile
          shaderFName = takeFileName shaderFile

      doesFileExist shaderFile >>= flip unless
        (error $ "compileGLSL: " ++ shaderFile ++ " does not exist.")

      doesFileExist spirvCodeFile >>= flip when
        (removeFile spirvCodeFile)

      (ec, stdo, stde) <-
        readCreateProcessWithExitCode
        (shell $ "glslangValidator -V -o " ++ spirvCodeFile ++ " " ++ shaderFName)
          { cwd = Just shaderDir
          } ""

      case ec of
        ExitSuccess -> pure ()
        ExitFailure i -> error $
          "glslangValidator exited with code " ++ show i ++ "\n"
          ++ "stdout:\n" ++ stdo ++ "\n"
          ++ "stderr:\n" ++ stde


      withBinaryFile spirvCodeFile ReadMode $ \h -> do
        fsize <- hFileSize h
        let contentSize = fromIntegral $ case rem fsize 4 of
              0 -> fsize
              k -> fsize + 4 - k
        allocaArray contentSize $ \ptr -> do
          hasRead <- hGetBuf h ptr contentSize
          (++ replicate (contentSize - hasRead) 0) <$> peekArray hasRead ptr

    return $ TupE [ LitE . IntegerL . fromIntegral $ length contents
                  , AppE (ConE 'Ptr) (LitE $ StringPrimL contents) ]
