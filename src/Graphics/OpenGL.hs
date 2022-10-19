module Graphics.OpenGL where

import           Control.Exception
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Internal (ByteString (..))
import           Data.ByteString.Unsafe
import           Data.Foldable
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.GL



genBuffer :: IO GLuint
genBuffer =
  alloca $ \ptr -> do
    glGenBuffers 1 ptr
    peek ptr

genVertexArray :: IO GLuint
genVertexArray =
  alloca $ \ptr -> do
    glGenVertexArrays 1 ptr
    peek ptr

genTexture :: IO GLuint
genTexture =
  alloca $ \ptr -> do
    glGenTextures 1 ptr
    peek ptr



-- BSC.lines that preserves trailing newlines
newlines :: BSC.ByteString -> [BSC.ByteString]
newlines bs = case BSC.elemIndex '\n' bs of
                Just i  -> let (xs, ys) = BSC.splitAt (i + 1) bs
                           in xs : newlines ys
                Nothing -> if BSC.null bs
                             then []
                             else [BSC.snoc bs '\n']

shaderSource :: GLuint -> BSC.ByteString -> IO ()
shaderSource shader file =
  dive broken $ \(ptrs, ns) ->
    withArray ptrs $ \sptr ->
      withArray ns $ \lptr ->
        glShaderSource shader (fromIntegral $ Prelude.length broken) sptr lptr
  where
    broken = newlines file

    dive []      action = action ([], [])
    dive (bs:cs) action =
      unsafeUseAsCStringLen bs $ \(bsptr, n) -> do
        dive cs $ action . (\(ptrs, ns) -> (bsptr:ptrs, fromIntegral n:ns))

compileShader :: GLuint -> IO Bool
compileShader shader = do
  glCompileShader shader
  alloca $ \ptr -> do
    glGetShaderiv shader GL_COMPILE_STATUS ptr
    (==) GL_TRUE <$> peek ptr

shaderInfoLog :: GLuint -> IO BSC.ByteString
shaderInfoLog shader = do
  len <- alloca $ \ptr -> do
          glGetShaderiv shader GL_INFO_LOG_LENGTH ptr
          peek ptr

  fptr <- mallocForeignPtrBytes $ fromIntegral len
  withForeignPtr fptr $
    glGetShaderInfoLog shader len nullPtr

  return $ BS (castForeignPtr fptr) (fromIntegral len)



withShader
  :: String
  -> GLenum
  -> BSC.ByteString
  -> (GLuint -> IO a)
  -> IO a
withShader name typ raw f = do
  bracket (glCreateShader typ) glDeleteShader $ \shader -> do
    shaderSource shader raw
    compiled <- compileShader shader
    if compiled
      then f shader
      else do
        infoLog <- shaderInfoLog shader
        BSC.putStrLn infoLog
        error $ "Could not compile shader " <> name




programInfoLog :: GLuint -> IO BSC.ByteString
programInfoLog program = do
  len <- alloca $ \ptr -> do
           glGetProgramiv program GL_INFO_LOG_LENGTH ptr
           peek ptr

  fptr <- mallocForeignPtrBytes $ fromIntegral len
  withForeignPtr fptr $
    glGetProgramInfoLog program len nullPtr

  return $ BS (castForeignPtr fptr) (fromIntegral len)



linkProgram :: GLuint -> IO Bool
linkProgram program = do
  glLinkProgram program
  alloca $ \ptr -> do
    glGetProgramiv program GL_LINK_STATUS ptr
    (==) GL_TRUE <$> peek ptr



newProgram :: [GLuint] -> IO GLuint
newProgram shaders = do
  program <- glCreateProgram
  traverse_ (glAttachShader program) shaders
  linked <- linkProgram program
  if linked
    then return program
    else do
      infoLog <- programInfoLog program
      BSC.putStrLn infoLog
      error $ "Could not compile program"



attribLocation :: GLuint -> BSC.ByteString -> IO GLuint
attribLocation program name = do
  loc <- BSC.useAsCString name $
           glGetAttribLocation program
  if loc == -1
    then error $ "No attribute location named \"" <> BSC.unpack name <> "\" exists"
    else return $ fromIntegral loc

vertexAttribPointer
  :: GLuint -> GLint -> GLenum -> Bool -> GLsizei -> Int -> IO ()
vertexAttribPointer loc size attribTyp norm stride =
  let normalized | norm      = GL_TRUE
                 | otherwise = GL_FALSE
  in glVertexAttribPointer loc size attribTyp normalized stride . intPtrToPtr . IntPtr



uniformLocation :: GLuint -> BSC.ByteString -> IO GLuint
uniformLocation program name = do
  loc <- BSC.useAsCString name $
           glGetUniformLocation program
  if loc == -1
    then error $ "No uniform location named \"" <> BSC.unpack name <> "\" exists"
    else return $ fromIntegral loc
