{-# LANGUAGE OverloadedStrings #-}

module Texexample where

import           Graphics.OpenGL

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.ByteString
import qualified Data.ByteString.Char8 as BSC
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.GL
import           Graphics.UI.GLFW as GLFW



vertexShader :: ByteString
vertexShader =
  BSC.unlines
    [ "#version 130"
    , "in  vec2 point;"
    , "in  vec2 coord;"

    , "out vec2 coord_;"

    , "void main () {"

    , "  coord_ = coord;"

    , "  gl_Position = vec4 (point, 0, 1);"
    , "}"
    ]

fragmentShader :: ByteString
fragmentShader =
  BSC.unlines
    [ "#version 130"

    , "in      vec2 coord_;"

    , "uniform sampler2D tex;"

    , "out     vec4 outColor;"

    , "void main () {"
    , "  outColor = texture (tex, coord_);"
    , "}"
    ]



mkWindow :: IO Window
mkWindow = do
  mayWin <- createWindow 512 512 "OpenGL Test" Nothing Nothing
  case mayWin of
    Just win -> return win
    Nothing  -> error "Could not create window"



main :: IO ()
main = do
  isInit <- GLFW.init
  unless isInit $ error "Not init"
  bracket mkWindow destroyWindow $ \win -> do
    bracket_ (makeContextCurrent $ Just win) (makeContextCurrent Nothing) $ do
      glClearColor 0.2 0 0.2 1
      glClear GL_COLOR_BUFFER_BIT

      prog <-
        withShader "Vertex" GL_VERTEX_SHADER vertexShader $ \vert ->
          withShader "Fragment" GL_FRAGMENT_SHADER fragmentShader $ \frag ->
            newProgram [vert, frag]

      vbo <- genBuffer
      vao <- genVertexArray

      glUseProgram prog
      glBindBuffer GL_ARRAY_BUFFER vbo
      glBindVertexArray vao

      pointLoc <- attribLocation prog "point"
      coordLoc <- attribLocation prog "coord"

      let float = sizeOf (undefined :: GLfloat)

      glEnableVertexAttribArray pointLoc
      glEnableVertexAttribArray coordLoc
      vertexAttribPointer pointLoc 2 GL_FLOAT False (4 * fromIntegral float) 0
      vertexAttribPointer coordLoc 2 GL_FLOAT False (4 * fromIntegral float) (2 * float)

      texLoc <- uniformLocation prog "tex"

      tex <- genTexture
      glBindTexture GL_TEXTURE_2D tex
      glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
      glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0
      glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL 0
      let rawTexture = [ 0, 0, 0, 1,   0, 0, 1, 1,   0, 1, 0, 1
                       , 0, 1, 1, 1,   1, 0, 0, 1,   1, 0, 1, 1
                       , 1, 1, 0, 1,   1, 1, 1, 1,   0, 0, 0, 0
                       ] :: [GLfloat]
      withArray rawTexture $ \ptr ->
        glTexImage2D GL_TEXTURE_2D 0 GL_RGBA 3 3 0 GL_RGBA GL_FLOAT (castPtr ptr)

      let points = [ 0  , 0  ,   0, 1
                   , 0.5, 0  ,   1, 1
                   , 0.5, 0.5,   1, 0
                   , 0  , 0.5,   0, 0 ] :: [GLfloat]
      withArray points $ \ptr ->
        glBufferData GL_ARRAY_BUFFER 64 (castPtr ptr) GL_STREAM_DRAW

      glDrawArrays GL_TRIANGLE_FAN 0 4

      swapBuffers win
      threadDelay 30000000
