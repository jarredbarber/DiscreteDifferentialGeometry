{-# LANGUAGE QuasiQuotes #-}
-- Graphics are heavily adapted (read: copy/pasted) from the Haskell roguelike tutorial
module Main where

import Control.Monad (when)
import Control.Exception (bracket)

import Foreign
import Foreign.C.String (withCAStringLen)

import Text.RawString.QQ

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Types

import qualified SComplex as SC

winWidth = 800
winHeight = 600
winTitle = "Discrete Differential Geometry Demo"

-- Make a cube
verts = let vl=[-0.5, 0.5]::[GLfloat]
        in  [[i,j,k] | i <- vl, j <- vl, k <- vl] >>= id
           
complex = SC.fromList [[0,1,2]]
faces = (SC.faces complex 3) -- >>= SC.boundary

callback :: GLFW.KeyCallback
callback window key scanCode keyState modKeys = do
  print key
  when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
    (GLFW.setWindowShouldClose window True)

bracketGLFW :: IO () -> IO ()
bracketGLFW act = bracket GLFW.init (const GLFW.terminate) $ \initWorked ->
    when initWorked act

vertexShaderSource :: String
vertexShaderSource = [r|#version 330 core
                       layout (location = 0) in vec3 position;
                       void main()
                       {
                       gl_Position = vec4(position.x, position.y, position.z, 1.0);
                       }
|]
fragmentShaderSource :: String
fragmentShaderSource = [r|#version 330 core
    out vec4 color;
    void main()
    {
        color = vec4(1.0f, 0.5f, 0.2f, 1.0f);
    }
    |]
-- | Given a shader type and a shader source, it gives you (Right id) of the
-- successfully compiled shader, or (Left err) with the error message. In the
-- error case, the shader id is deleted before the function returns to avoid
-- accidentally leaking shader objects.
loadShader :: GLenum -> String -> IO (Either String GLuint)
loadShader shaderType source = do
    -- new shader object
    shaderID <- glCreateShader shaderType
    -- assign the source to the shader object
    withCAStringLen source $ \(strP, strLen) ->
        withArray [strP] $ \linesPtrsPtr ->
            withArray [fromIntegral strLen] $ \lengthsPtr ->
                glShaderSource shaderID 1 linesPtrsPtr lengthsPtr
    -- compile and check success
    glCompileShader shaderID
    success <- alloca $ \successP -> do
        glGetShaderiv shaderID GL_COMPILE_STATUS successP
        peek successP
    if success == GL_TRUE
        -- success: we're done
        then return (Right shaderID)
        -- failure: we get the log, delete the shader, and return the log.
        else do
            -- how many bytes the info log should be (including the '\0')
            logLen <- alloca $ \logLenP -> do
                glGetShaderiv shaderID GL_INFO_LOG_LENGTH logLenP
                peek logLenP
            -- space for the info log
            logBytes <- allocaBytes (fromIntegral logLen) $ \logP -> do
                -- space for the log reading result
                alloca $ \resultP -> do
                    -- Try to obtain the log bytes
                    glGetShaderInfoLog shaderID logLen resultP logP
                    -- this is how many bytes we actually got
                    result <- fromIntegral <$> peek resultP
                    peekArray result logP
            -- delete the shader object and return the log
            glDeleteShader shaderID
            let prefix = case shaderType of
                    GL_VERTEX_SHADER -> "Vertex"
                    GL_GEOMETRY_SHADER -> "Geometry"
                    GL_FRAGMENT_SHADER -> "Fragment"
                    _ -> "Unknown Type"
            return $ Left $
                prefix ++ " Shader Error:" ++
                    (map (toEnum.fromEnum) logBytes)

-- | Given a vertex shader object and a fragment shader object, this will link
-- them into a new program, giving you (Right id). If there's a linking error
-- the error log is retrieved, the program deleted, and (Left err) is returned.
linkProgram :: GLuint -> GLuint -> IO (Either String GLuint)
linkProgram vertexID fragmentID = do
    programID <- glCreateProgram
    glAttachShader programID vertexID
    glAttachShader programID fragmentID
    glLinkProgram programID
    success <- alloca $ \successP -> do
        glGetProgramiv programID GL_LINK_STATUS successP
        peek successP
    if success == GL_TRUE
        -- success: we're done
        then return (Right programID)
        -- failure: we get the log, delete the shader, and return the log.
        else do
            -- how many bytes the info log should be (including the '\0')
            logLen <- alloca $ \logLenP -> do
                glGetProgramiv programID GL_INFO_LOG_LENGTH logLenP
                peek logLenP
            -- space for the info log
            logBytes <- allocaBytes (fromIntegral logLen) $ \logP -> do
                -- space for the log reading result
                alloca $ \resultP -> do
                    -- Try to obtain the log bytes
                    glGetProgramInfoLog programID logLen resultP logP
                    -- this is how many bytes we actually got
                    result <- fromIntegral <$> peek resultP
                    peekArray result logP
            -- delete the program object and return the log
            glDeleteProgram programID
            return $ Left $ "Program Link Error: " ++
                (map (toEnum.fromEnum) logBytes)

-- | Given the source for the vertex shader and the fragment shader, compiles
-- both and links them into a single program. If all of that is successful, the
-- intermediate shaders are deleted before the final value is returned.
programFromSources :: String -> String -> IO (Either String GLuint)
programFromSources vertexSource fragmentSource = do
    eitherVertShader <- loadShader GL_VERTEX_SHADER vertexSource
    case eitherVertShader of
        Left e -> return $ Left e
        Right vertShader -> do
            eitherFragShader <- loadShader GL_FRAGMENT_SHADER fragmentSource
            case eitherFragShader of
                Left e -> do
                    glDeleteShader vertShader
                    return $ Left e
                Right fragShader -> do
                    eitherProgram <- linkProgram vertShader fragShader
                    glDeleteShader vertShader
                    glDeleteShader fragShader
                    return $ eitherProgram

main :: IO ()
main = bracketGLFW $ do
    GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
    GLFW.windowHint (GLFW.WindowHint'Resizable False)
    maybeWindow <- GLFW.createWindow winWidth winHeight winTitle Nothing Nothing
    case maybeWindow of
        Nothing -> putStrLn "Failed to create a GLFW window!"
        Just window -> do
            -- enable keys
            GLFW.setKeyCallback window (Just callback)

            -- calibrate the viewport
            GLFW.makeContextCurrent (Just window)
            (x,y) <- GLFW.getFramebufferSize window
            glViewport 0 0 (fromIntegral x) (fromIntegral y)

            -- ready our program
            eErrP <- programFromSources vertexShaderSource fragmentShaderSource
            shaderProgram <- case eErrP of
                Left e -> putStrLn e >> return 0
                Right p -> return p

            -- activate the program
            glUseProgram shaderProgram

            let verticies = let vl=[0.5, -0.5]::[GLfloat]
                  in  [[i,j,0.0] | i <- vl, j <- vl] >>= id
            -- setup our verticies
            -- let verticies = [
            --         0.5,  0.5, 0.0,  -- Top Right
            --         0.5, -0.5, 0.0,  -- Bottom Right
            --         -0.5, -0.5, 0.0, -- Bottom Left
            --         -0.5,  0.5, 0.0  -- Top Left
            --         ] :: [GLfloat]
            let verticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length verticies)
            verticesP <- newArray verticies

            -- setup the indexes
            let indices = [  -- Note that we start from 0!
                    0, 1, 3, -- First Triangle
                    1, 2, 3  -- Second Triangle
                    ] :: [GLuint]
            let indicesSize = fromIntegral $ sizeOf (0 :: GLuint) * (length indices)
            indicesP <- newArray indices

            -- setup a vertex array object
            vaoP <- malloc
            glGenVertexArrays 1 vaoP
            vao <- peek vaoP
            glBindVertexArray vao

            -- setup a vertex buffer object and send it data
            vboP <- malloc
            glGenBuffers 1 vboP
            vbo <- peek vboP
            glBindBuffer GL_ARRAY_BUFFER vbo
            glBufferData GL_ARRAY_BUFFER verticesSize (castPtr verticesP) GL_STATIC_DRAW

            -- setup an element buffer object and send it data
            eboP <- malloc
            glGenBuffers 1 eboP
            ebo <- peek eboP
            glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
            glBufferData GL_ELEMENT_ARRAY_BUFFER indicesSize (castPtr indicesP) GL_STATIC_DRAW

            -- assign the attribute pointer information
            let threeFloats = fromIntegral $ sizeOf (0.0::GLfloat) * 3
            glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE threeFloats nullPtr
            glEnableVertexAttribArray 0

            -- unbind our vertex array object to prevent accidental changes in
            -- between our draw calls.
            glBindVertexArray 0

            -- Uncomment this line for "wireframe mode"
            -- glPolygonMode GL_FRONT_AND_BACK GL_LINE

            -- enter our main loop
            let loop = do
                    shouldContinue <- not <$> GLFW.windowShouldClose window
                    when shouldContinue $ do
                        -- event poll
                        GLFW.pollEvents
                        -- clear the screen
                        glClearColor 0.2 0.3 0.3 1.0
                        glClear GL_COLOR_BUFFER_BIT
                        -- draw the triangle
                        glBindVertexArray vao
                        glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr
                        glBindVertexArray 0
                        -- swap buffers and go again
                        GLFW.swapBuffers window
                        loop
            loop
