
-----------------------------------------------------------------------------------------------------
----SDL初期化
--initSDL :: CoreConf -> IO ()
--initSDL conf = let
--  surfaceFlags = if fullScreen conf then [SDL.Fullscreen] else []
--  in do
--    SDL.init [SDL.InitEverything]
--    SDL.setVideoMode 640 480 32 surfaceFlags
--    return ()

----終了
--quitSDL :: IO ()
--quitSDL = SDL.quit

