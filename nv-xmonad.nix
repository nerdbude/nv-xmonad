{ ... }:

{
  services.xserver = {
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      
      config = ''
         ------------------------------------------------------------------------
        ---IMPORTS
        ------------------------------------------------------------------------
        -- Base
        import XMonad
        import XMonad.Config.Desktop (desktopConfig)
        import Data.Monoid
        import Data.Maybe (isJust)
        import System.IO
        import System.Exit (exitSuccess)
        import System.Process (readProcess)
        import qualified XMonad.StackSet as W

        -- Utilities
        import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)
        import XMonad.Util.NamedScratchpad
        import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe, runProcessWithInput)
        import XMonad.Util.SpawnOnce
        
        -- Hooks
        import XMonad.Hooks.DynamicLog (dynamicLogWithPP, ppCurrent, ppVisible, ppTitle, ppOrder, ppSep, ppLayout, ppExtras, ppOutput, wrap, pad, xmobarPP, xmobarColor, shorten, PP(..))
        import XMonad.Hooks.ManageDocks
        import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat)
        import XMonad.Hooks.SetWMName
        import XMonad.Hooks.EwmhDesktops
        
        -- Actions
        import XMonad.Actions.Minimize (minimizeWindow)
        import XMonad.Actions.Promote
        import XMonad.Actions.CopyWindow (kill1)
        import XMonad.Actions.WindowGo (runOrRaise, raiseMaybe)
        import XMonad.Actions.WithAll (sinkAll, killAll)
        import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen)
        import XMonad.Actions.GridSelect
        import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeEmptyWorkspace)
        import XMonad.Actions.MouseResize
        
        -- Layouts modifiers
        import XMonad.Layout.PerWorkspace (onWorkspace)
        import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
        import XMonad.Layout.Spacing (spacing)
        import XMonad.Layout.NoBorders
        import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
        import XMonad.Layout.WindowArranger 
        import XMonad.Layout.Reflect (reflectVert, reflectHoriz, REFLECTX(..), REFLECTY(..))
        import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
        import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
        import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
        
        -- Layouts
        import XMonad.Layout.NoFrillsDecoration
        import XMonad.Layout.SimplestFloat
        import XMonad.Layout.OneBig
        import XMonad.Layout.ThreeColumns
        import XMonad.Layout.ResizableTile
        import XMonad.Layout.ZoomRow (zoomRow, zoomIn, zoomOut, zoomReset, ZoomMessage(ZoomFullToggle))
        
        -- Prompts
        import XMonad.Prompt

        -- Modal imports
        import qualified Data.Map as M
        import qualified DBus as D 
        import qualified DBus.Client as D
        import Data.IORef (IORef, newIORef, readIORef, writeIORef)
        import XMonad.Actions.MessageFeedback
        import Control.Monad (when)
        import XMonad (liftIO)
        import Data.Time (getZonedTime, formatTime, defaultTimeLocale)
        import Data.Time.Clock (getCurrentTime)
        import XMonad.StackSet as W

------------------------------------------------------------------------
        ---CONFIG
        ------------------------------------------------------------------------
        data Mode = Normal | ResizeMode | MoveMode | LayoutMode | SessionMode | LaunchMode deriving (Eq, Show)

        myFont          = "xft:Berkeley Mono Nerd Font:style=Regular:pixelsize=11"
        myModMask       = mod4Mask
        myTerminal      = "alacritty"
        myTextEditor    = "nvim"
        myBorderWidth   = 2

        initialClient :: Mode
        initialClient = Normal

        main :: IO ()
        main = do
            modeRef <- newIORef Normal
            dbus <- D.connectSession
            xmproc <- spawnPipe "xmobar"
            dbusRef <- newIORef dbus
            xmonad $ docks def 
                  { terminal           = myTerminal
                  , modMask            = myModMask
                  , startupHook        = myStartupHook
                  , XMonad.workspaces  = myWorkspaces
                  , borderWidth        = myBorderWidth
                  , normalBorderColor  = "#44475A"
                  , focusedBorderColor = "#50FA7b"
                  , keys               = \c -> myKeys modeRef c
                  , layoutHook         = myLayoutHook
                  , manageHook         = manageDocks <+> manageHook def
                  , logHook            = dynamicLogWithPP (myXmobarPP xmproc modeRef)
                                          { ppOutput = hPutStrLn xmproc
                                          , ppTitle = xmobarColor "#BD93F9" "" . shorten 50
                                          , ppExtras = [modeLogger modeRef]
                                          }
                                        >> updateModeLog modeRef dbusRef
                  }

        ------------------------------------------------------------------------
        ---AUTOSTART
        ------------------------------------------------------------------------
        myStartupHook = do
                  spawnOnce "nitrogen --restore &"

        ------------------------------------------------------------------------
        --- SET KEYS 
        --- TO FIND KEY IDENTIFIERS: XEV
        ------------------------------------------------------------------------
        myKeys :: IORef Mode -> XConfig l -> M.Map (KeyMask, KeySym) (X ())
        myKeys modeRef conf = M.fromList $
            [ ((mod4Mask, xK_space), switchMode modeRef Normal)
            , ((mod4Mask, xK_r),     switchMode modeRef ResizeMode)
            , ((mod4Mask, xK_m),     switchMode modeRef MoveMode)
            , ((mod4Mask, xK_y),     switchMode modeRef LayoutMode)
            , ((mod4Mask, xK_s),     switchMode modeRef SessionMode)
            , ((mod4Mask, xK_a),     switchMode modeRef LaunchMode)

            , ((mod4Mask, xK_1),     windows $ W.view "0") 
            , ((mod4Mask, xK_2),     windows $ W.view "1") 
            , ((mod4Mask, xK_3),     windows $ W.view "2") 
            , ((mod4Mask, xK_4),     windows $ W.view "3") 
            , ((mod4Mask, xK_5),     windows $ W.view "4") 
            , ((mod4Mask, xK_6),     windows $ W.view "5") 
            , ((mod4Mask, xK_7),     windows $ W.view "6") 
            , ((mod4Mask, xK_8),     windows $ W.view "7") 
            , ((mod4Mask, xK_9),     windows $ W.view "8") 
            ]
            ++ dynamicKeys modeRef
        
        switchMode :: IORef Mode -> Mode -> X ()
        switchMode modeRef newMode = do
            liftIO $ writeIORef modeRef newMode
            refresh

        modeDispatch :: IORef Mode -> [(Mode, X ())] -> X ()
        modeDispatch modeRef actions = do
            current <- liftIO $ readIORef modeRef
            case lookup current actions of
                Just action -> action
                Nothing     -> return ()

        ------------------------------------------------------------------------
        --- DEFINE MODE KEY MAPS 
        ------------------------------------------------------------------------
        dynamicKeys :: IORef Mode -> [((KeyMask, KeySym), X ())]
        dynamicKeys modeRef = 
            [ ((mod4Mask, xK_h), modeDispatch modeRef
                [ (Normal, windows W.focusUp)
                , (ResizeMode, sendMessage Shrink)
                , (MoveMode, sendMessage (MoveLeft 10))
                -- , (LayoutMode, COMMAND)
                , (SessionMode, spawn "xmonad --restart")
                -- , (LaunchMode, COMMAND)
                ])
            
            , ((mod4Mask, xK_j), modeDispatch modeRef
                [ (Normal, windows W.focusMaster)
                , (ResizeMode, sendMessage MirrorShrink)
                , (MoveMode, sendMessage (MoveDown 10))
                -- , (LayoutMode,  COMMAND)
                -- , (SessionMode, COMMAND)
                -- , (LaunchMode,  COMMAND)
                ])
            
            , ((mod4Mask, xK_k), modeDispatch modeRef
                [ -- (Normal,     COMMAND)
                  (ResizeMode, sendMessage MirrorExpand)
                , (MoveMode, sendMessage (MoveUp 10))
                -- , (LayoutMode, COMMAND)
                , (SessionMode, io exitSuccess)
                -- , (LaunchMode, COMMAND)
                ])
            
            , ((mod4Mask, xK_l), modeDispatch modeRef
                [ (Normal, windows W.focusDown)
                , (ResizeMode, sendMessage Expand)
                , (MoveMode, sendMessage (MoveRight 10))
                , (LayoutMode, sendMessage ZoomFullToggle)
                , (SessionMode, spawn "loginctl lock-session")
                -- , (LaunchMode, COMMAND)
                ])
            
            , ((mod4Mask, xK_Return), modeDispatch modeRef
                [ (Normal, spawn myTerminal)
                -- , (ResizeMode,  COMMAND)
                -- , (MoveMode,    COMMAND)
                -- , (LayoutMode,  COMMAND)
                -- , (SessionMode, COMMAND)
                -- , (LaunchMode,  COMMAND)
                ])

            , ((mod4Mask, xK_d), modeDispatch modeRef
                [ (Normal, kill1)
                -- , (ResizeMode,  COMMAND)
                -- , (MoveMode,    COMMAND)
                , (LayoutMode, setLayout (Layout (Tall 1 (3/100) (1/2))))
                -- , (SessionMode, COMMAND)
                , (LaunchMode, spawn "rofi -show run")
                ])

            , ((mod4Mask, xK_f), modeDispatch modeRef
                [ (Normal, spawn "librewolf")
                -- , (ResizeMode,  COMMAND)
                -- , (MoveMode,    COMMAND)
                , (LayoutMode, sendMessage (T.Toggle "float"))
                -- , (SessionMode, COMMAND)
                , (LaunchMode, spawn "librewolf")
                ])

            , ((mod4Mask, xK_n), modeDispatch modeRef
                [-- (Normal,       COMMAND)
                --, (ResizeMode,   COMMAND)
                --, (MoveMode,     COMMAND)
                  (LayoutMode, sendMessage (MoveRight 10))
                -- , (SessionMode, COMMAND)
                -- , (LaunchMode,  COMMAND)
                ])

            , ((mod4Mask, xK_o), modeDispatch modeRef
                [ -- (Normal, COMMAND)
                -- , (ResizeMode,  COMMAND)
                -- , (MoveMode,    COMMAND)
                  (LayoutMode, sendMessage $ Toggle MIRROR)
                -- , (SessionMode, COMMAND)
                , (LaunchMode, spawn "obsidian")
                ])

            , ((mod4Mask, xK_b), modeDispatch modeRef
                [ -- (Normal, COMMAND)
                -- , (ResizeMode,  COMMAND)
                -- , (MoveMode,    COMMAND)
                  (LayoutMode, sendMessage $ Toggle NOBORDERS)
                -- , (SessionMode, COMMAND)
                -- , (LaunchMode,  COMMAND)
                ])

            , ((mod4Mask, xK_u), modeDispatch modeRef
                [ -- (Normal, COMMAND)
                -- , (ResizeMode,  COMMAND)
                -- , (MoveMode,    COMMAND)
                -- , (LayoutMode,  COMMAND)
                -- , (SessionMode, COMMAND)
                -- , (LaunchMode,  COMMAND)
                ])

            , ((mod4Mask, xK_t), modeDispatch modeRef
                [ -- (Normal, COMMAND)
                -- , (ResizeMode,  COMMAND)
                -- , (MoveMode,    COMMAND)
                -- , (LayoutMode,  COMMAND)
                -- , (SessionMode, COMMAND)
                 (LaunchMode, spawn myTerminal)
                ])

            , ((mod4Mask, xK_v), modeDispatch modeRef
                [ -- (Normal, COMMAND)
                -- , (ResizeMode,  COMMAND)
                -- , (MoveMode,    COMMAND)
                -- , (LayoutMode,  COMMAND)
                -- , (SessionMode, COMMAND)
                 (LaunchMode, spawn "nvim")
                ])
            ]
        
        withMode :: Mode -> IORef Mode -> X () -> X ()
        withMode expected modeRef action = do
            mode <- liftIO $ readIORef modeRef
            when (mode == expected) $ do
               liftIO $ writeIORef modeRef expected
               action
    
        ------------------------------------------------------------------------
        ---WORKSPACES
        ------------------------------------------------------------------------
        xmobarEscape :: String -> String
        xmobarEscape = concatMap doubleLts
          where
                doubleLts '<' = "<<"
                doubleLts x   = [x]
        
        myWorkspaces :: [String]
        myWorkspaces = ["0", "1", "2", "3", "4", "5", "6", "7", "8"]

        myManageHook :: Query (Data.Monoid.Endo WindowSet)
        myManageHook = composeAll
            [
                className =? "Firefox"     --> doShift "<action=xdotool key super+1>www</action>"
              , title =? "Vivaldi"         --> doShift "<action=xdotool key super+1>www</action>"
              , title =? "irssi"           --> doShift "<action=xdotool key super+9>irc</action>"
              , (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
            ] 

        ------------------------------------------------------------------------
        --- XMOBAR
        ------------------------------------------------------------------------
        myXmobarPP :: Handle -> IORef Mode -> PP
        myXmobarPP xmproc modeRef = def {
            ppCurrent            = xmobarColor "#50FA7b" "" . wrap " [" "] ",
            ppVisible            = const "",
            ppHidden             = const "",
            ppHiddenNoWindows    = const "",
            ppTitle              = xmobarColor "#f8f8f2" "" . shorten 60,
            ppSep                = " ",
            ppLayout             = xmobarColor "#ff79c6" "",
            ppOrder              = \(ws:l:t:extras) -> extras ++ [ws, t],
            ppExtras = [modeLogger modeRef, windowCountLogger, timeAndKernelLogger]

            }

        timeAndKernelLogger :: X (Maybe String)
        timeAndKernelLogger = liftIO $ do
           time <- getCurrentTime
           let timeString = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time
           kernel <- readProcess "uname" ["-r"] ""
           return $ Just $ xmobarColor "#6272A4" "" (kernel ++ " | " ++ timeString)
        
        dynamicLogTimeAndKernel :: IO String
        dynamicLogTimeAndKernel = do
          time <- dynamicLogTime
          kernel <- kernelInfo
          return (kernel ++ " | " ++ time)
              
        dynamicLogTime :: IO String
        dynamicLogTime = do
          time <- getCurrentTime
          let timeString = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time
          return timeString

        kernelInfo :: IO String
        kernelInfo = do
          kernel <- readProcess "uname" ["-r"] ""
          return kernel
          
        modeLogger :: IORef Mode -> X (Maybe String)
        modeLogger modeRef = do
                mode <- liftIO $ readIORef modeRef
                let icon txt = "<fn=1>" ++ txt ++ "</fn>"
                let formatMode bg fg name = wrapColor bg fg (icon " \xE61F " ++ name) 
                let fmt bg fg txt = "<fc=" ++ fg ++ "," ++ bg ++ "> " ++ txt ++ " </fc>"
                return $ Just $ case mode of
                       Normal       -> formatMode "#50FA7B" "#282A36" ( "NORMAL " )
                       ResizeMode   -> formatMode "#FFB86C" "#282A36" ( "RESIZE " )
                       MoveMode     -> formatMode "#8B39FD" "#282A36" ( "MOVE " )
                       LayoutMode   -> formatMode "#FF79C6" "#282A36" ( "LAYOUT " )
                       SessionMode  -> formatMode "#BD93F9" "#282A36" ( "SESSION " )
                       LaunchMode   -> formatMode "#F1FA8C" "#282A36" ( "LAUNCH " )

        windowCountLogger :: X (Maybe String)
        windowCountLogger = withWindowSet $ \ws -> do
          let count = length (W.index ws)
          return $ Just $ xmobarColor "#ff79c6" "" (show count)
          
        updateModeLog :: IORef Mode -> IORef D.Client -> X ()
        updateModeLog modeRef dbusRef = do
            mode <- liftIO $ readIORef modeRef
            dbus <- liftIO $ readIORef dbusRef
            let modeText = case mode of
                    Normal             -> " NORMAL "
                    ResizeMode     -> " RESIZE "
                    MoveMode       -> " MOVE "
                    LayoutMode     -> " LAYOUT "
                    SessionMode   -> " SESSION "
                    LaunchMode    -> " LAUNCH "

            let modeFile = "/tmp/xmonad_mode.txt"
            liftIO $ do
                handle <- openFile modeFile WriteMode
                hPutStrLn handle modeText
                hClose handle

            io $ D.emit dbus (D.signal objectPath interfaceName memberName)
               { D.signalBody = [D.toVariant (modeText)] }
            where
              objectPath = D.objectPath_ "/org/xmonad/Log"
              interfaceName = D.interfaceName_ "org.xmonad.Log"
              memberName = D.memberName_ "Update"
            
        wrapColor :: String -> String -> String -> String
        wrapColor bg fg txt = "<box type=Full width=2 mb=0 color=" ++ bg ++ "><fc=" ++ fg ++ "," ++ bg ++ ">" ++ txt ++ "</fc></box>"
        
        ------------------------------------------------------------------------
        --- LAYOUTS
        ------------------------------------------------------------------------
        
        myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
                       mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ myDefaultLayout
        myDefaultLayout =
              tall 
          ||| threeCol 
          ||| threeRow 
          ||| oneBig 
          ||| noBorders monocle 
          ||| space 
          ||| floats


        tall       = renamed [Replace "T"]     $ limitWindows 12 $ spacing 6 $ ResizableTall 1 (3/100) (1/2) [] 
        threeCol   = renamed [Replace "3C"]    $ limitWindows 3  $ ThreeCol 1 (3/100) (1/2)
        threeRow   = renamed [Replace "3R"]    $ limitWindows 3  $ Mirror $ mkToggle (single MIRROR) zoomRow
        oneBig     = renamed [Replace "1B"]    $ limitWindows 6  $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (5/9) (8/12)
        monocle    = renamed [Replace "M"]     $ limitWindows 20 $ Full
        space      = renamed [Replace "S"]     $ limitWindows 4  $ spacing 12 $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (2/3) (2/3)
        floats     = renamed [Replace "F"]     $ limitWindows 20 $ simplestFloat

        myTitleTheme :: Theme
        myTitleTheme = def {
                fontName                = "xft:Berkeley Mono Nerd Font:style=Regular:pixelsize=11"
                , inactiveBorderColor   = "#44475A"
                , inactiveColor         = "#44475A"
                , inactiveTextColor     = "#BD93F9"
                , activeBorderColor     = "#44475A"
                , activeColor           = "#44475A"
                , activeTextColor       = "#50FA7B"
                , urgentBorderColor     = "#FF4242"
                , urgentTextColor       = "#262626"
                , urgentColor           = "#FF4242"
                , decoHeight            = 12
        }
      '';
      enableConfiguredRecompile = true;
    };
  };


}
