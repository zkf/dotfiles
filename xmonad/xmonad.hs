-- XMonad config file
--
-- By Bjørnar Hansen <tilbjornar@gmail.com>
--
-- XMonad version 0.10
--
{-- imports --}
import XMonad hiding ( (|||) )
import Data.Char (isSpace)
import System.Exit
import System.IO (Handle)

import XMonad.Util.Run
import XMonad.Util.EZConfig

import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import qualified XMonad.Layout.ToggleLayouts as TL
import XMonad.Layout.NoBorders
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.WindowNavigation
import XMonad.Layout.PerWorkspace

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Input


import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks   -- avoidstruts
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Actions.UpdatePointer
import XMonad.Actions.CopyWindow
import XMonad.Actions.SinkAll
import XMonad.Actions.Volume
import XMonad.Actions.CycleWS

import qualified Data.Map         as M
import qualified XMonad.StackSet  as W

import qualified SolarizedColors  as Color
import Data.Maybe
import Data.Monoid (Endo)

{-- Theme / color settings --}

-- define some fonts
fontInconsolata :: String
fontInconsolata = "xft:Inconsolata:size=12"

fontDroidSansMono :: String
fontDroidSansMono = "xft:Droid Sans Mono Dotted:size = 14"

fontTerminus :: String
fontTerminus = "-*-terminus-*-*-*-*-18-*-*-*-*-*-*-*"

dark :: Bool
dark = False

background, foreground, border, secondary, emphasis :: String
(background, foreground, secondary, emphasis) =
    if dark
        then ( Color.background Color.solarizedDark
             , Color.primaryContent Color.solarizedDark
             , Color.secondaryContent Color.solarizedDark
             , Color.emphasizedContent Color.solarizedDark
             )
        else ( Color.background Color.solarizedLight
             , Color.primaryContent Color.solarizedLight
             , Color.secondaryContent Color.solarizedLight
             , Color.emphasizedContent Color.solarizedLight
             )
border     = Color.orange

myFocusedBorderColor, myNormalBorderColor :: String
myFocusedBorderColor = border
myNormalBorderColor  = secondary

dzenFont, dzenBgColor, dzenFgColor :: String
dzenFont    = drop 4 fontInconsolata  --dzen does not support `xft:` when xmonad launches it
dzenBgColor = background
dzenFgColor = foreground

-- Shell prompt theme
myShellPrompt :: XPConfig
myShellPrompt = defaultXPConfig
        {
                font              = fontTerminus,
                bgColor           = background,
                fgColor           = foreground,
                fgHLight          = background,
                bgHLight          = Color.yellow,
                borderColor       = border,
                promptBorderWidth = 2,
                position          = Bottom,
                height            = 24,
                defaultText       = ""
        }

-- Pretty printing for logHook
myPP :: Handle -> PP
myPP h = defaultPP
    {   ppCurrent         = \wsId -> dzenColor background (ppMultiColor wsId) . pad $ wsName wsId,
        ppHidden          = pad . (\wsId ->  dzenColor (ppMultiColor wsId) "" (wsName wsId)),
        ppHiddenNoWindows = dzenColor secondary "" . pad . wsName,
        ppLayout          = dzenColor foreground "" . pad,
        ppUrgent          = dzenColor background Color.red . dzenStrip . pad . wsName,
        ppSep             = dzenColor secondary "" "¦",
        ppWsSep           = "",
        ppTitle           = dzenColor Color.orange "" . pad . shorten 100,
        ppOutput          = hPutStrLn h
        -- ppExtras = logLoad : L.date ("^pa(1250)^bg() %a, %b %d ^fg(white)%H:%M^fg()") : []
    }
    where ppMultiColor wsId = fromMaybe emphasis (M.lookup wsId wsColorMap)
          wsName wsId = case M.lookup wsId wsNameMap of
                                Nothing    -> wsId
                                Just name  -> wsId ++ ":" ++ name

myXmobarPP :: Handle -> PP
myXmobarPP h = defaultPP
    {   ppCurrent         = \wsId -> xmobarColor background (ppMultiColor wsId) . pad $ wsName wsId,
        ppHidden          = pad . (\wsId ->  xmobarColor (ppMultiColor wsId) "" (wsName wsId)),
        ppHiddenNoWindows = xmobarColor secondary "" . pad . wsName,
        ppLayout          = xmobarColor foreground "" . pad,
        ppUrgent          = xmobarColor background Color.red . xmobarStrip . pad . wsName,
        ppSep             = xmobarColor secondary "" "¦",
        ppWsSep           = "",
        ppTitle           = xmobarColor Color.orange "" . pad . shorten 100,
        ppOutput          = hPutStrLn h
        -- ppExtras = logLoad : L.date ("^pa(1250)^bg() %a, %b %d ^fg(white)%H:%M^fg()") : []
    }
    where ppMultiColor wsId = fromMaybe emphasis (M.lookup wsId wsColorMap)
          wsName wsId = case M.lookup wsId wsNameMap of
                                Nothing    -> wsId
                                Just name  -> wsId ++ ":" ++ name
-- { ppOutput = hPutStrLn xmproc, ppTitle = xmobarColor "green" "" . shorten 50}


calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans =
    inputPrompt c (trim ans) ?+ \input ->
        liftIO(runProcessWithInput "qalc" [input] "") >>= calcPrompt c
    where
        trim  = f . f
            where f = reverse . dropWhile isSpace

{-- Workspaces --}

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..9::Int] ++ ["0","+"]

myWorkspaceNames :: [WorkspaceId]
myWorkspaceNames =
        [ "sys", "web", "com", "mus", "log" ]

wsNameMap :: M.Map WorkspaceId WorkspaceId
wsNameMap = M.fromList $ zip myWorkspaces myWorkspaceNames

wsNameToId :: M.Map WorkspaceId WorkspaceId
wsNameToId = M.fromList $ zip myWorkspaceNames myWorkspaces

-- Workspace colors map
wsColorMap :: M.Map WorkspaceId String
wsColorMap =
        M.fromList $ zip myWorkspaces
            [ Color.yellow
            , Color.orange
            , Color.magenta
            , Color.violet
            , Color.blue
            , Color.cyan
            , Color.green
            ]

{-- Key bindings --}
--TODO: add focusUrgent keybind
myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = mkKeymap conf $
    [
        -- Spawn applications
        ("M-S-<Return>" , spawn $ XMonad.terminal conf)        ,
        ("M-l"          , shellPrompt myShellPrompt)           ,
        ("M-<KP_Multiply>"          , calcPrompt  myShellPrompt "qalc")   ,
        ("M-<F12>"      , spawn "/home/anachron/bin/udsks.sh") ,

        -- Multimedia keys
        ("<XF86AudioLowerVolume>", spawn . ponymix $ Decrease volumeStep),
        ("<XF86AudioRaiseVolume>", spawn . ponymix $ Increase volumeStep),
        ("<XF86AudioMute>"       , spawn $ ponymix Toggle),
        ("<XF86Tools>"           , spawn "/home/anachron/bin/headphones-toggle"),
        ("<XF86TouchpadToggle>"  , spawn "/home/anachron/bin/trackpad-toggle")  ,
        ("<XF86HomePage>"        , spawn "/usr/bin/xbmc"),
        ("<XF86LaunchA>"         , spawn "/home/anachron/bin/dualhead"),

        -- Layout
        ("M-\\" , sendMessage NextLayout)      ,
        ("M-m"  , sendMessage $ TL.Toggle "Full") ,
        ("M-,"  , sendMessage $ IncMasterN 1),
        ("M-."  , sendMessage $ IncMasterN (-1)),

        -- Windows
        ("M-c" , kill1),
        ("M-f" , withFocused $ windows . W.sink),
        ("M-S-f", sinkAll),
        ("M-k"  , spawn "/home/anachron/bin/invertx"),

        -- Move focus
        ("M-n" , sendMessage $ Go L) ,
        ("M-e" , sendMessage $ Go D) ,
        ("M-i" , sendMessage $ Go R) ,
        ("M-u" , sendMessage $ Go U) ,

        -- Move focus (on floats)
        ("M-<Page_Down>", windows W.focusDown),
        ("M-<Page_Up>"  , windows W.focusUp  ),

        -- Move / swap windows
        ("M-S-n" , sendMessage $ Swap L) ,
        ("M-S-e" , sendMessage $ Swap D) ,
        ("M-S-i" , sendMessage $ Swap R) ,
        ("M-S-u" , sendMessage $ Swap U) ,

        -- Resize master area
        ("M-C-e" , sendMessage Shrink) ,
        ("M-C-u" , sendMessage Expand) ,

        -- Screenshot
        ("<Print>"  , spawn "scrot '%Y-%m-%d_%H:%M:%S_$wx$h.png' -e 'mv $f ~/screenshots/'"),
        ("C-<Print>", spawn "sleep 0.2; scrot -s '%Y-%m-%d_%H:%M:%S_$wx$h.png' -e 'mv $f ~/screenshots/'"),

        -- Lock screen
        ("M-z"   , spawn "xscreensaver-command -lock"),

        -- Quit or reload XMonad
        ("M-S-<Escape>" , io exitSuccess),
        ("M-<Escape>"   , broadcastMessage ReleaseResources >> restart "xmonad" True),

        -- next / previous screen
        ("M-w"   , nextScreen),
        ("M-S-w" , swapNextScreen)
    ]
    ++
    -- "M-[1..9,0,-]" -- Switch to workspace N
    -- "M-S-[1..9,0,-]" -- Move client to workspace N
    -- "M-C-[1..9,0,-]" -- Copy client to workspace N
    [("M-" ++ m ++ k, windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) myWorkspaces
        , (f, m) <- [ (W.view, ""), (W.shift, "S-"), (copy, "C-") ]
    ]
    ++
    -- "M-C-S-[1..9,0,-]" -- Move client to workspace N and follow
    [("M-C-S-" ++ k, windows (W.shift i) >> windows (W.view i))
        | (i, k) <- zip (XMonad.workspaces conf) myWorkspaces
    ]

  where ponymix cmd = "ponymix -c0 " ++ show cmd
        volumeStep = 5

data PonymixCmd = Decrease Int | Increase Int | Toggle
instance Show PonymixCmd where
    show (Decrease step) = "decrease " ++ show step
    show (Increase step) = "increase " ++ show step
    show Toggle          = "toggle"


{--  Hooks --}

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ className      =? "stalonetray"        --> doIgnore
    , className      =? "Skype"              --> doShift ( workspace "com" )
    , className      =? "Kopete"             --> doShift ( workspace "com" )
    , className      =? "Spotify"            --> doShift ( workspace "mus" )
    , className      =? "Tomahawk"           --> doShift ( workspace "mus" )
    , className      =? "Firefox"            --> doShift ( workspace "web" )
    , title          =? "Firefox Preferences"--> doFloat
    , title          =? "glxgears"           --> doFloat
    , title          =? "TSP"                --> doFloat
    , className      =? ""                   --> doFloat
    , resource       =? "sun-awt-X11-XFramePeer" --> doFloat
    , resource       =? "explorer.exe"       --> doFloat
    , className      =? "fontforge"          --> doFloat
    , className      =? "MPlayer"            --> doFloat
    , className      =? "Steam"              --> doFloat
    , title          =? "Steam"              --> doFloat
    , className      =? "Gnuplot"            --> doFloat
    , className      =? "feh"                --> doFloat
    , className      =? "XVroot"             --> doFloat
    , className      =? "Pavucontrol"        --> doFloat
    , className      =? "xbmc.bin"           --> doFullFloat
    , isFullscreen                           --> doFullFloat
    ] <+> manageDocks
    where workspace wsName = fromMaybe "1" $ M.lookup wsName wsNameToId


myLayoutHook =  avoidStruts
                -- . windowNavigation
                -- . configurableNavigation (navigateColor  myNormalBorderColor)
                . configurableNavigation (navigateBrightness 0.0 )
                . TL.toggleLayouts (noBorders Full)
                . smartBorders
                . layoutHintsToCenter
                . onWorkspace "3" imLayout
                $ myLayouts
                where
                    myLayouts   = tiled ||| Mirror tiled ||| Grid
                    imLayout = withIM 0.22 isSkype Grid
                    isSkype = Or (Title "anachron88 - Skype™")(Title "Skype™ 2.1 (Beta) for Linux")
                    tiled       = Tall nmaster delta ratio
                    nmaster     = 1
                    ratio       = 1/2
                    delta       = 3/100

myLogHook :: Handle -> X ()
myLogHook dzpipe =
        dynamicLogWithPP (myPP dzpipe)

myXmobar :: Handle -> X ()
myXmobar xmproc =
    dynamicLogWithPP (myXmobarPP xmproc)

statusBarCmd :: String
statusBarCmd = "dzen2" ++
               " -bg '" ++ dzenBgColor ++ "'" ++
               " -fg '" ++ dzenFgColor ++ "'" ++
               " -sa c" ++
               " -fn '" ++ dzenFont ++ "'" ++
               " -x 0 -y 0 -ta l -e 'onstart=lower'"

xmobarCmd :: Int -> String
xmobarCmd scr = "xmobar " ++
                "-B '" ++ background ++ "' " ++
                "-F '" ++ foreground ++ "' " ++
                "-f '" ++ fontTerminus ++ "' " ++
                "-x " ++ show scr

main :: IO ()
main = do
    --dzpipe <- spawnPipe statusBarCmd
    xmproc0 <- spawnPipe $ xmobarCmd 0
    xmproc1 <- spawnPipe $ xmobarCmd 1
    xmonad $ withUrgencyHookC myUrgencyHook myUrgencyConfig $ ewmh defaultConfig
        {
            handleEventHook    = fullscreenEventHook,
            borderWidth        = 0,
            normalBorderColor  = myNormalBorderColor,
            focusedBorderColor = myFocusedBorderColor,

            keys        = myKeys,
            layoutHook  = myLayoutHook,
            logHook     = fadeInactiveLogHook 0.8
                            >> myXmobar xmproc0
                            >> myXmobar xmproc1
                            >> updatePointer (Relative 0.99 0.99),
            -- logHook     = myLogHook dzpipe,
            manageHook  = myManageHook,
            modMask     = mod4Mask,
            startupHook = setWMName "LG3D",
            workspaces  = myWorkspaces
        }
  where myUrgencyHook = SpawnUrgencyHook "~/.xmonad/urgentHook"
        myUrgencyConfig = urgencyConfig {remindWhen = Every (minutes 1)}

