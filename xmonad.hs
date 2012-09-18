-- XMonad config file
--
-- By Bjørnar Hansen <tilbjornar@gmail.com>
--
-- XMonad version 0.10
--
{-- imports --}
import XMonad hiding ( (|||) )
import System.Exit
import System.IO (Handle)

import XMonad.Util.Run
import XMonad.Util.EZConfig

import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.NoBorders
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.WindowNavigation

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks   -- avoidstruts
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog hiding (xmobar, xmobarPP, xmobarColor, sjanssenPP, byorgeyPP)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Actions.UpdatePointer
import XMonad.Actions.CopyWindow
import XMonad.Actions.SinkAll

import qualified Data.Map         as M
import qualified XMonad.StackSet  as W

import qualified SolarizedColors  as Color
import Data.Maybe
import Data.Monoid (Endo)

{-- Theme / color settings --}

-- define some fonts
fontInconsolata :: String
fontInconsolata = "xft:Inconsolata:size=19"

fontDroidSansMono :: String
fontDroidSansMono = "xft:Droid Sans Mono Dotted:size = 14"

background, foreground, border :: String
background = Color.base03
foreground = Color.base0
border     = Color.green

myFocusedBorderColor, myNormalBorderColor :: String
myFocusedBorderColor = border
myNormalBorderColor  = Color.base01

dzenFont, dzenBgColor, dzenFgColor :: String
dzenFont    = drop 4 fontInconsolata  --dzen does not support `xft:` when xmonad launches it
dzenBgColor = background
dzenFgColor = foreground

-- Shell prompt theme
myShellPrompt :: XPConfig
myShellPrompt = defaultXPConfig
        {
                font              = fontInconsolata,
                bgColor           = background,
                fgColor           = foreground,
                fgHLight          = Color.base03,
                bgHLight          = Color.yellow,
                borderColor       = border,
                promptBorderWidth = 2,
                position          = Top,
                height            = 24,
                defaultText       = ""
        }

-- Pretty printing for logHook
myPP :: Handle -> PP
myPP h = defaultPP
    {   ppCurrent         = \wsId -> dzenColor Color.base03 (ppMultiColor wsId) . pad $ wsName wsId,
        ppHidden          = pad . (\wsId ->  dzenColor (ppMultiColor wsId) "" (wsName wsId)),
        ppHiddenNoWindows = dzenColor Color.base01 "" . pad . wsName,
        ppLayout          = dzenColor foreground "" . pad,
        ppUrgent          = dzenColor background Color.red . dzenStrip . pad . wsName,
        ppSep             = dzenColor Color.base01 "" "¦",
        ppWsSep           = "",
        ppTitle           = dzenColor Color.violet "" . pad . shorten 100,
        ppOutput          = hPutStrLn h
        -- ppExtras = logLoad : L.date ("^pa(1250)^bg() %a, %b %d ^fg(white)%H:%M^fg()") : []
    }
    where ppMultiColor wsId = fromMaybe Color.base1 (M.lookup wsId wsColorMap)
          wsName wsId = case M.lookup wsId wsNameMap of
                                Nothing    -> wsId
                                Just name  -> wsId ++ ":" ++ name


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
        ("M-<F12>"      , spawn "/home/anachron/bin/udsks.sh") ,

        -- Multimedia keys
        ("<XF86AudioLowerVolume>", spawn "/home/anachron/bin/dvol.sh -d 2")  ,
        ("<XF86AudioRaiseVolume>", spawn "/home/anachron/bin/dvol.sh -i 2")  ,
        ("<XF86AudioMute>"       , spawn "/home/anachron/bin/dvol.sh -t")    ,
        ("<XF86Tools>"           , spawn "amixer set 'Front Panel' toggle")  ,

        -- Layout
        ("M-\\" , sendMessage NextLayout)      ,
        ("M-m"  , sendMessage $ Toggle "Full") ,
        ("M-,"  , sendMessage $ IncMasterN 1),
        ("M-."  , sendMessage $ IncMasterN (-1)),

        -- Windows
        ("M-c" , kill1),
        ("M-f" , withFocused $ windows . W.sink),
        ("M-S-f", sinkAll),
        ("M-k"  , spawn "/usr/bin/xcalib -invert -alter") ,

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
        ("<Print>"  , spawn "scrot"),
        ("C-<Print>", spawn "sleep 0.2; scrot -s"),

        -- Lock screen
        ("M-z"   , spawn "xscreensaver-command -lock"),

        -- Quit or reload XMonad
        ("M-S-q" , io exitSuccess),
        ("M-q"   , broadcastMessage ReleaseResources >> restart "xmonad" True)
    ]
    ++
    -- "M-[1..9,0,-]" -- Switch to workspace N
    -- "M-S-[1..9,0,-]" -- Move client to workspace N
    -- "M-C-[1..9,0,-]" -- Copy client to workspace N
    [("M-" ++ m ++ k, windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) myWorkspaces
        , (f, m) <- [ (W.greedyView, ""), (W.shift, "S-"), (copy, "C-") ]
    ]
    ++
    -- "M-C-S-[1..9,0,-]" -- Move client to workspace N and follow
    [("M-C-S-" ++ k, windows (W.shift i) >> windows (W.greedyView i))
        | (i, k) <- zip (XMonad.workspaces conf) myWorkspaces
    ]

{--  Hooks --}

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ className      =? "stalonetray"        --> doIgnore
    , className      =? "Skype"              --> doShift ( workspace "com" )
    , className      =? "Kopete"             --> doShift ( workspace "com" )
    , className      =? "Spotify"            --> doShift ( workspace "mus" )
    , className      =? "Tomahawk"           --> doShift ( workspace "mus" )
    , className      =? "Firefox"            --> doShift ( workspace "web" )
    , title          =? "glxgears"           --> doFloat
    , title          =? "TSP"                --> doFloat
    , className      =? ""                   --> doFloat
    , className      =? "fontforge"          --> doFloat
    , isFullscreen                           --> doFullFloat
    ] <+> manageDocks
    where workspace wsName = fromMaybe "1" $ M.lookup wsName wsNameToId

myLayoutHook =  avoidStruts
                -- . windowNavigation
                -- . configurableNavigation (navigateColor  myNormalBorderColor)
                . configurableNavigation (navigateBrightness 0.0 )
                . toggleLayouts (noBorders Full)
                . smartBorders
                . layoutHintsToCenter
                $ myLayouts
                where
                    myLayouts   = tiled ||| Mirror tiled ||| Grid
                                   ||| withIM 0.22 isSkype Grid
                    isSkype = Or (Title "anachron88 - Skype™")(Title "Skype™ 2.1 (Beta) for Linux")
                    tiled       = Tall nmaster delta ratio
                    nmaster     = 1
                    ratio       = 1/2
                    delta       = 3/100

myLogHook :: Handle -> X ()
myLogHook dzpipe =
        dynamicLogWithPP (myPP dzpipe) >> updatePointer (Relative 0.95 0.95)

statusBarCmd :: String
statusBarCmd = "dzen2" ++
               " -bg '" ++ dzenBgColor ++ "'" ++
               " -fg '" ++ dzenFgColor ++ "'" ++
               " -sa c" ++
               " -fn '" ++ dzenFont ++ "'" ++
               " -h 16 -x 0 -y 0 -ta l -e 'onstart=lower'"

main :: IO ()
main = do
    dzpipe <- spawnPipe statusBarCmd
    xmonad $ withUrgencyHook NoUrgencyHook defaultConfig    -- xmonad $ ewmh defaultconfig
        {
            handleEventHook    = fullscreenEventHook,
            borderWidth        = 2,
            normalBorderColor  = myNormalBorderColor,
            focusedBorderColor = myFocusedBorderColor,

            keys        = myKeys,
            layoutHook  = myLayoutHook,
            logHook     = myLogHook dzpipe,
            manageHook  = manageHook defaultConfig <+> myManageHook,
            modMask     = mod4Mask,
            startupHook = setWMName "LG3D",
            terminal    = "urxvt",
            workspaces  = myWorkspaces
        }

