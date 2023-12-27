  -- Base
import           System.Directory
import           System.Exit                         (exitSuccess)
import           System.IO                           (hClose, hPutStr,
                                                      hPutStrLn)
import           XMonad
import qualified XMonad.StackSet                     as W

    -- Actions
import           XMonad.Actions.CopyWindow           (kill1)
import           XMonad.Actions.CycleWS              (Direction1D (..),
                                                      WSType (..), moveTo,
                                                      nextScreen, prevScreen,
                                                      shiftTo)
import           XMonad.Actions.GridSelect
import           XMonad.Actions.MouseResize
import           XMonad.Actions.Promote
import           XMonad.Actions.RotSlaves            (rotAllDown, rotSlavesDown)
import qualified XMonad.Actions.Search               as S
import           XMonad.Actions.WindowGo             (runOrRaise)
import           XMonad.Actions.WithAll              (killAll, sinkAll)

    -- Data
import           Data.Char                           (isSpace, toUpper)
import qualified Data.Map                            as M
import           Data.Maybe                          (fromJust, isJust)
import           Data.Monoid
import           Data.Tree

    -- Hooks
import           XMonad.Hooks.DynamicLog             (PP (..), dynamicLogWithPP,
                                                      shorten, wrap,
                                                      xmobarColor, xmobarPP)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks            (ToggleStruts (..),
                                                      avoidStruts, docks,
                                                      manageDocks)
import           XMonad.Hooks.ManageHelpers          (doCenterFloat,
                                                      doFullFloat, isFullscreen)
import           XMonad.Hooks.ServerMode
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Hooks.WindowSwallowing
import           XMonad.Hooks.WorkspaceHistory

    -- Layouts
import           XMonad.Layout.Accordion
import           XMonad.Layout.GridVariants          (Grid (Grid))
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Spiral
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns

    -- Layouts modifiers
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LimitWindows          (decreaseLimit,
                                                      increaseLimit,
                                                      limitWindows)
import qualified XMonad.Layout.MultiToggle           as MT (Toggle (..))
import           XMonad.Layout.MultiToggle           (EOT (EOT), mkToggle,
                                                      single, (??))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.ShowWName
import           XMonad.Layout.Simplest
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import qualified XMonad.Layout.ToggleLayouts         as T (ToggleLayout (Toggle),
                                                           toggleLayouts)
import           XMonad.Layout.WindowArranger        (WindowArrangerMsg (..),
                                                      windowArrange)
import           XMonad.Layout.WindowNavigation

   -- Utilities
import           XMonad.Util.Dmenu
import           XMonad.Util.EZConfig                (additionalKeysP,
                                                      mkNamedKeymap)
import           XMonad.Util.NamedActions
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run                     (runProcessWithInput,
                                                      safeSpawn, spawnPipe)
import           XMonad.Util.SpawnOnce

-- Colors

colorScheme = "gruvbox-dark"

colorBack = "#282828"
colorFore = "#ebdbb2"

color01 = "#282828"
color02 = "#cc241d"
color03 = "#98971a"
color04 = "#d79921"
color05 = "#458588"
color06 = "#b16286"
color07 = "#689d6a"
color08 = "#a89984"
color09 = "#928374"
color10 = "#fb4934"
color11 = "#b8bb26"
color12 = "#fabd2f"
color13 = "#83a598"
color14 = "#d3869b"
color15 = "#8ec07c"
color16 = "#ebdbb2"

colorTrayer :: String
colorTrayer = "--tint 0x282828"

-- Vars

myFont :: String
myFont = "xft:Hasklug Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask        -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty"    -- Sets default terminal

myBrowser :: String
myBrowser = "brave"  -- Sets qutebrowser as browser

myBorderWidth :: Dimension
myBorderWidth = 5           -- Sets border width for windows

myNormColor :: String       -- Border color of normal windows
myNormColor   = colorBack   -- This variable is imported from Colors.THEME

myFocusColor :: String      -- Border color of focused windows
myFocusColor  = color15     -- This variable is imported from Colors.THEME

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Scratchpads
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "calculator" spawnCalc findCalc manageCalc
                ]
  where
    spawnTerm  = myTerminal ++ " -t scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnCalc  = "qalculate-gtk"
    findCalc   = className =? "Qalculate-gtk"
    manageCalc = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.4
                 t = 0.75 -h
                 l = 0.70 -w

-- Layouts
--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Defining a bunch of layouts, many that I don't use.
-- limitWindows n sets maximum number of windows displayed for layout.
-- mySpacing n sets the gap size around the windows.
tall     = renamed [Replace "tall"]
           . limitWindows 5
           . windowNavigation
           . addTabs shrinkText myTabTheme
           -- . subLayout [] (smartBorders Simplest)
           $ ResizableTall 1 (3/100) (1/2) []
-- monocle  = renamed [Replace "monocle"]
--            . smartBorders
--            . windowNavigation
--            . addTabs shrinkText myTabTheme
--            . subLayout [] (smartBorders Simplest)
--            $ Full
floats   = renamed [Replace "floats"] simplestFloat
grid     = renamed [Replace "grid"]
           . limitWindows 9
           . windowNavigation
           . addTabs shrinkText myTabTheme
           -- . subLayout [] (smartBorders Simplest)
           . mkToggle (single MIRROR)
           $ Grid (16/10)
-- spirals  = renamed [Replace "spirals"]
--            . limitWindows 9
--            . smartBorders
--            . windowNavigation
--            . addTabs shrinkText myTabTheme
--            . subLayout [] (smartBorders Simplest)
--            $ spiral (6/7)
threeCol = renamed [Replace "threeCol"]
           . limitWindows 7
           . windowNavigation
           . addTabs shrinkText myTabTheme
           -- . subLayout [] (smartBorders Simplest)
           $ ThreeCol 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
           . limitWindows 7
           . windowNavigation
           . addTabs shrinkText myTabTheme
           -- . subLayout [] (smartBorders Simplest)
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
           . Mirror
           $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
           -- add spacing between window and tabs which looks bad.
           $ tabbed shrinkText myTabTheme
tallAccordion  = renamed [Replace "tallAccordion"] Accordion
wideAccordion  = renamed [Replace "wideAccordion"]
           . Mirror
           $ Accordion

-- setting colors for tabs layout and tabs sublayout.
myTabTheme = def { fontName            = myFont
                 , activeColor         = color15
                 , inactiveColor       = color08
                 , activeBorderColor   = color15
                 , inactiveBorderColor = colorBack
                 , activeTextColor     = colorBack
                 , inactiveTextColor   = color16
                 }

-- The layout hook
myLayoutHook = avoidStruts
               . mouseResize
               . windowArrange
               . T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout = withBorder myBorderWidth tall
                                           -- ||| noBorders monocle
                                           -- ||| floats
                                           ||| tabs
                                           -- ||| grid
                                           -- ||| spirals
                                           ||| threeCol
                                           ||| threeRow
                                           ||| tallAccordion
                                           ||| wideAccordion

-- Workspaces
-- myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
-- myWorkspaces = [" dev ", " www ", " sys ", " doc ", " vbox ", " chat ", " mus ", " vid ", " gfx "]
myWorkspaces = show <$> reverse [1..9]
myWorkspaceIndices = M.fromList $ zip myWorkspaces (reverse [1..9]) -- (,) == \x y -> (x,y)

myWSKeys = concat [[("M-" ++ n, addName ("Switch to workspace " ++ n) $ windows $ W.greedyView n), ("M-S-" ++ n, addName ("Send to workspace " ++ n) $ windows $ W.shift n)] | n <- myWorkspaces]

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

subtitle' ::  String -> ((KeyMask, KeySym), NamedAction)
subtitle' x = ((0,0), NamedAction $ map toUpper
                      $ sep ++ "\n-- " ++ x ++ " --\n" ++ sep)
  where
    sep = replicate (6 + length x) '-'

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe "yad --text-info --fontname=\"SauceCodePro Nerd Font Mono 12\" --fore=#46d9ff back=#282c36 --center --geometry=1200x800 --title \"XMonad keybindings\""
  --hPutStr h (unlines $ showKm x) -- showKM adds ">>" before subtitles
  hPutStr h (unlines $ showKmSimple x) -- showKmSimple doesn't add ">>" to subtitles
  hClose h
  return ()

myKeys :: XConfig l0 -> [((KeyMask, KeySym), NamedAction)]
myKeys c =
  --(subtitle "Custom Keys":) $ mkNamedKeymap c $
  let subKeys str ks = subtitle' str : mkNamedKeymap c ks in
  subKeys "Xmonad Essentials"
  [ ("M-C-r", addName "Recompile XMonad"       $ spawn "xmonad --recompile")
  , ("M-S-r", addName "Restart XMonad"         $ spawn "xmonad --restart")
  , ("M-S-q", addName "Quit XMonad"            $ sequence_ [io exitSuccess])
  , ("M-S-c", addName "Kill focused window"    kill1)
  , ("M-S-a", addName "Kill all windows on WS" killAll)
  , ("M-S-<Return>", addName "Run prompt"      $ sequence_ [spawn "rofi -show combi"])]

  ^++^ subKeys "Switch to workspace"
  myWSKeys

  ^++^ subKeys "Window navigation"
  [ ("M-j", addName "Move focus to next window"                $ windows W.focusDown)
  , ("M-k", addName "Move focus to prev window"                $ windows W.focusUp)
  , ("M-m", addName "Move focus to master window"              $ windows W.focusMaster)
  , ("M-S-j", addName "Swap focused window with next window"   $ windows W.swapDown)
  , ("M-S-k", addName "Swap focused window with prev window"   $ windows W.swapUp)
  , ("M-S-m", addName "Swap focused window with master window" $ windows W.swapMaster)
  , ("M-<Backspace>", addName "Move focused window to master" promote)
  , ("M-S-,", addName "Rotate all windows except master" rotSlavesDown)
  , ("M-S-.", addName "Rotate all windows current stack" rotAllDown)]

  ^++^ subKeys "Favorite programs"
  [ ("M-<Return>", addName "Launch terminal"   $ spawn myTerminal)
  , ("M-b", addName "Launch web browser"       $ spawn myBrowser)
  , ("M-u", addName "Start Emacsclient" $ spawn "emacsclient -c -a 'emacs'")
  , ("M-S-u", addName "Start VSCode" $ spawn "code")]

  ^++^ subKeys "Monitors"
  [ ("M-.", addName "Switch focus to next monitor" nextScreen)
  , ("M-,", addName "Switch focus to prev monitor" prevScreen)]

  -- Switch layouts
  ^++^ subKeys "Switch layouts"
  [ ("M-<Tab>", addName "Switch to next layout"   $ sendMessage NextLayout)
  , ("M-<Space>", addName "Toggle noborders/full" $ sendMessage (MT.Toggle NBFULL))]

  -- Window resizing
  ^++^ subKeys "Window resizing"
  [ ("M-h", addName "Shrink window"               $ sendMessage Shrink)
  , ("M-l", addName "Expand window"               $ sendMessage Expand)
  , ("M-M1-j", addName "Shrink window vertically" $ sendMessage MirrorShrink)
  , ("M-M1-k", addName "Expand window vertically" $ sendMessage MirrorExpand)]

  -- Floating windows
  ^++^ subKeys "Floating windows"
  [ ("M-f", addName "Toggle float layout"        $ sendMessage (T.Toggle "floats"))
  , ("M-t", addName "Sink a floating window"     $ withFocused $ windows . W.sink)
  , ("M-S-t", addName "Sink all floated windows" sinkAll)]

  -- Increase/decrease spacing (gaps)
  ^++^ subKeys "Window spacing (gaps)"
  [ ("C-M1-j", addName "Decrease window spacing" $ decWindowSpacing 4)
  , ("C-M1-k", addName "Increase window spacing" $ incWindowSpacing 4)
  , ("C-M1-h", addName "Decrease screen spacing" $ decScreenSpacing 4)
  , ("C-M1-l", addName "Increase screen spacing" $ incScreenSpacing 4)]

  -- Increase/decrease windows in the master pane or the stack
  ^++^ subKeys "Increase/decrease windows in master pane or the stack"
  [ ("M-S-<Up>", addName "Increase clients in master pane"   $ sendMessage (IncMasterN 1))
  , ("M-S-<Down>", addName "Decrease clients in master pane" $ sendMessage (IncMasterN (-1)))
  , ("M-=", addName "Increase max # of windows for layout" increaseLimit)
  , ("M--", addName "Decrease max # of windows for layout" decreaseLimit)]

  -- Multimedia Keys
  ^++^ subKeys "Multimedia keys"
  [ ("<XF86AudioMute>", addName "Toggle audio mute"   $ spawn "amixer set Master toggle")
  , ("<XF86AudioLowerVolume>", addName "Lower vol"    $ spawn "amixer set Master 5%- unmute")
  , ("<XF86AudioRaiseVolume>", addName "Raise vol"    $ spawn "amixer set Master 5%+ unmute")
  ]

-- Main
main :: IO ()
main = do
  -- Launching three instances of xmobar on their monitors.
  xmproc0 <- spawnPipe ("xmobar -x 0 $HOME/.config/xmobar/" ++ colorScheme ++ "-xmobarrc")
  xmproc1 <- spawnPipe ("xmobar -x 1 $HOME/.config/xmobar/" ++ colorScheme ++ "-xmobarrc")
  -- the xmonad, ya know...what the WM is named after!
  xmonad $ addDescrKeys' ((mod4Mask, xK_F1), showKeybindings) myKeys $ ewmh $ docks $ def
    { handleEventHook    = swallowEventHook (className =? "Alacritty"  <||> className =? "st-256color" <||> className =? "XTerm") (return True)
    , modMask            = myModMask
    , terminal           = myTerminal
    , layoutHook         = myLayoutHook
    , workspaces         = myWorkspaces
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormColor
    , focusedBorderColor = myFocusColor
    , logHook = dynamicLogWithPP $  filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP
        { ppOutput = \x -> hPutStrLn xmproc0 x   -- xmobar on monitor 1
                        >> hPutStrLn xmproc1 x   -- xmobar on monitor 2
        , ppCurrent = xmobarColor color05 ""
                    . wrap ("<box type=Top width=2 mt=1 color=" ++ color05 ++ ">") "</box>"
          -- Visible but not current workspace
        , ppVisible = xmobarColor color06 "" . clickable
          -- Hidden workspace
        , ppHidden = xmobarColor color05 "" . wrap " " ""
          -- Hidden workspaces (no windows)
        , ppHiddenNoWindows = xmobarColor color09 ""  . clickable
          -- Title of active window
        , ppTitle = xmobarColor color16 "" . shorten 60
          -- Separator character
        , ppSep =  "<fc=" ++ color09 ++ "> <fn=1>|</fn> </fc>"
          -- Urgent workspace
        , ppUrgent = xmobarColor color02 "" . wrap "!" "!"
          -- Adding # of windows on current workspace to the bar
        , ppExtras  = [windowCount]
          -- order of things in xmobar
        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
        }
    }
