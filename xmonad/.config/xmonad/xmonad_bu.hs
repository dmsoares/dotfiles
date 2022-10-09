import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import qualified XMonad.StackSet              as W
import           XMonad.Util.EZConfig
import           XMonad.Util.WorkspaceCompare

myBorder :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
myBorder = def { focusedBorderColor = "#e95678" -- violet: "#b877db" -- magenta: "#6c6f93" -- yellow: "#fab795"
               , borderWidth = 5 }

myTerminal :: String
myTerminal = "alacritty"

myKeys :: [(String, X ())]
myKeys =
  [ ("M-p", spawn "rofi -show combi") -- Launch Rofi
  , ("M-b", spawn "brave-browser-stable") -- Launch Brave Browser
  , ("M-u", spawn "emacsclient -c -a 'emacs'") -- Launch Emacs client
  , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+") -- Raise volume by 5%
  , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-") -- Lower volume by 5%
  , ("<XF86AudioMute>", spawn "amixer set Master toggle") -- Toggle mute volume
  , ("M-<Escape>", spawn "xscreensaver-command --lock") -- Lock screen
  ]

myWorkspaces :: [String]
myWorkspaces = show <$> reverse [1 :: Integer ..9]

myWSKeys :: [(String, X ())]
myWSKeys = concat [[("M-" ++ n, windows $ W.greedyView n), ("M-S-" ++ n, windows $ W.shift n)] | n <- myWorkspaces]

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " • "
    , ppTitleSanitize = xmobarStrip
    , ppCurrent = wrap (blue "[") (blue "]")
    , ppHidden = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    , ppOrder = \[ws, l, t] -> [ws, l, t]
    , ppSort = getSortByTag
    }
  where
    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

myConfig :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
myConfig =
  def
    { modMask = mod4Mask -- Rebind Mod to the Super key
    , borderWidth = borderWidth myBorder
    , focusedBorderColor = focusedBorderColor myBorder
    , workspaces = myWorkspaces
    , terminal = myTerminal
    }
    `additionalKeysP` (myKeys ++ myWSKeys)

main :: IO ()
main =
  xmonad . ewmh =<< statusBar "xmobar ~/.xmonad/xmobar.hs" myXmobarPP toggleStrutsKey myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig {modMask = m} = (m .|. shiftMask, xK_b)
