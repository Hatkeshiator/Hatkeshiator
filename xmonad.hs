import           XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Grid
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Reflect

import XMonad.Actions.Submap

import System.Exit

-----------------------------------------------------------------------------------------------------------

-- preferred apps

myTerminal, myBrowser :: String
myTerminal  =  "alacritty"
myBrowser   =     "falkon"

-----------------------------------------------------------------------------------------------------------

-- keybinds

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = m}) = M.fromList $   -- Normal mode for core functionality
  [
    ( (m .|. s, xK_Return ),           windows W.swapMaster )
  , ( (m .|. s, xK_space  ),           layoutDefaultRestore )
  , ( (m .|. s, xK_e      ),      io (exitWith ExitSuccess) )
  , ( (m .|. s, xK_j      ),             windows W.swapDown )
  , ( (m .|. s, xK_k      ),               windows W.swapUp )
  , ( (m .|. s, xK_l      ),                     lockScreen )
  , ( (m .|. s, xK_m      ),          windows W.focusMaster )
  , ( (m .|. s, xK_r      ),                      recompile )
  , ( (m      , xK_Return ),               spawn myTerminal )
  , ( (m      , xK_Tab    ),            windows W.focusDown )
  , ( (m      , xK_space  ),         sendMessage NextLayout )
  , ( (m      , xK_comma  ),                addMasters $ -1 )
  , ( (m      , xK_period ),                   addMasters 1 )
  , ( (m      , xK_b      ),                spawn myBrowser )
  , ( (m      , xK_d      ),                          dMenu )
  , ( (m      , xK_h      ),             sendMessage Shrink )
  , ( (m      , xK_j      ),            windows W.focusDown )
  , ( (m      , xK_k      ),              windows W.focusUp )
  , ( (m      , xK_l      ),             sendMessage Expand )
  , ( (m      , xK_q      ),                           kill )
  , ( (m      , xK_r      ),                        restart )
  , ( (m      , xK_s      ),                  captureScreen )
  , ( (m      , xK_t      ),                  windowUnfloat )
  ]

  ++

  [
    ( (m, xK_w), submap . M.fromList $ -- Wander mode for workspace management
      [ ( (0, xK_s), mvView    "swamp" )
      , ( (s, xK_s), mvWindow  "swamp" )
      , ( (0, xK_b), mvView     "berg" )
      , ( (s, xK_b), mvWindow   "berg" )
      , ( (0, xK_t), mvView   "tundra" )
      , ( (s, xK_t), mvWindow "tundra" )
      , ( (0, xK_r), mvView     "ruin" )
      , ( (s, xK_r), mvWindow   "ruin" )
      ]
    )
  ]

  ++

  [
    ( (m, xK_m), submap . M.fromList $    -- Mirror mode for toggles
      [ ( (0, xK_h), layoutFlipHorizontal )
      , ( (0, xK_v), layoutFlipVertical   )
      , ( (0, xK_m), layoutTurnNinety     ) 
      , ( (0, xK_f), layoutMakeFullscreen ) 
      ]
    )
  ]
    where

      s                    :: KeyMask
      s                     = shiftMask

      captureScreen, dMenu, lockScreen, layoutDefaultRestore, layoutFlipHorizontal, layoutFlipVertical, layoutMakeFullscreen, layoutTurnNinety, recompile, restart, windowUnfloat :: X () -- idiomatic
      captureScreen              =               spawn "scrot"
      dMenu                      =               spawn "rofi -modi drun,run -show drun"
      lockScreen                 =               spawn "XSECURELOCK_PASSWORD_PROMPT=time_hex xsecurelock"
      layoutDefaultRestore       = setLayout   $ XMonad.layoutHook conf
      layoutFlipHorizontal       = sendMessage $ Toggle REFLECTX
      layoutFlipVertical         = sendMessage $ Toggle REFLECTY
      layoutTurnNinety           = sendMessage $ Toggle MIRROR
      layoutMakeFullscreen       = sendMessage $ Toggle FULL
      recompile                  =               spawn "xmonad --recompile"
      restart                    =               spawn "xmonad --restart"
      windowUnfloat              =               withFocused $ windows . W.sink 

      addMasters  :: Int -> X () 
      addMasters x = sendMessage $ IncMasterN x

      mvWindow, mvView     :: WorkspaceId -> X ()
      mvWindow              = windows . W.shift
      mvView                = windows . W.greedyView


myMouseBindings :: XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

  [
    ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                     >> windows W.shiftMaster))

  , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                     >> windows W.shiftMaster))
  ]

-----------------------------------------------------------------------------------------------------------

-- startup

myStartupHook :: X ()
myStartupHook = do
  spawn "xsetroot -cursor_name left_ptr"
  spawn "xrdb -merge ~/.Xresources"

-- layout

myLayoutHook :: MultiToggle (HCons StdTransformers (HCons StdTransformers (HCons REFLECTX (HCons REFLECTY EOT)))) (Choose SpiralWithDir (Choose Tall (Choose ThreeCol (Choose ThreeCol Grid)))) Window
myLayoutHook = mkToggle (FULL ?? MIRROR ?? REFLECTX ?? REFLECTY ?? EOT) $ spiral ihp ||| classic ihp ||| threecolmid ihp ||| threecol ihp ||| Grid

    where

        classic      :: Rational -> Tall Window
        classic       = Tall n d
        threecolmid, threecol :: Rational -> ThreeCol Window
        threecolmid            = ThreeColMid n d
        threecol               = ThreeCol    n d

        n            :: Int
        n             = 1
        d            :: Rational
        d             = 1/1000
        ihp          :: Rational                            -- $\Phi^{-1}$
        ihp           = (/) 2 $ toRational $ (+) 1 $ sqrt 5

-----------------------------------------------------------------------------------------------------------

main = xmonad $ def {

       terminal = myTerminal,
           keys = myKeys,
  mouseBindings = myMouseBindings,
     layoutHook = myLayoutHook,
    startupHook = myStartupHook,

         borderWidth = 1,
             modMask = mod4Mask,
          workspaces = ["swamp", "berg", "tundra", "ruin"],
   normalBorderColor = "#333333",
  focusedBorderColor = "#0deaf0"

}
