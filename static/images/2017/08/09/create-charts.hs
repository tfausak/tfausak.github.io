-- stack --resolver lts-9.0 script
import Control.Lens ((&), (.~))

import qualified Control.Monad as Monad
import qualified Data.Colour as Color
import qualified Data.Colour.SRGB as Color
import qualified Data.Default.Class as Default
import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Diagrams


main :: IO ()
main = do

  createChart
    "deriving-performance.svg"
    "Impact of deriving type classes on Haskell compilation time"
    "type classes derived"
    "compilation time in milliseconds"
    [ ("none", 326.2027465)
    , ("Typeable", 331.8147953)
    , ("Eq", 597.027184)
    , ("Show", 982.1137298)
    , ("Ord", 1152.791624)
    , ("Data", 1379.676688)
    , ("Read", 1448.547814)
    , ("all", 3881.5056)
    ]

  createChart
    "ghc-performance.svg"
    "Compilation time across GHC versions"
    "GHC version"
    "compilation time in milliseconds"
    [ ("7.0.4", 4233.025664)
    , ("7.2.2", 4216.841279)
    , ("7.4.2", 4090.347325)
    , ("7.6.3", 4508.029407)
    , ("7.8.4", 3840.177728)
    , ("7.10.3", 4488.732122)
    , ("8.0.2", 4642.653821)
    , ("8.2.1", 3881.5056)
    ]


createChart :: FilePath -> String -> String -> String -> [(String, Double)] -> IO ()
createChart file title xTitle yTitle values =
  Monad.void
  . Diagrams.renderableToFile Default.def file
  . Chart.toRenderable
  $ Default.def
  & Chart.layout_title .~ title
  & Chart.layout_y_axis . Chart.laxis_title .~ yTitle
  & Chart.layout_x_axis . Chart.laxis_title .~ xTitle
  & Chart.layout_x_axis . Chart.laxis_generate .~ Chart.autoIndexAxis (map fst values)
  & Chart.layout_plots .~
    [ Chart.plotBars $ Default.def
      & Chart.plot_bars_item_styles .~
        [ (Default.def & Chart.fill_color .~ Color.opaque (Color.sRGB24 117 80 123), Nothing)
        ]
      & Chart.plot_bars_values .~ Chart.addIndexes (map (pure . snd) values)
    ]
