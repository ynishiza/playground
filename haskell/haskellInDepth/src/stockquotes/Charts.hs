{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Charts
  ( plotChartToFile,
    PlotOptions (..),
    PlotFormat (..),
    defaultPlotOptions,
  )
where

import Data.Foldable as F
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Rendering.Chart.Backend.Cairo as Cairo
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Diagrams
import Graphics.Rendering.Chart.Easy hiding (bars, close, label)
import QuoteData

data PlotFormat = SVG | PNG

data PlotOptions = PlotOptions
  { width :: !Int,
    height :: !Int,
    title :: !Text,
    format :: !PlotFormat
  }

defaultPlotOptions :: PlotOptions
defaultPlotOptions =
  PlotOptions
    { width = 1280,
      height = 720,
      format = PNG,
      title = ""
    }

plotChartToFile :: Foldable t => t QuoteData -> FilePath -> PlotOptions -> IO ()
plotChartToFile quotes path (PlotOptions width height title format) = do
  _ <- case format of
    PNG -> Cairo.renderableToFile cairoOpts path $ toRenderable chart
    SVG -> Diagrams.renderableToFile diagramOpts path $ toRenderable chart
  return ()
  where
    cairoOpts = Cairo.FileOptions (width, height) Cairo.PNG
    diagramOpts = Diagrams.FileOptions (realToFrac width, realToFrac height) Diagrams.SVG Diagrams.loadSansSerifFonts
    chart = getChart quotes title

getChart :: Foldable t => t QuoteData -> Text -> Renderable ()
getChart quotes title = toRenderable p
  where
    p =
      def
        { _slayouts_layouts =
            [ StackedLayout candlesLayout,
              StackedLayout volumesLayout
            ]
        }

    candlesLayout =
      def
        { _layout_plots =
            [ toPlot $ candleinfo "Candle" candles cyan,
              toPlot $ linesinfo "Closing" closings green
            ],
          _layout_title = T.unpack title
        }
    volumesLayout =
      def
        { _layout_plots = [plotBars $ barinfo "Volume" volumes cyan]
        }

    (candles, closings, volumes) = unzip3 $ do
      QuoteData {..} <- F.toList quotes
      return (Candle day low open 0 close high, (day, close), (day, [volume]))

    candleinfo label values color =
      def
        { _plot_candle_title = label,
          _plot_candle_values = values,
          _plot_candle_line_style = linestyle 1 gray,
          _plot_candle_rise_fill_style = fillstyle white,
          _plot_candle_fall_fill_style = fillstyle color,
          _plot_candle_fill = True,
          _plot_candle_tick_length = 5,
          _plot_candle_width = 5,
          _plot_candle_centre = 0
        }

    linesinfo label values color =
      def
        { _plot_lines_title = label,
          _plot_lines_values = [values],
          _plot_lines_style = linestyle 1 color
        }

    barinfo label values color =
      def
        { _plot_bars_titles = [label],
          _plot_bars_values = values,
          _plot_bars_item_styles = [(fillstyle color, Nothing)]
        }

    fillstyle color = solidFillStyle $ opaque color
    linestyle n color = solidLine n $ opaque color
