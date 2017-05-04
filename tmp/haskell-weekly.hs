#!/usr/bin/env stack
{-
  stack
  --resolver lts-8.13
  script
  --package Chart
  --package Chart-diagrams
  --package colour
  --package time
-}
{-# OPTIONS_GHC -Wall #-}

import Data.Colour.SRGB
import Data.Time
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy


main :: IO ()
main = do

  chart
    "haskell-weekly-recipients.svg"
    "Haskell Weekly recipients"
    "Number of recipients"
    recipients

  chart
    "haskell-weekly-open-rate.svg"
    "Haskell Weekly open rate"
    "Open rate"
    openRates

  chart
    "haskell-weekly-click-rate.svg"
    "Haskell Weekly click rate"
    "Click rate"
    clickRates


recipients :: [(Int, Int)]
recipients = for campaigns (\ campaign ->
  (campaignIndex campaign, campaignRecipients campaign))

openRates :: [(Int, Percent)]
openRates = for campaigns (\ campaign ->
  (campaignIndex campaign, 100 * campaignOpenRate campaign))

clickRates :: [(Int, Percent)]
clickRates = for campaigns (\ campaign ->
  (campaignIndex campaign, 100 * campaignClickRate campaign))


data Campaign = Campaign
  {
    campaignId :: String,
    campaignIndex :: Int,
    campaignSentAt :: UTCTime,
    campaignTitle :: String,
    campaignRecipients :: Int,
    campaignOpenRate :: Percent,
    campaignClickRate :: Percent
  }

campaigns :: [Campaign]
campaigns =
  [
    Campaign {
      campaignId = "ea216ab278",
      campaignIndex = 0,
      campaignSentAt = parseSentAt "2016-05-05T13:30:00+00:00",
      campaignTitle = "Haskell Weekly - Issue 1 - May 5 2016",
      campaignRecipients = 7,
      campaignOpenRate = 0.85714285714286,
      campaignClickRate = 0.71428571428571
    },
    Campaign {
      campaignId = "2135c9d5eb",
      campaignIndex = 1,
      campaignSentAt = parseSentAt "2016-05-12T13:30:00+00:00",
      campaignTitle = "Haskell Weekly - Issue 2 - May 12 2016",
      campaignRecipients = 31,
      campaignOpenRate = 0.74193548387097,
      campaignClickRate = 0.38709677419355
    },
    Campaign {
      campaignId = "70e62fba24",
      campaignIndex = 2,
      campaignSentAt = parseSentAt "2016-05-19T13:36:45+00:00",
      campaignTitle = "Haskell Weekly - Issue 3 - May 19 2016",
      campaignRecipients = 40,
      campaignOpenRate = 0.775,
      campaignClickRate = 0.4
    },
    Campaign {
      campaignId = "ab3477f7f7",
      campaignIndex = 3,
      campaignSentAt = parseSentAt "2016-05-26T13:30:00+00:00",
      campaignTitle = "Haskell Weekly - Issue 4 - May 26 2016",
      campaignRecipients = 45,
      campaignOpenRate = 0.75555555555556,
      campaignClickRate = 0.26666666666667
    },
    Campaign {
      campaignId = "8b3b87c8fe",
      campaignIndex = 4,
      campaignSentAt = parseSentAt "2016-06-02T13:30:00+00:00",
      campaignTitle = "Haskell Weekly - Issue 5 - June 2 2016",
      campaignRecipients = 61,
      campaignOpenRate = 0.73770491803279,
      campaignClickRate = 0.49180327868852
    },
    Campaign {
      campaignId = "b502fe2ea0",
      campaignIndex = 5,
      campaignSentAt = parseSentAt "2016-06-09T13:30:00+00:00",
      campaignTitle = "Haskell Weekly - Issue 6 - June 9 2016",
      campaignRecipients = 71,
      campaignOpenRate = 0.76056338028169,
      campaignClickRate = 0.43661971830986
    },
    Campaign {
      campaignId = "14b0b97a2f",
      campaignIndex = 6,
      campaignSentAt = parseSentAt "2016-06-16T13:30:00+00:00",
      campaignTitle = "Haskell Weekly - Issue 7 - June 16 2016",
      campaignRecipients = 74,
      campaignOpenRate = 0.7027027027027,
      campaignClickRate = 0.37837837837838
    },
    Campaign {
      campaignId = "b57ba0cb4b",
      campaignIndex = 7,
      campaignSentAt = parseSentAt "2016-06-23T13:30:00+00:00",
      campaignTitle = "Haskell Weekly - Issue 8 - June 23 2016",
      campaignRecipients = 75,
      campaignOpenRate = 0.73333333333333,
      campaignClickRate = 0.38666666666667
    },
    Campaign {
      campaignId = "a86a003ada",
      campaignIndex = 8,
      campaignSentAt = parseSentAt "2016-06-30T13:30:00+00:00",
      campaignTitle = "Haskell Weekly - Issue 9 - June 30 2016",
      campaignRecipients = 76,
      campaignOpenRate = 0.67105263157895,
      campaignClickRate = 0.35526315789474
    },
    Campaign {
      campaignId = "8fd00b73d2",
      campaignIndex = 9,
      campaignSentAt = parseSentAt "2016-07-07T15:00:00+00:00",
      campaignTitle = "Haskell Weekly - Issue 10 - July 7 2016",
      campaignRecipients = 77,
      campaignOpenRate = 0.76623376623377,
      campaignClickRate = 0.32467532467532
    },
    Campaign {
      campaignId = "8f52c9e6f7",
      campaignIndex = 10,
      campaignSentAt = parseSentAt "2016-07-14T13:30:00+00:00",
      campaignTitle = "Haskell Weekly - Issue 11 - July 14 2016",
      campaignRecipients = 77,
      campaignOpenRate = 0.68831168831169,
      campaignClickRate = 0.31168831168831
    },
    Campaign {
      campaignId = "8e9c1e85ea",
      campaignIndex = 11,
      campaignSentAt = parseSentAt "2016-07-21T13:30:00+00:00",
      campaignTitle = "Haskell Weekly - Issue 12 - July 21 2016",
      campaignRecipients = 78,
      campaignOpenRate = 0.57692307692308,
      campaignClickRate = 0.26923076923077
    },
    Campaign {
      campaignId = "5264f3bb33",
      campaignIndex = 12,
      campaignSentAt = parseSentAt "2016-07-28T13:30:00+00:00",
      campaignTitle = "Haskell Weekly - Issue 13 - July 28 2016",
      campaignRecipients = 85,
      campaignOpenRate = 0.64705882352941,
      campaignClickRate = 0.45882352941176
    },
    Campaign {
      campaignId = "871fbc6806",
      campaignIndex = 13,
      campaignSentAt = parseSentAt "2016-08-04T14:00:00+00:00",
      campaignTitle = "Haskell Weekly - Issue 14 - August 4 2016",
      campaignRecipients = 86,
      campaignOpenRate = 0.72093023255814,
      campaignClickRate = 0.37209302325581
    },
    Campaign {
      campaignId = "7cd8b44ad7",
      campaignIndex = 14,
      campaignSentAt = parseSentAt "2016-08-11T13:15:53+00:00",
      campaignTitle = "Haskell Weekly - Issue 15 - August 11 2016",
      campaignRecipients = 90,
      campaignOpenRate = 0.64444444444444,
      campaignClickRate = 0.35555555555556
    },
    Campaign {
      campaignId = "f85d0c5391",
      campaignIndex = 15,
      campaignSentAt = parseSentAt "2016-08-18T13:02:46+00:00",
      campaignTitle = "Haskell Weekly - 08/17/2016",
      campaignRecipients = 338,
      campaignOpenRate = 0.72106824925816,
      campaignClickRate = 0.39169139465875
    },
    Campaign {
      campaignId = "ec0f2785ed",
      campaignIndex = 16,
      campaignSentAt = parseSentAt "2016-08-25T13:02:09+00:00",
      campaignTitle = "Haskell Weekly - 08/24/2016",
      campaignRecipients = 363,
      campaignOpenRate = 0.70994475138122,
      campaignClickRate = 0.39502762430939
    },
    Campaign {
      campaignId = "e16363fae0",
      campaignIndex = 17,
      campaignSentAt = parseSentAt "2016-09-01T14:03:23+00:00",
      campaignTitle = "Haskell Weekly - 08/31/2016",
      campaignRecipients = 377,
      campaignOpenRate = 0.704,
      campaignClickRate = 0.30933333333333
    },
    Campaign {
      campaignId = "d34f7fb6ee",
      campaignIndex = 18,
      campaignSentAt = parseSentAt "2016-09-08T15:02:52+00:00",
      campaignTitle = "Haskell Weekly - 09/07/2016",
      campaignRecipients = 390,
      campaignOpenRate = 0.67866323907455,
      campaignClickRate = 0.29562982005141
    },
    Campaign {
      campaignId = "19d44c4a2d",
      campaignIndex = 19,
      campaignSentAt = parseSentAt "2016-09-15T14:01:14+00:00",
      campaignTitle = "Haskell Weekly - 09/14/2016",
      campaignRecipients = 398,
      campaignOpenRate = 0.68261964735516,
      campaignClickRate = 0.3375314861461
    },
    Campaign {
      campaignId = "fe30140646",
      campaignIndex = 20,
      campaignSentAt = parseSentAt "2016-09-22T14:01:17+00:00",
      campaignTitle = "Haskell Weekly - 09/21/2016",
      campaignRecipients = 403,
      campaignOpenRate = 0.66169154228856,
      campaignClickRate = 0.2636815920398
    },
    Campaign {
      campaignId = "885539a225",
      campaignIndex = 21,
      campaignSentAt = parseSentAt "2016-09-29T14:01:38+00:00",
      campaignTitle = "Haskell Weekly - 09/28/2016",
      campaignRecipients = 406,
      campaignOpenRate = 0.68472906403941,
      campaignClickRate = 0.31773399014778
    },
    Campaign {
      campaignId = "cb0189920b",
      campaignIndex = 22,
      campaignSentAt = parseSentAt "2016-10-06T14:00:52+00:00",
      campaignTitle = "Haskell Weekly - 10/05/2016",
      campaignRecipients = 412,
      campaignOpenRate = 0.64963503649635,
      campaignClickRate = 0.30170316301703
    },
    Campaign {
      campaignId = "02d1382424",
      campaignIndex = 23,
      campaignSentAt = parseSentAt "2016-10-13T14:00:53+00:00",
      campaignTitle = "Haskell Weekly - 10/12/2016",
      campaignRecipients = 413,
      campaignOpenRate = 0.63106796116505,
      campaignClickRate = 0.33980582524272
    },
    Campaign {
      campaignId = "814aed66b9",
      campaignIndex = 24,
      campaignSentAt = parseSentAt "2016-10-21T05:00:27+00:00",
      campaignTitle = "Haskell Weekly - 10/19/2016",
      campaignRecipients = 426,
      campaignOpenRate = 0.65258215962441,
      campaignClickRate = 0.27934272300469
    },
    Campaign {
      campaignId = "0c965bfbf7",
      campaignIndex = 25,
      campaignSentAt = parseSentAt "2016-10-27T14:00:48+00:00",
      campaignTitle = "Haskell Weekly - 10/26/2016",
      campaignRecipients = 432,
      campaignOpenRate = 0.63425925925926,
      campaignClickRate = 0.27083333333333
    },
    Campaign {
      campaignId = "5714aecdc2",
      campaignIndex = 26,
      campaignSentAt = parseSentAt "2016-11-03T14:01:12+00:00",
      campaignTitle = "Haskell Weekly - 11/02/2016",
      campaignRecipients = 438,
      campaignOpenRate = 0.66666666666667,
      campaignClickRate = 0.337899543379
    },
    Campaign {
      campaignId = "32402de9c0",
      campaignIndex = 27,
      campaignSentAt = parseSentAt "2016-11-10T15:03:55+00:00",
      campaignTitle = "Haskell Weekly - 11/09/2016",
      campaignRecipients = 449,
      campaignOpenRate = 0.69042316258352,
      campaignClickRate = 0.38084632516704
    },
    Campaign {
      campaignId = "1c840f498a",
      campaignIndex = 28,
      campaignSentAt = parseSentAt "2016-11-17T15:01:50+00:00",
      campaignTitle = "Haskell Weekly - 11/16/2016",
      campaignRecipients = 461,
      campaignOpenRate = 0.65509761388286,
      campaignClickRate = 0.23210412147505
    },
    Campaign {
      campaignId = "ad499b8148",
      campaignIndex = 29,
      campaignSentAt = parseSentAt "2016-11-24T15:01:31+00:00",
      campaignTitle = "Haskell Weekly - 11/23/2016",
      campaignRecipients = 476,
      campaignOpenRate = 0.69117647058824,
      campaignClickRate = 0.31932773109244
    },
    Campaign {
      campaignId = "fe71065be7",
      campaignIndex = 30,
      campaignSentAt = parseSentAt "2016-12-01T15:02:42+00:00",
      campaignTitle = "Haskell Weekly - 11/30/2016",
      campaignRecipients = 493,
      campaignOpenRate = 0.64503042596349,
      campaignClickRate = 0.27789046653144
    },
    Campaign {
      campaignId = "684f1d9463",
      campaignIndex = 31,
      campaignSentAt = parseSentAt "2016-12-08T15:01:08+00:00",
      campaignTitle = "Haskell Weekly - 12/07/2016",
      campaignRecipients = 509,
      campaignOpenRate = 0.66208251473477,
      campaignClickRate = 0.24557956777996
    },
    Campaign {
      campaignId = "e702b23ce1",
      campaignIndex = 32,
      campaignSentAt = parseSentAt "2016-12-15T15:01:35+00:00",
      campaignTitle = "Haskell Weekly - 12/14/2016",
      campaignRecipients = 570,
      campaignOpenRate = 0.63859649122807,
      campaignClickRate = 0.31929824561404
    },
    Campaign {
      campaignId = "3675e37b14",
      campaignIndex = 33,
      campaignSentAt = parseSentAt "2016-12-22T15:01:37+00:00",
      campaignTitle = "Haskell Weekly - 12/21/2016",
      campaignRecipients = 610,
      campaignOpenRate = 0.65901639344262,
      campaignClickRate = 0.28360655737705
    },
    Campaign {
      campaignId = "861fb9bdd5",
      campaignIndex = 34,
      campaignSentAt = parseSentAt "2016-12-29T15:01:55+00:00",
      campaignTitle = "Haskell Weekly - 12/28/2016",
      campaignRecipients = 630,
      campaignOpenRate = 0.66349206349206,
      campaignClickRate = 0.4031746031746
    },
    Campaign {
      campaignId = "2e9e82f366",
      campaignIndex = 35,
      campaignSentAt = parseSentAt "2017-01-05T15:03:02+00:00",
      campaignTitle = "Haskell Weekly - 01/04/2017",
      campaignRecipients = 649,
      campaignOpenRate = 0.65793528505393,
      campaignClickRate = 0.24036979969183
    },
    Campaign {
      campaignId = "e5318c0417",
      campaignIndex = 36,
      campaignSentAt = parseSentAt "2017-01-12T18:02:33+00:00",
      campaignTitle = "Haskell Weekly - 01/11/2017",
      campaignRecipients = 696,
      campaignOpenRate = 0.6867816091954,
      campaignClickRate = 0.29885057471264
    },
    Campaign {
      campaignId = "d1f16b3bfc",
      campaignIndex = 37,
      campaignSentAt = parseSentAt "2017-01-19T15:02:55+00:00",
      campaignTitle = "Haskell Weekly - 01/18/2017",
      campaignRecipients = 719,
      campaignOpenRate = 0.65229485396384,
      campaignClickRate = 0.34631432545202
    },
    Campaign {
      campaignId = "f50c671e92",
      campaignIndex = 38,
      campaignSentAt = parseSentAt "2017-01-26T15:03:26+00:00",
      campaignTitle = "Haskell Weekly - 01/25/2017",
      campaignRecipients = 737,
      campaignOpenRate = 0.64314789687924,
      campaignClickRate = 0.29715061058345
    },
    Campaign {
      campaignId = "fb8a8b946b",
      campaignIndex = 39,
      campaignSentAt = parseSentAt "2017-02-02T15:02:46+00:00",
      campaignTitle = "Haskell Weekly - 02/01/2017",
      campaignRecipients = 767,
      campaignOpenRate = 0.66623207301173,
      campaignClickRate = 0.33637548891786
    },
    Campaign {
      campaignId = "278d8744ac",
      campaignIndex = 40,
      campaignSentAt = parseSentAt "2017-02-09T15:02:28+00:00",
      campaignTitle = "Haskell Weekly - 02/08/2017",
      campaignRecipients = 788,
      campaignOpenRate = 0.65609137055838,
      campaignClickRate = 0.33121827411168
    },
    Campaign {
      campaignId = "b6f1d0b027",
      campaignIndex = 41,
      campaignSentAt = parseSentAt "2017-02-17T05:01:31+00:00",
      campaignTitle = "Haskell Weekly - 02/15/2017",
      campaignRecipients = 816,
      campaignOpenRate = 0.65441176470588,
      campaignClickRate = 0.27083333333333
    },
    Campaign {
      campaignId = "8c5ae9a689",
      campaignIndex = 42,
      campaignSentAt = parseSentAt "2017-02-23T16:00:31+00:00",
      campaignTitle = "Haskell Weekly - 02/22/2017",
      campaignRecipients = 830,
      campaignOpenRate = 0.6578313253012,
      campaignClickRate = 0.3566265060241
    },
    Campaign {
      campaignId = "a5be133807",
      campaignIndex = 43,
      campaignSentAt = parseSentAt "2017-03-02T16:02:31+00:00",
      campaignTitle = "Haskell Weekly - 03/01/2017",
      campaignRecipients = 853,
      campaignOpenRate = 0.65334900117509,
      campaignClickRate = 0.35605170387779
    },
    Campaign {
      campaignId = "57bffedc1c",
      campaignIndex = 44,
      campaignSentAt = parseSentAt "2017-03-09T16:01:44+00:00",
      campaignTitle = "Haskell Weekly - 03/08/2017",
      campaignRecipients = 877,
      campaignOpenRate = 0.65789473684211,
      campaignClickRate = 0.30892448512586
    },
    Campaign {
      campaignId = "662dfe05f5",
      campaignIndex = 45,
      campaignSentAt = parseSentAt "2017-03-16T15:02:17+00:00",
      campaignTitle = "Haskell Weekly - 03/15/2017",
      campaignRecipients = 890,
      campaignOpenRate = 0.63851351351351,
      campaignClickRate = 0.31418918918919
    },
    Campaign {
      campaignId = "2d3a1a5f40",
      campaignIndex = 46,
      campaignSentAt = parseSentAt "2017-03-23T15:02:35+00:00",
      campaignTitle = "Haskell Weekly - 03/22/2017",
      campaignRecipients = 925,
      campaignOpenRate = 0.62946912242687,
      campaignClickRate = 0.23943661971831
    },
    Campaign {
      campaignId = "f74e2a96c1",
      campaignIndex = 47,
      campaignSentAt = parseSentAt "2017-03-30T15:01:24+00:00",
      campaignTitle = "Haskell Weekly - 03/29/2017",
      campaignRecipients = 937,
      campaignOpenRate = 0.65882352941176,
      campaignClickRate = 0.31122994652406
    },
    Campaign {
      campaignId = "024e836408",
      campaignIndex = 48,
      campaignSentAt = parseSentAt "2017-04-06T15:02:28+00:00",
      campaignTitle = "Haskell Weekly - 04/05/2017",
      campaignRecipients = 961,
      campaignOpenRate = 0.65135699373695,
      campaignClickRate = 0.33089770354906
    },
    Campaign {
      campaignId = "2710f0ca23",
      campaignIndex = 49,
      campaignSentAt = parseSentAt "2017-04-13T15:01:52+00:00",
      campaignTitle = "Haskell Weekly - 04/12/2017",
      campaignRecipients = 970,
      campaignOpenRate = 0.62111801242236,
      campaignClickRate = 0.26811594202899
    },
    Campaign {
      campaignId = "fac2d641ab",
      campaignIndex = 50,
      campaignSentAt = parseSentAt "2017-04-20T15:05:02+00:00",
      campaignTitle = "Haskell Weekly - 04/19/2017",
      campaignRecipients = 991,
      campaignOpenRate = 0.62145748987854,
      campaignClickRate = 0.26012145748988
    },
    Campaign {
      campaignId = "b2e2060b08",
      campaignIndex = 51,
      campaignSentAt = parseSentAt "2017-04-27T15:02:39+00:00",
      campaignTitle = "Haskell Weekly - 04/26/2017",
      campaignRecipients = 1010,
      campaignOpenRate = 0.60615079365079,
      campaignClickRate = 0.26984126984127
    }
  ]


chart :: (Num y, PlotValue y) =>
  FilePath -> String -> String -> [(Int, y)] -> IO ()
chart file title legend values = toFile def file (do
  let color = sRGB24 92 53 102

  layout_title .= title
  layout_x_axis .laxis_title .= "Issue number"
  layout_x_axis . laxis_generate .= scaledIntAxis
    (LinearAxisParams (map (\ i -> show (i + 1))) 5 1) (0, 50)

  plot (liftEC (do
    plot_fillbetween_title .= legend
    plot_fillbetween_style . fill_color .= withOpacity color 0.5
    plot_fillbetween_values .= map (\ (x, y) -> (x, (0, y))) values))

  plot (liftEC (do
    plot_lines_style . line_width .= 2
    plot_lines_style . line_color .= opaque color
    plot_lines_values .= [values])))

for :: [a] -> (a -> b) -> [b]
for = flip map

parseSentAt :: String -> UTCTime
parseSentAt = parseTimeOrError False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"
