module Screens.ExtraChargeInfoScreen.Controller where

import Prelude
import Effect (Effect)
import PrestoDOM (Eval, update, Props, exit, continue, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable,defaultPerformLog)
import Screens.Types
import Log (trackAppActionClick, trackAppScreenRender, trackAppEndScreen)
import Engineering.Helpers.LogEvent (logEvent)
import Effect.Unsafe (unsafePerformEffect)
import Screens.ExtraChargeInfoScreen.ScreenData
import Components.PrimaryButton as PrimaryButton
import Data.Array as DA
import Data.Maybe
import Common.Types.App as CTA
import Data.Function.Uncurried
import JBridge
import Engineering.Helpers.Commons
import PrestoDOM.Core
import Debug
import Data.Number
import ConfigProvider
import Storage
import Helpers.Utils
import Components.ExtraChargeCard as ExtraChargeCard

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
    performLog = defaultPerformLog

data ScreenOutput = CloseModal

data Action = OnBackPressed
  | GotItBtn PrimaryButton.Action
  | OnCallSupportAC
  | OnQuestionClick Int
  | YoutubeVideoStatus String
  | NoAction
  | PauseVideo
  | BottomSheetStageChanged String
  | BottomSheetSlide String
  | ExtraChargeCardAC ExtraChargeCard.Action

eval :: Action -> ExtraChargeInfoScreenState -> Eval Action ScreenOutput ExtraChargeInfoScreenState
eval action state =
  case action of
    OnBackPressed -> exit $ CloseModal

    GotItBtn action1 ->
      case action1 of
        PrimaryButton.OnClick -> exit $ CloseModal
        PrimaryButton.NoAction -> update state

    OnCallSupportAC -> do
      let cityConfig = getCityConfig (getAppConfig appConfig).cityConfig $ getValueToLocalStore DRIVER_LOCATION
      void $ pure $ showDialer cityConfig.supportNumber false
      continue state

    OnQuestionClick index -> continue state {optionOpened = fromMaybe state.optionOpened $ DA.modifyAt index (\elem -> not $ elem) state.optionOpened}
    YoutubeVideoStatus status -> update state
    NoAction -> update state

    PauseVideo -> do
      continueWithCmd state [do
        let
          id = (getNewIDWithTag "extraChargeVideoView")
          url = "https://youtu.be/2MLe3lLmjyg?si=PaLEg6_wQ6TmEej8"
        push <- getPushFn Nothing "ExtraChargeInfoScreen"
        pure $ runFn5 setYoutubePlayer (getYoutubeData{videoType = "VIDEO",videoId = getVideoID url}) id (show CTA.PAUSE) push YoutubeVideoStatus
        pure NoAction
      ]

    BottomSheetStageChanged stage -> do
      if stage == "5" && state.sheetPositionRef < -0.65 then exit $ CloseModal else update state
    BottomSheetSlide stage -> do
      let number = fromString stage
      case number of
        Just position -> update state{sheetPositionRef = position}
        Nothing -> update state

    ExtraChargeCardAC _ -> update state



