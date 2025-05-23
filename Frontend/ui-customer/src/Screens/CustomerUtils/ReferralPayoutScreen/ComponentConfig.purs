{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ReferralPayoutScreen.ComponentConfig where

import Components.GenericHeader as GenericHeader
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM ( Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.Types as ST
import Screens.ReferralPayoutScreen.ScreenData as ST
import Styles.Colors as Color
import Common.Types.App
import Helpers.Utils (fetchImage, FetchImageFrom(..), showTitle, isParentView)
import Common.Types.App (LazyCheck(..))
import Prelude
import Components.PrimaryButton as PrimaryButton

genericHeaderConfig :: ST.ReferralPayoutScreenState -> GenericHeader.Config
genericHeaderConfig state = let 
  config = if state.data.appConfig.nyBrandingVisibility then GenericHeader.merchantConfig else GenericHeader.config
  btnVisibility =  config.prefixImageConfig.visibility
  titleVisibility = if showTitle FunctionCall then config.visibility else GONE
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , visibility = btnVisibility
      , margin = Margin 8 8 8 8 
      , layoutMargin = Margin 4 4 4 4
      , enableRipple = true
      } 
    , textConfig {
        text = getString $ if state.props.isEarnings then EARNINGS else SHARE_AND_REFER
      , color = Color.darkCharcoal
      }
    , suffixImageConfig {
        visibility = GONE
      }
    , visibility = titleVisibility
    }
  in genericHeaderConfig'

primaryButtonConfig :: ST.ReferralPayoutScreenState -> PrimaryButton.Config
primaryButtonConfig state = PrimaryButton.config{
  textConfig {
    text = getString ADD_UPI_ID
  }, 
  margin = MarginTop 24
  , alpha = if state.data.verificationStatus == ST.UpiVerified then 1.0 else 0.4
  , isClickable = state.data.verificationStatus == ST.UpiVerified
  , id = "ReferralPayoutScreenPB"
}

donePrimaryButtonConfig :: ST.ReferralPayoutScreenState -> PrimaryButton.Config
donePrimaryButtonConfig state = PrimaryButton.config{
  textConfig {
    text = getString DONE
  }, 
  margin = MarginTop 24
  , id = "ReferralPayoutScreenPBDone"

}
