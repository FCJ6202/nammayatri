module Screens.TicketBookingScreen.ComponentConfig where

import Prelude
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Screens.Types as ST
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..), visibility)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Styles.Colors as Color

genericHeaderConfig :: ST.TicketBookingScreenState -> GenericHeader.Config
genericHeaderConfig state = let
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
       visibility = VISIBLE
      , imageUrl = fetchImage FF_ASSET "ny_ic_chevron_left"
      , height = V 25
      , width = V 25
      , margin = Margin 16 16 16 16
      } 
    , padding = PaddingVertical 5 5
    , textConfig {
        text = case state.props.currentStage of
                  ST.DescriptionStage -> "Zoological Garden, Alipore"
                  ST.ChooseTicketStage -> "Choose Tickets"
                  ST.ViewTicketStage -> "Choose Tickets"
                  ST.TicketInfoStage -> state.props.selectedBookingInfo.ticketPlaceName
                  _ -> ""
      , color = Color.darkCharcoal
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'

primaryButtonConfig :: ST.TicketBookingScreenState -> PrimaryButton.Config
primaryButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
        { text = (case state.props.currentStage of 
                    ST.DescriptionStage -> "Book Tickets"
                    ST.ChooseTicketStage -> ("Pay ₹" <> (show state.data.totalAmount))
                    ST.ViewTicketStage -> "Book Tickets"
                    _ -> "")
        , color = Color.yellow900
        }
      , cornerRadius = 8.0
      , background = Color.black900 
      , isClickable = (state.props.currentStage == ST.DescriptionStage) || (state.props.currentStage == ST.ViewTicketStage) || (state.props.termsAndConditionsSelected && state.data.totalAmount > 0)
      , alpha = if (state.props.currentStage == ST.DescriptionStage) || (state.props.currentStage == ST.ViewTicketStage) || (state.props.termsAndConditionsSelected && state.data.totalAmount > 0) then 1.0 else 0.5
      , id = "BookTicketsButton"
      , margin = (MarginHorizontal 20 20)
      }
  in primaryButtonConfig'