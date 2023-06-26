module Components.MakePaymentModal.View where

import Components.MakePaymentModal.Controller (Action(..), MakePaymentModalState, FeeItem, FeeOptions(..))
import Effect (Effect)
import PrestoDOM.Types.Core (PrestoDOM)
import PrestoDOM.Types.DomAttributes (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..), Corners(..))
import PrestoDOM.Properties (alpha, background, color, cornerRadius, ellipsize, fontStyle, gravity, height, id, imageWithFallback, margin, orientation, padding, singleLine, stroke, text, textSize, visibility, weight, width, cornerRadii)
import PrestoDOM.Elements.Elements (imageView, textView, linearLayout)
import PrestoDOM.Events (afterRender, onBackPressed, onClick)
import Styles.Colors as Color
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Animation.Config as AnimConfig
import Components.PrimaryButton as PrimaryButton
import Font.Size as FontSize
import Font.Style as FontStyle
import Common.Types.App(LazyCheck(..))
import Halogen.VDom.DOM.Prop (Prop)
import Engineering.Helpers.Commons (screenWidth)
import JBridge as JBridge
import Data.Array as DA
import Prelude


view :: forall w . (Action -> Effect Unit) -> MakePaymentModalState -> PrestoDOM (Effect Unit) w
view push state = 
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , background Color.black9000
  , gravity BOTTOM
  ][ PrestoAnim.animationSet [ Anim.translateYAnim AnimConfig.translateYAnimConfig ] $
      linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , cornerRadii $ Corners 20.0 true true false false
      , orientation VERTICAL
      , background Color.white900
      , padding $ Padding 16 10 16 20
      , gravity CENTER
      ][ commonTV push state.title Color.black800 FontStyle.h2 CENTER 8 NoAction
        , commonTV push state.description Color.black800 FontStyle.subHeading1 CENTER 8 NoAction
        , paymentReview push state
        , commonTV push state.description2 Color.black800 FontStyle.subHeading2 CENTER 8 NoAction
        , primaryButton push state
        , commonTV push state.cancelButtonText Color.black650 FontStyle.subHeading2 CENTER 8 Cancel
      ]
  ]

primaryButton :: forall w . (Action -> Effect Unit) -> MakePaymentModalState -> PrestoDOM (Effect Unit) w
primaryButton push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ MarginTop 12
  ][PrimaryButton.view (push <<< PrimaryButtonActionController) (buttonConfig state)]

paymentReview :: forall w . (Action -> Effect Unit) -> MakePaymentModalState -> PrestoDOM (Effect Unit) w
paymentReview push state = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 8
    , background Color.blue600
    , cornerRadius 8.0
    , padding $ Padding 10 10 10 10
    ](DA.mapWithIndex (\index item -> 
      linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][  feeItem push state item
        , imageView
          [ width MATCH_PARENT
          , height $ V 2 
          , padding $ PaddingHorizontal 10 10
          , imageWithFallback "ny_ic_horizontal_dash,https://assets.juspay.in/nammayatri/images/user/ny_ic_horizontal_dash.png"
          , visibility if index == 0 then VISIBLE else GONE
          ]
      ]
      ) state.feeItem )
      
      
feeItem :: forall w . (Action -> Effect Unit) -> MakePaymentModalState -> FeeItem -> PrestoDOM (Effect Unit) w
feeItem push state item = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , padding $ Padding 10 10 10 10
  , cornerRadius 8.0
  , background if item.feeType == GST_PAYABLE then Color.yellow800 else Color.blue600
  , gravity CENTER_VERTICAL
  ][  textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text item.title
      , color Color.black800
      ] <> FontStyle.body1 TypoGraphy
    , imageView 
      [ height $ V 18
      , width $ V 18
      , margin $ MarginLeft 5
      , imageWithFallback "ny_ic_info,https://assets.juspay.in/nammayatri/images/user/ny_ic_information_grey.png"
      , visibility if item.feeType == GST_PAYABLE then VISIBLE else GONE
      , onClick push $ const Info
      ]
    , textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , gravity RIGHT
      , weight 1.0
      , color Color.black800
      , text $ "₹" <> (show item.val)
      ] <> FontStyle.body1 TypoGraphy
  ]

commonTV :: forall w .  (Action -> Effect Unit) -> String -> String -> (LazyCheck -> forall properties. (Array (Prop properties))) -> Gravity -> Int -> Action -> PrestoDOM (Effect Unit) w
commonTV push text' color' theme gravity' marginTop action = 
  textView $
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , text text'
  , color color'
  , gravity gravity'
  , margin $ MarginTop marginTop
  , onClick push $ const action
  ] <> theme TypoGraphy

buttonConfig :: MakePaymentModalState -> PrimaryButton.Config
buttonConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
        { text = state.okButtontext
        , color = Color.yellow900
        , textSize = FontSize.a_16
        , width = MATCH_PARENT
        }
      , height = V 55
      , gravity = CENTER
      , cornerRadius = 8.0
      , background = Color.black900
      , margin = MarginHorizontal 16 16
      , id = "MakePaymentButton"
      }
  in primaryButtonConfig'