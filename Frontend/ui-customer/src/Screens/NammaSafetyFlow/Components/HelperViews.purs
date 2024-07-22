module Screens.NammaSafetyFlow.Components.HelperViews where

import Prelude (Unit, ($), (<>), (==), map, (/=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), PrestoDOM, Visibility(..), Orientation(..), background, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, padding, text, textView, visibility, weight, width, relativeLayout, orientation, alignParentBottom, shimmerFrameLayout, stroke)
import Common.Types.App (LazyCheck(..))
import Effect (Effect)
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Screens.Types as ST
import Data.Array as DA

emptyTextView :: forall w. PrestoDOM (Effect Unit) w
emptyTextView = textView [ visibility GONE ]

layoutWithWeight :: forall w. PrestoDOM (Effect Unit) w
layoutWithWeight = linearLayout [ weight 1.0 ] []

separatorView :: forall w. String -> Boolean -> Margin  -> PrestoDOM (Effect Unit) w
separatorView color' visibility' margin' =
  linearLayout
    [ height (V 1)
    , width MATCH_PARENT
    , margin margin'
    , background color'
    , visibility $ boolToVisibility visibility'
    ]
    []

recommendContactsToInstallView :: forall w. String -> PrestoDOM (Effect Unit) w
recommendContactsToInstallView appName =
  textView
    $ [ text $ getString $ RECOMMEND_EMERGENCY_CONTACTS_TO_INSTALL appName
      , color Color.black700
      , background Color.blue600
      , padding $ Padding 8 12 8 12
      , margin $ MarginVertical 16 16
      , cornerRadius 8.0
      ]
    <> FontStyle.body1 LanguageStyle

safetyPartnerView :: forall w. LazyCheck -> PrestoDOM (Effect Unit) w
safetyPartnerView _lazy =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background Color.blackOpacity12
    , gravity CENTER_VERTICAL
    , cornerRadius 12.0
    , margin $ MarginBottom 16
    , padding $ Padding 16 16 16 16
    , visibility $ boolToVisibility $ getValueToLocalStore CUSTOMER_LOCATION == "Bangalore"
    ]
    [ textView
        $ [ text $ getString OUR_SAFETY_PARTNER
          , color Color.white900
          ]
        <> FontStyle.tags TypoGraphy
    , layoutWithWeight
    , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER_VERTICAL
        ]
        [ imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_bangalore_police"
            , width $ V 36
            , height $ V 36
            , margin $ MarginRight 8
            ]
        , textView
            $ [ text $ getString BANGALURU_CITY_POLICE
              , color Color.white900
              ]
            <> FontStyle.tags TypoGraphy
        ]
    ]

shimmerView :: forall w. ST.NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
shimmerView state =
  relativeLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin 16 16 16 16
    , visibility if state.props.showShimmer then VISIBLE else GONE
    ]
    [ sfl (V 400) 16 1 true
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , alignParentBottom "true,-1"
        ]
        [ sfl (V 80) 130 3 (getValueToLocalStore IS_SOS_ACTIVE == "true")
        , sfl (V 80) 130 1 (getValueToLocalStore IS_SOS_ACTIVE /= "true")
        ]
    ]

sfl :: forall w. Length -> Int -> Int -> Boolean -> PrestoDOM (Effect Unit) w
sfl height' marginTop numberOfBoxes visibility' =
  shimmerFrameLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop marginTop
    , visibility $ boolToVisibility visibility'
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ]
        ( map
            ( \_ ->
                linearLayout
                  [ height height'
                  , background Color.greyDark
                  , cornerRadius 12.0
                  , weight 1.0
                  , stroke $ "1," <> Color.grey900
                  , margin $ Margin 4 4 4 4
                  ]
                  []
            )
            (1 DA... numberOfBoxes)
        )
    ]