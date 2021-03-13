module MoonLander
  ( component
  ) where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as ES

import Graphics.Canvas (Context2D)
import Graphics.Canvas as GC
import Unsafe.Coerce (unsafeCoerce)
import Partial.Unsafe (unsafePartial)

import Effect (Effect, foreachE)
import Effect.Console (log)
import Effect.Class
import Effect.Aff.Class
import Effect.Aff (Aff, delay, Milliseconds(..), makeAff)

import Web.Event.Event as E
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.HTML.HTMLImageElement as IE

import Data.Int
import Data.Array
import Data.Traversable
import Math as Math
import Data.Either
import Effect.Exception
import Effect.Random

import Data.Number.Format

import CanvasExtra as GCE

data Action
  = Init | Draw | HandleKeyUp H.SubscriptionId KeyboardEvent | HandleKeyDown H.SubscriptionId KeyboardEvent

data GameResult
  = Success | Failure

data GameOverWait
  = Waiting | Ready

data GameStatus
  = Opening | Playing | GameOver GameResult GameOverWait

type State = {
  canvas ::
  { width :: Int
  , height :: Int
  }
, stars :: Array { 
    x :: Number
  , y :: Number
  }
, images :: {
    rocket :: Maybe GC.CanvasImageSource
  }
, game :: 
  { altitude :: Number
  , speed :: Number
  , acceleration :: Number
  , keyFlag :: Boolean
  , gameStatus :: GameStatus
  }
}

component :: forall q i o m. (MonadAff m) => (MonadEffect m) => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction
      , initialize = Just Init
      }
    }

initialState :: forall i. i -> State
initialState _ = {
  canvas: 
  { 
    width: 400
  , height: 600
  }
, stars: []
, images: 
  { rocket: Nothing
  }
, game: initialGame
}

initialGame = { altitude: 100.0
  , speed: 0.0
  , acceleration: 0.0
  , keyFlag: false
  , gameStatus: Opening
  }

render :: forall m. (MonadEffect m) => State -> H.ComponentHTML Action () m
render state = do
  HH.div_
    [ HH.div
      [ ]
      [ HH.text "Moon Lander" ]
    , HH.div
      [ HP.attr (HC.AttrName "style") "display: inline-block" ]
      [ HH.canvas
        [ HP.id_ "canvas"
        , HP.width state.canvas.width
        , HP.height state.canvas.height
        ]
      , HH.canvas
        [ HP.id_ "hidden_canvas"
        , HP.width state.canvas.width
        , HP.height state.canvas.height
        , HP.attr (HC.AttrName "style") "display: none"
        ]
      ]
    , explain
    ]

explain :: forall w i. HH.HTML w i
explain = do
  HH.div
    [ HP.attr (HC.AttrName "style") "display: inline-block; vertical-align: top; margin-left: 20px;" ]
    [ HH.h2 [] [ HH.text "遊び方" ]
    , HH.div [] [ HH.text "スペースキーを押すと、ロケットが上方に加速します。" ]
    , HH.div [] [ HH.text "スペースキーを離すとと、ロケットが重力に引かれて落下し始めます。" ]
    , HH.div [] [ HH.text "緩やかな速度(-2.0 km/h 以上)で着陸すれば成功です。" ]
    , HH.div [] [ HH.text "注意：PC専用です。現在、スマホには対応していません。" ]
    , HH.h3 [] [ HH.text "画像ファイル" ]
    , HH.div []
      [ HH.text "ロケット画像(rocket.png)"
      , HH.br []
      , HH.a
        [ HP.attr (HC.AttrName "href") "https://www.kenney.nl/assets"
        , HP.attr (HC.AttrName "target") "_blank" ]
        [ HH.text "(C) KENNEY" ]
      ]
    , HH.h3 [] [HH.text "参考プログラム (Python)" ]
    , HH.div []
      [ HH.text "日経ソフトウェア 2020年5月号　第２付録"
      , HH.br []
      , HH.a
        [ HP.attr (HC.AttrName "href") "http://bizboard.nikkeibp.co.jp/kijiken/summary/20200324/NSW0269F_4688963a.html"
        , HP.attr (HC.AttrName "target") "_blank" ]
        [ HH.text "5日でできる！ Pythonでゲーム作成入門／第四日目" ]
      ]
    , HH.h3 [] [HH.text "Github"]
    , HH.div []
      [ HH.a
        [ HP.attr (HC.AttrName "href") "https://github.com/knight9999/MoonLanderPureScript"
        , HH.attr (HC.AttrName "target") "_blank"
        ]
        [ HH.text "https://github.com/knight9999/MoonLanderPureScript" ]
      ]
    ]


doLoop :: forall m o. (MonadAff m) => (MonadEffect m) => H.HalogenM State Action () o m Unit
doLoop = do
    H.liftAff $ delay (Milliseconds $ 1000.0/60.0)
    s <- H.get
    let acceleration = if s.game.altitude < 300.0
        then 
          if s.game.keyFlag
          then 0.25
          else -0.25
        else
          -0.25
    let speed = s.game.speed + acceleration
    let altitude = s.game.altitude + speed
    H.modify_ (\st -> st { game = st.game
      { acceleration = acceleration
      , speed = speed
      , altitude = altitude 
      } })
    handleAction Draw
    s <- H.get
    case s.game.gameStatus of 
      Playing -> do
        if s.game.altitude <= 0.0 
        then do
          let result =
                  if s.game.speed >= -2.0
                    then Success
                    else Failure
          H.modify_ (\st -> st { game = st.game { gameStatus = GameOver result Waiting }})
          handleAction Draw
          H.liftAff $ delay (Milliseconds $ 500.0)
          H.modify_ (\st -> st { game = s.game { gameStatus = GameOver result Ready }})
          handleAction Draw
        else
          doLoop
      _ -> do
        pure unit


handleAction :: forall m o. (MonadAff m) => (MonadEffect m) => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Init -> do
    s <- H.get
    -- Load Images
    rocket_src <- H.liftAff do
      rocket_src' <- tryLoadImageAff "images/rocket.png"
      pure rocket_src'
    H.modify_ (\st -> st { images = (st.images { rocket = Just rocket_src }) })

    -- Set Stars
    stars <- liftEffect $ traverse (\_ -> do
      x <- toNumber <$> randomInt 1 (s.canvas.width-2)
      y <- toNumber <$> randomInt 1 (s.canvas.height-2)
      pure $ { x: x, y: y}
    ) (range 1 20)
    H.modify_ (\st -> st { stars = stars })

    -- Draw Canvas
    handleAction Draw

    -- Setup KeyHandler
    document <- H.liftEffect $ Web.document =<< Web.window
    H.subscribe' \sid ->
      ES.eventListener
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (HandleKeyUp sid) <<< KE.fromEvent)
    H.subscribe' \sid ->
      ES.eventListener
        KET.keydown
        (HTMLDocument.toEventTarget document)
        (map (HandleKeyDown sid) <<< KE.fromEvent)
    pure unit

  HandleKeyDown sid ev -> do
    s <- H.get
    let char = KE.key ev
    if (char == " ") then do
      case s.game.gameStatus of
        Playing -> do
          H.modify_ (\st -> st { game = s.game { keyFlag = true } })
        Opening -> do
          H.modify_ (\st -> st { game = s.game { gameStatus = Playing, keyFlag = true } })
          handleAction Draw
          _ <- H.fork do
            doLoop
          pure unit
        GameOver result wait -> do
          case wait of 
            Waiting -> pure unit
            Ready -> do
              H.modify_ (\st -> st { game = initialGame })
              handleAction Draw
        _ -> do
          pure unit
    else 
      pure unit
    pure unit

  HandleKeyUp sid ev -> do
    s <- H.get
    let char = KE.key ev
    if (char == " ") then do
      case s.game.gameStatus of
        Playing -> do
          H.modify_ (\st -> st { game = s.game { keyFlag = false } })
        _ -> do
          pure unit
    else 
      pure unit
    pure unit


  Draw -> do
    s <- H.get
    let rocket = s.images.rocket
    H.liftEffect $ void $ unsafePartial do

      -- show Canvas
      Just canvas <- GC.getCanvasElementById "hidden_canvas"
      ctx <- GC.getContext2D canvas
      GC.setFillStyle ctx "#000"
      GC.fillPath ctx $ GC.rect ctx
        { x: 0.0
        , y: 0.0
        , width: (toNumber s.canvas.width)
        , height: (toNumber s.canvas.height)
        }

      -- show Stars
      GC.setFillStyle ctx "#FFF"      
      foreachE s.stars \position -> do
        GC.fillPath ctx $ GC.rect ctx
          { x: position.x - 1.0
          , y: position.y - 1.0
          , width: 3.0
          , height: 3.0 
          }
      
      -- Show Base
      GC.setStrokeStyle ctx "#CCC"
      foreachE (range 0 50) \i -> do
        GC.strokePath ctx $ do
          GC.moveTo ctx (toNumber $ (s.canvas.width/2)-50-i*3) (toNumber $ (s.canvas.height - 50)+i)
          GC.lineTo ctx (toNumber $ (s.canvas.width/2)+50+i*3) (toNumber $ (s.canvas.height - 50)+i)
          GC.closePath ctx
        
      -- show Rocket Fire
      case s.game.gameStatus of
        Playing -> 
          if s.game.keyFlag 
          then
            case rocket of
              Just rocket_src -> do
                let img = (unsafeCoerce rocket_src) :: IE.HTMLImageElement -- HTMLImageElementに変換しないと、widthとheightを取得できない
                w <- IE.naturalWidth img
                h <- IE.naturalHeight img
                let x = (toNumber s.canvas.width) / 2.0
                let y = ((toNumber $ s.canvas.height - 50) - s.game.altitude)
                GC.setLineWidth ctx 2.0
                foreachE (range 0 9) \i -> do
                  GC.setStrokeStyle ctx $ mkColor 255 (i*20) 0
                  GC.strokePath ctx $ GC.arc ctx
                    { x: x
                    , y: y
                    , radius: (toNumber i)
                    , start: 0.0
                    , end: 2.0 * Math.pi
                    }

                pure unit
              Nothing -> do
                pure unit
          else
            pure unit    
        _ ->
          pure unit

      -- show Rocket
      case rocket of 
        Just rocket_src -> do
          let img = (unsafeCoerce rocket_src) :: IE.HTMLImageElement -- HTMLImageElementに変換しないと、widthとheightを取得できない
          w <- IE.naturalWidth img
          h <- IE.naturalHeight img
          let x = ((toNumber s.canvas.width) - (toNumber w))/2.0
          let y = ((toNumber $ s.canvas.height - 50 - h) - s.game.altitude)
          let angle = 
                    case s.game.gameStatus of 
                      GameOver result _ -> case result of
                        Success -> 0.0
                        Failure -> 50.0
                      _ -> 0.0
          drawRocket ctx rocket_src x y (toNumber w) (toNumber h) angle          
          pure unit
        Nothing -> do
          pure unit

      -- show Speed
      GC.setTextAlign ctx GC.AlignLeft
      GC.setFont ctx "bold 18px \"ヒラギノ丸ゴ ProN W4\", \"Hiragino Maru Gothic ProN\", Meiryo, Arial, sans-serif"
      GC.setFillStyle ctx "#FFF"
      case s.game.gameStatus of 
        Opening -> pure unit
        _ ->  do
          GC.fillText ctx ("speed: " <> toStringWith (fixed 3) s.game.speed <> " km/h") 20.0 50.0

      -- show Title
      textAlign <- GC.textAlign ctx
      GC.setTextAlign ctx GC.AlignCenter
      GC.setFont ctx "bold 42px \"ヒラギノ丸ゴ ProN W4\", \"Hiragino Maru Gothic ProN\", Meiryo, Arial, sans-serif"
      -- drawText ctx "Moon Lander" 60.0 140.0 "#ff0" "#000" 6.0 
      drawText ctx "Moon Lander" (toNumber s.canvas.width/2.0) 140.0 "#ff0" "#000" 6.0 

      let 
        showHitSpace = do
          GC.setFont ctx "bold 36px \"ヒラギノ丸ゴ ProN W4\", \"Hiragino Maru Gothic ProN\", Meiryo, Arial, sans-serif"
          drawText ctx "Hit Space Key"  (toNumber s.canvas.width/2.0) 240.0 "#7f7" "#000" 6.0
        showGameClear = do
          GC.setFont ctx "bold 42px \"ヒラギノ丸ゴ ProN W4\", \"Hiragino Maru Gothic ProN\", Meiryo, Arial, sans-serif"
          drawText ctx "GAME CLEAR"  (toNumber s.canvas.width/2.0) 340.0 "#0ff" "#000" 6.0
        showGameOver = do
          GC.setFont ctx "bold 42px \"ヒラギノ丸ゴ ProN W4\", \"Hiragino Maru Gothic ProN\", Meiryo, Arial, sans-serif"
          drawText ctx "GAME OVER"  (toNumber s.canvas.width/2.0) 340.0 "#f77" "#000" 6.0

      case s.game.gameStatus of
        Opening -> showHitSpace
        GameOver Success _ -> showGameClear
        GameOver Failure _ -> showGameOver
        _ -> pure unit
          
      GC.setTextAlign ctx textAlign

      -- Double Buffering
      Just canvas2 <- GC.getCanvasElementById "canvas"
      ctx2 <- GC.getContext2D canvas2
      GC.drawImage ctx2 (GC.canvasElementToImageSource canvas) 0.0 0.0

      pure unit

drawText :: Context2D -> String -> Number -> Number -> String -> String -> Number -> Effect Unit
drawText ctx text x y color bkcolor bold = do
  let splitNumber = 16
  let splitLenNumber = 4
  let positions = map (\j ->
        let boldL = bold * (toNumber j / toNumber splitLenNumber)
        in map (\i ->
            let theta = (2.0 * Math.pi) / (toNumber splitNumber) * (toNumber i)
            in { x: boldL * (Math.cos theta)
              , y: boldL * (Math.sin theta)
              }
            ) (range 0 (splitNumber-1))
      ) (range 0 (splitLenNumber-1))

  let color_ = GCE.getFillStyle ctx
  GC.setFillStyle ctx color
  foreachE positions \rounds -> do
    foreachE rounds \p -> do
      GC.fillText ctx text (x+p.x) (y+p.y)

  GC.setFillStyle ctx bkcolor
  GC.fillText ctx text x y
  GC.setFillStyle ctx color_

  pure unit

drawRocket :: GC.Context2D -> GC.CanvasImageSource -> Number -> Number -> Number -> Number -> Number -> Effect Unit
drawRocket ctx rocket_src left top w h angle = do
  GC.save ctx
  GC.translate ctx { translateX: left + (w/2.0), translateY: top + (h/2.0) }
  GC.rotate ctx $ angle / 360.0 * (2.0 * Math.pi) 
  let img = (unsafeCoerce rocket_src) :: IE.HTMLImageElement -- HTMLImageElementに変換しないと、widthとheightを取得できない
  GC.drawImage ctx rocket_src (0.0-w/2.0) (0.0-h/2.0)          
  GC.restore ctx
  pure unit

tryLoadImageAff ::
  String ->
  Aff GC.CanvasImageSource
tryLoadImageAff path = makeAff wrappedFn
  where
    wrappedFn done = do
      GC.tryLoadImage path (\maybeImage -> case maybeImage of
        Just canvasImage -> done (Right canvasImage)
        Nothing -> done (Left (error $ "Could not load " <> path))
      )
      pure mempty -- return empty cancellation function

mkColor :: Int -> Int -> Int -> String
mkColor r g b = "rgb(" <> (toStringAs decimal r) <> "," <> (toStringAs decimal g) <> "," <> (toStringAs decimal b) <> ")"

