import Reflex.Dom
import Safe (readMay)


main :: IO ()
main = mainWidget $ el "div" $ do
  hdDemo
  calculator
calculator :: MonadWidget t m => m ()
calculator = el "div" $ do
  el "h2" $ text $ "calculator"
  nx <- numberInput
  text " + "
  ny <- numberInput
  text " = " 
  result <- combineDyn (\x y -> (+) <$> x <*> y) nx ny
  resultString <- mapDyn show result
  dynText resultString

numberInput :: MonadWidget t m => m (Dynamic t (Maybe Double))
numberInput = do
    n <- textInput $ def & textInputConfig_inputType .~ "number"
                         & textInputConfig_initialValue .~ "0"
    mapDyn readMay $ _textInput_value n -- type inference is done at the top level



hdDemo :: MonadWidget t m => m ()
hdDemo = el "div" $ do
  el "h2" $ text $ "holdDyn demonstration"
  el "p" $ text $ "holdDyn :: MonadHold t m => a -> Event t a -> m (Dynamic t a)"
  el "p" $ text $ "holdDyn takes an Event and returns a Dynamic, or a \"latch\" of that event."
  t <- textInput def
  text "Last key pressed: "
  let keypressEvent = fmap show $ _textInput_keypress t
  keypressDyn <- holdDyn "None" keypressEvent
  dynText keypressDyn
