module Helpers
    
    ( getBuka
    , getTinggi
    , getRendah
    , getTutup
    , newStreak
    , kalukasi
    ) where
        
getBuka :: [Float] -> Float
getBuka candle = head candle

getTinggi :: [Float] -> Float
getTinggi candle = candle !! 1

getRendah :: [Float] -> Float
getRendah candle = candle !! 2

getTutup :: [Float] -> Float
getTutup candle = candle !! 3

newStreak :: Float -> Float -> Float -> Float
newStreak streak open close
    | (streak >= 0) && ((close - open) > 0) = streak + 1.0
    | (streak >= 0) && ((close - open) < 0) = (-1.0)
    | (streak < 0) && ((close - open) > 0)  = 1.0
    | (streak < 0) && ((close - open) < 0)  = streak - 1.0
    | otherwise                             = streak

kalukasi :: Float -> Float -> [Float] -> [Float] -> [Float]
kalukasi up down position candle
    | (money > 0) && (low < priceBase * (1 - down / 100))
    = let price = priceBase * (1 - down / 100)
      in  [0.0, money / price, price, 1.0, streak', (money / price) * close]
    | (quantity > 0) && (high > priceBase * (1 + up / 100)) -- cek stackoverflow


    = let price = priceBase * (1 + up / 100)
      in  [quantity * price, 0.0, price, -1.0, close, streak', quantity * close]
    | otherwise


    = let price = if money > 0 then close else priceBase
      in  [money, quantity, price, 0.0, streak', money + quantity * close]




  where
    money     = head position
    quantity  = position !! 1
    priceBase = position !! 2
    action    = position !! 3
    streak    = position !! 4
    open      = getBuka candle
    close     = getTutup candle
    low       = getRendah candle
    high      = getTinggi candle
    streak'   = newStreak streak open close


