holescore :: Int -> Int -> String
holescore strokes par
  | score < 0 = show (abs(score)) ++ " under par"
  | score == 0 = "level par"
  | otherwise = show(score) ++ " over par"
 where score = strokes-par


