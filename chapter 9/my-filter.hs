module MyFilter where

articles :: [String]
articles = ["the", "a", "an", "The", "A", "An", "THE", "AN", "aN", "tHE", "tHe", "thE", "ThE"]

myFilter :: String -> [String]
myFilter s = [str | str <- words s, not $ elem str articles]
