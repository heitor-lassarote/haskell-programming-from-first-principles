x = undefined
y = "blah"
main = do
    x `seq` print (snd (x, y))

