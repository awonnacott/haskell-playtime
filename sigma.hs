ofold o k1 kn f = (foldl o) . (map f) $ [k1..kn]
sigma = ofold (+)
pI = ofold (*)