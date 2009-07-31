module HaskShapes where

data Shape a = Box ((a,a), a)
             | Translate ((a,a), a) (Shape a)
             | Colour ((a,a), a) (Shape a)

box = Box
translate = Translate
colour = Colour