# Polyomino Enumerator Written in Racket

Main functions:

polyomino enumeration:
- (gen-fixed-polyo-of n)
- (gen-fixed-polyo-upto n)
- (gen-free-polyo-of n)
- (gen-free-polyo-upto n)

visualize shapes:
- (shape->pict a-shape)
- (map shape->pict list-of-shapes)
- (deepmap shape->pict nested-list-of-shapes)