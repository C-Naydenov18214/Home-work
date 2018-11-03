     module Geometry
      where     
         data Shape = Circle Double | Square Double  | Nosquare Double Double|Konus Double Double| Shar Double deriving (Show)
 
         area::Shape  ->Double
         area (Circle  r) = pi*r*r
         area (Square a) = a*a
         area (Nosquare a b) = a*b

         per::Shape->Double
         per (Circle r) = pi*r*2
         per (Square a) = a*4
         per (Nosquare a b) = 2*a + 2*b

         v::Shape->Double
         v (Shar r) = 4/3*r*r*r
         v (Konus r h) = 1/3*pi*r*r*h
 
         sPOV::Shape->Double
         sPOV (Shar r) = 4*pi*r*r
         sPOV (Konus r h) = pi*r*r + pi*r*sqrt(r*r + h*h)




