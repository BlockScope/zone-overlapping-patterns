{-# LANGUAGE DeriveAnyClass #-}

newtype Radius a = Radius a
newtype LatLng a = LatLng (a, a)

data Zone a
    = Point (LatLng a)
    | Line (Radius a) (LatLng a)

separated :: Zone a -> Zone a -> Bool
separated (Point _) (Point _) = undefined
separated x y@(Point _) = separated y x
separated (Point _) y = undefined
separated x y = undefined
