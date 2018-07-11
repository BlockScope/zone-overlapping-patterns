{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}

newtype Radius a = Radius a
newtype LatLng a = LatLng (a, a)

data Zone a where
    Point :: Eq a => LatLng a -> Zone a
    Line :: Eq a => Radius a -> LatLng a -> Zone a

separated :: Zone a -> Zone a -> Bool
separated (Point _) (Point _) = undefined
separated x y@(Point _) = separated y x
separated (Point _) y = undefined
separated x y = undefined
