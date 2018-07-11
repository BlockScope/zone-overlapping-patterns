{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}

newtype Radius a = Radius a
newtype LatLng a = LatLng (a, a)

data Zone a where
    Point :: LatLng a -> Zone a
    Cylinder :: Radius a -> LatLng a -> Zone a
    Line :: Radius a -> LatLng a -> Zone a

separated :: Zone a -> Zone a -> Bool
separated (Point _) (Point _) = undefined
separated x y@(Point _) = separated y x
separated (Point _) (Cylinder _ _) = undefined
separated (Point _) y = undefined
separated (Cylinder _ _) (Cylinder _ _) = undefined
separated x y = undefined
