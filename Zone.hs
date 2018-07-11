{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}

newtype Radius a = Radius a
newtype LatLng a = LatLng (a, a)

data Zone a where
    Point :: Eq a => LatLng a -> Zone a
    Cylinder :: Eq a => Radius a -> LatLng a -> Zone a
    Line :: Eq a => Radius a -> LatLng a -> Zone a

separated :: Zone a -> Zone a -> Bool
separated (Point _) (Point _) = undefined
separated x y@(Point _) = separated y x
separated (Point _) (Cylinder _ _) = undefined

-- warning: [-Woverlapping-patterns]
--     Pattern match is redundant
--     In an equation for ‘separated’: separated x@(Point _) y = ...
--    |
-- 71 | separated x@(Point _) y =
--    | ^^^^^^^^^^^^^^^^^^^^^^^^^...
separated (Point _) y = undefined
separated (Cylinder _ _) (Cylinder _ _) = undefined
separated x y = undefined
