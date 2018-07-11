{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}

data Goal deriving AnyZone
data CourseLine deriving AnyZone

class AnyZone a

newtype Radius a = Radius a
newtype LatLng a = LatLng (a, a)

data Zone k a where
    Point :: Eq a => LatLng a -> Zone CourseLine a
    Line :: Eq a => Radius a -> LatLng a -> Zone Goal a

separated :: AnyZone k => Zone k a -> Zone k a -> Bool
separated (Point _) (Point _) = undefined
separated x y@(Point _) = separated y x

-- NoCylWarning.hs:26:1: warning: [-Woverlapping-patterns]
--     Pattern match is redundant
--     In an equation for ‘separated’: separated (Point _) y = ...
--    |
-- 26 | separated (Point _) y = undefined
--    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
separated (Point _) y = undefined
separated x y = undefined
