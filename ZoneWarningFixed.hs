{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}

data Goal
    deriving (AnyZone, ZoneMaybeCylindrical)

data CourseLine
    deriving (AnyZone, ZoneMaybeCylindrical)

class AnyZone a
class ZoneMaybeCylindrical a

newtype Radius a = Radius a
newtype LatLng a = LatLng (a, a)

data Zone k a where
    Point :: LatLng a -> Zone CourseLine a
    Cylinder :: ZoneMaybeCylindrical k => Radius a -> LatLng a -> Zone k a
    Line :: Radius a -> LatLng a -> Zone Goal a

separated :: (AnyZone k1, AnyZone k2) => Zone k1 a -> Zone k2 a -> Bool
separated (Point _) (Point _) = undefined
separated x y@(Point _) = separated y x
separated (Point _) (Cylinder _ _) = undefined

-- ZoneWarning.hs:32:1: warning: [-Woverlapping-patterns]
--     Pattern match is redundant
--     In an equation for ‘separated’: separated (Point _) y = ...
--    |
-- 32 | separated (Point _) y = undefined
--    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^...
separated (Point _) y = undefined
separated (Cylinder _ _) (Cylinder _ _) = undefined
separated x y = undefined
