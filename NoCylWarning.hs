{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}

data Goal deriving AnyZone
data CourseLine deriving AnyZone

class AnyZone a

newtype Radius a = Radius a
newtype LatLng a = LatLng (a, a)

data Zone k a where
    Point :: LatLng a -> Zone CourseLine a
    Line :: Radius a -> LatLng a -> Zone Goal a

separated :: AnyZone k => Zone k a -> Zone k a -> Bool
separated (Point _) (Point _) = undefined

-- NoCylWarning.hs:25:1: warning: [-Woverlapping-patterns]
--     Pattern match has inaccessible right hand side
--     In an equation for ‘separated’: separated x y@(Point _) = ...
--    |
-- 25 | separated x y@(Point _) = separated y x
--    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
separated x y@(Point _) = separated y x

-- NoCylWarning.hs:33:1: warning: [-Woverlapping-patterns]
--     Pattern match is redundant
--     In an equation for ‘separated’: separated (Point _) y = ...
--    |
-- 33 | separated (Point _) y = undefined
--    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
separated (Point _) y = undefined
separated x y = undefined
