module Data.Time.Hora.Type.YmdHms where


data YmdHms = YmdHms {
    year::Int,
    month::Int,
    day::Int,
    hour::Int,
    minute::Int,
    second::Int
    } deriving (Show,Eq)


instance Ord YmdHms where
    (<=) a0 b0 = not $
        year a0 > (year b0)
        || month a0 > (month b0)
        || day a0 > (day b0)
        || hour a0 > (hour b0)
        || minute a0 > (minute b0)
        || second a0 > (second b0)
             