import Data.Time.Clock.POSIX

calc_n_centuries :: Double -> Double -> Double
calc_n_centuries jd ut = (jd + ut/24 - 2451545.0) / 36525.0

calc_mean_longitude :: Double -> Double
calc_mean_longitude t = 280.460 + 36000.770 * t

calc_mean_anomaly :: Double -> Double
calc_mean_anomaly t = 357.528 + 35999.050 * t

calc_ecliptic_longitude :: Double -> Double -> Double
calc_ecliptic_longitude l g = l + 1.915 * sin g + 0.020 * sin (2*g)

calc_obliquity_of_ecliptic :: Double -> Double
calc_obliquity_of_ecliptic t = 23.4393 - 0.01300 * t

calc_equation_of_time :: Double -> Double -> Double
calc_equation_of_time g lambda = -1.915 * sin g - 0.020 * sin (2*g) + 2.466 * sin (2*lambda) - 0.053 * sin (4*lambda)

calc_greenwich_hour_angle :: Double -> Double -> Double
calc_greenwich_hour_angle ut e = 15 * ut - 180 + e

calc_declination :: Double -> Double -> Double
calc_declination epsilon lambda = asin (sin epsilon * sin lambda)

calc_semidiameter :: Double -> Double
calc_semidiameter g = 0.267 / (1 - 0.017 * cos g)

main = do
    print (calc_n_centuries 10.0 10.0)
    let test  = ((round . (* 1000)) <$> getPOSIXTime)
    print (test)
