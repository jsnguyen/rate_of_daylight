import Data.Time.Clock.POSIX
import Data.Fixed
import Debug.Trace

toRadians = pi/180
toDegrees = 180/pi
toHours = 24/360

-- this is a horrible function but it works!!!
calc_julian_date :: Double -> Double -> Double -> Double
calc_julian_date y m d = 367 * y - fromIntegral (floor (7 * (y + fromIntegral (floor ( (m + 9) / 12 )) ) / 4 )) - fromIntegral (floor ( 3 * ( fromIntegral (floor ( ( y + (m - 9) / 7 ) / 100 ) + 1) / 4))) + fromIntegral (floor (275 * m / 9 )) + d + 1721028.5

calc_n_centuries :: Double -> Double -> Double
calc_n_centuries jd ut = (jd + ut/24 - 2451545.0) / 36525.0

calc_mean_longitude :: Double -> Double
calc_mean_longitude t = (280.460 + 36000.770 * t) `mod'` 360

calc_mean_anomaly :: Double -> Double
calc_mean_anomaly t = (357.528 + 35999.050 * t) `mod'` 360

calc_ecliptic_longitude :: Double -> Double -> Double
calc_ecliptic_longitude l g = l + 1.915 * sin (g*toRadians) + 0.020 * sin (2*g*toRadians)

calc_obliquity_of_ecliptic :: Double -> Double
calc_obliquity_of_ecliptic t = 23.4393 - 0.01300 * t

calc_equation_of_time :: Double -> Double -> Double
calc_equation_of_time g lambda = -1.915 * sin (g*toRadians) - 0.020 * sin (2*g*toRadians) + 2.466 * sin (2*lambda*toRadians) - 0.053 * sin (4*lambda*toRadians)

calc_greenwich_hour_angle :: Double -> Double -> Double
calc_greenwich_hour_angle ut e = ut - 12 + e*toHours

calc_declination :: Double -> Double -> Double
calc_declination epsilon lambda = toDegrees * asin (sin (epsilon*toRadians) * sin (lambda*toRadians))

calc_semidiameter :: Double -> Double
calc_semidiameter g = 0.267 / (1 - 0.017 * cos (g*toRadians))

calcSunrise :: Double -> Double -> Double -> Double -> Double
calcSunrise ut0 gha longitude tha = ut0 - (gha + toHours*longitude + tha)

calcSunset :: Double -> Double -> Double -> Double -> Double
calcSunset ut0 gha longitude tha = ut0 - (gha + toHours*longitude - tha)

calcUT0HourAngle :: Double -> Double -> Double -> Double
calcUT0HourAngle h latitude dec
    | tha' > 1  = 0
    | tha' < -1 = 12
    | otherwise = toHours * toDegrees * acos tha'
    where
        tha' = (sin (h*toRadians) - sin (latitude*toRadians) * sin (dec*toRadians)) / (cos (latitude*toRadians) * cos (dec*toRadians))

addUntil :: Double -> Double -> Double -> Double
addUntil var ltVal untilVal = 
    if var < ltVal
        then addUntil (var + untilVal) ltVal untilVal
        else var

subUntil :: Double -> Double -> Double -> Double
subUntil var gtVal untilVal = 
    if var > gtVal
        then subUntil (var - untilVal) gtVal untilVal
        else var

iterativeSolverSunrise :: Double -> Double -> Double -> Double -> Double
iterativeSolverSunrise ut0 h latitude longitude = do
    let tup = calcSolarGHADec ut0
    let gha = fst tup
    let dec = snd tup
    let tha = calcUT0HourAngle h latitude dec
    let ut = calcSunrise ut0 gha longitude tha
    let utCorr | ut < 0   = addUntil ut 0 24
               | ut > 24  = subUntil ut 24 24
               | otherwise = ut
    let delta = ut0-utCorr
    if abs (delta) < 1e-6
        then utCorr
        else iterativeSolverSunrise utCorr h latitude longitude

iterativeSolverSunset :: Double -> Double -> Double -> Double -> Double
iterativeSolverSunset  ut0 h latitude longitude = do
    let tup = calcSolarGHADec ut0
    let gha = fst tup
    let dec = snd tup
    let tha = calcUT0HourAngle h latitude dec
    let ut = calcSunset ut0 gha longitude tha
    let utCorr | ut < 0   = addUntil ut 0 24
               | ut > 24  = subUntil ut 24 24
               | otherwise = ut
    let delta = ut0-utCorr
    if abs (delta) < 1e-6
        then utCorr
        else iterativeSolverSunset utCorr h latitude longitude

calcTrueAltitude :: Double -> Double
calcTrueAltitude h0 = -50/60 - 0.0353 * sqrt h0

calcSolarGHADec :: Double -> (Double, Double)
calcSolarGHADec ut = do
    let jd = calc_julian_date 2021 7 1
    let t = calc_n_centuries jd ut
    let l = calc_mean_longitude t
    let g = calc_mean_anomaly t
    let lambda = calc_ecliptic_longitude l g
    let epsilon = calc_obliquity_of_ecliptic t

    let e = calc_equation_of_time g lambda
    let gha = calc_greenwich_hour_angle ut e
    let dec = calc_declination epsilon lambda
    let sd = calc_semidiameter g

    (gha, dec)

main = do

    let h = calcTrueAltitude 0
    let latitude = 51.48257659
    let longitude = 0

    let ut0 = 12
    putStrLn ("Running iterator...")
    let sunriseTime = iterativeSolverSunrise ut0 h latitude longitude
    let sunsetTime = iterativeSolverSunset ut0 h latitude longitude

    print ("Sunrise",sunriseTime)
    print ("Sunset",sunsetTime)
    putStrLn ("Done!")
