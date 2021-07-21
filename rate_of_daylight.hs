-- Most everythign is in units of degrees and converted to radians when needed
-- Hour angles are in units of hours
-- Most equations taken from "Explanatory Supplement to the Astronomical Almanac, 3rd Ed.", pg. 511-513

import Data.Fixed
import Data.List

degToRad = pi/180
degToRad = 180/pi
degToHr = 24/360

-- Gets the Julian day, not taking into account the specific time of day
-- This is a horrible looking function but it works!!!
calcJulianDay :: Double -> Double -> Double -> Double
calcJulianDay y m d = 367 * y - fromIntegral (floor (7 * (y + fromIntegral (floor ( (m + 9) / 12 )) ) / 4 )) - fromIntegral (floor ( 3 * ( fromIntegral (floor ( ( y + (m - 9) / 7 ) / 100 ) + 1) / 4))) + fromIntegral (floor (275 * m / 9 )) + d + 1721028.5

-- Calculate the number of decimal centuries from the Julian date and UT
calcDecCenturies :: Double -> Double -> Double
calcDecCenturies jd ut = (jd + ut/24 - 2451545.0) / 36525.0


-- #################################
-- APPROXIMATE EPHEMERIS FOR THE SUN
-- #################################


calcMeanLongitude :: Double -> Double
calcMeanLongitude t = (280.460 + 36000.770 * t) `mod'` 360

calcMeanAnomaly :: Double -> Double
calcMeanAnomaly t = (357.528 + 35999.050 * t) `mod'` 360

calcEclipticLongitude :: Double -> Double -> Double
calcEclipticLongitude l g = l + 1.915 * sin (g*degToRad) + 0.020 * sin (2*g*degToRad)

calcObliquityOfEcliptic :: Double -> Double
calcObliquityOfEcliptic t = 23.4393 - 0.01300 * t

calcEquationOfTime :: Double -> Double -> Double
calcEquationOfTime g lambda = -1.915 * sin (g*degToRad) - 0.020 * sin (2*g*degToRad) + 2.466 * sin (2*lambda*degToRad) - 0.053 * sin (4*lambda*degToRad)

calcGreenwichHourAngle :: Double -> Double -> Double
calcGreenwichHourAngle ut e = ut - 12 + e*degToHr

calcDeclination :: Double -> Double -> Double
calcDeclination epsilon lambda = degToRad * asin (sin (epsilon*degToRad) * sin (lambda*degToRad))

-- #################################
-- END #############################
-- #################################


-- Solve this equation iteratively to find the sunrise time
calcSunrise :: Double -> Double -> Double -> Double -> Double
calcSunrise ut0 gha lon tha = ut0 - (gha + degToHr*lon + tha)

-- Solve this equation iteratively to find the sunset time
calcSunset :: Double -> Double -> Double -> Double -> Double
calcSunset ut0 gha lon tha = ut0 - (gha + degToHr*lon - tha)

-- Calculte the hour angle at UT = 0
calcUT0HourAngle :: Double -> Double -> Double -> Double
calcUT0HourAngle h lat dec
    | tha' > 1  = 0
    | tha' < -1 = 12
    | otherwise = degToHr * degToRad * acos tha'
    where
        tha' = (sin (h*degToRad) - sin (lat*degToRad) * sin (dec*degToRad)) / (cos (lat*degToRad) * cos (dec*degToRad))

-- Add whole units until a set value
addUntil :: Double -> Double -> Double -> Double
addUntil var ltVal unit = 
    if var < ltVal
        then addUntil (var + unit) ltVal unit
        else var

-- Subtract whole units until a set value
subUntil :: Double -> Double -> Double -> Double
subUntil var gtVal unit = 
    if var > gtVal
        then subUntil (var - unit) gtVal unit
        else var

-- Recursive solver to find the sunrise time in hours, UT
-- If using to find the actual sunset times, convert UTC to your local timezone
iterativeSolverSunrise :: Double -> Double -> Double -> Double -> Double -> Double
iterativeSolverSunrise jd h ut0 lat lon = do
    let tup = calcSolarGHADec jd ut0
    let gha = fst tup
    let dec = snd tup
    let tha = calcUT0HourAngle h lat dec
    let ut = calcSunrise ut0 gha lon tha
    let utCorr | ut < 0   = addUntil ut 0 24
               | ut > 24  = subUntil ut 24 24
               | otherwise = ut
    let delta = ut0-utCorr
    if abs (delta) < 1e-6
        then utCorr
        else iterativeSolverSunrise jd h utCorr lat lon

-- Recursive solver to find the sunset time in hours, UT
-- If using to find the actual sunset times, convert UTC to your local timezone
iterativeSolverSunset :: Double -> Double -> Double -> Double -> Double -> Double
iterativeSolverSunset jd h ut0 lat lon = do
    let tup = calcSolarGHADec jd ut0
    let gha = fst tup
    let dec = snd tup
    let tha = calcUT0HourAngle h lat dec
    let ut = calcSunset ut0 gha lon tha
    let utCorr | ut < 0   = addUntil ut 0 24
               | ut > 24  = subUntil ut 24 24
               | otherwise = ut
    let delta = ut0-utCorr
    if abs (delta) < 1e-6
        then utCorr
        else iterativeSolverSunset jd h utCorr lat lon

-- Calculate the true altitude for the Sun
calcTrueAltitude :: Double -> Double
calcTrueAltitude h0 = -50/60 - 0.0353 * sqrt h0

-- Calculate the solar Greenwich Hour Angle
calcSolarGHADec :: Double -> Double -> (Double, Double)
calcSolarGHADec jd ut = do
    let t = calcDecCenturies jd ut
    let l = calcMeanLongitude t
    let g = calcMeanAnomaly t
    let lambda = calcEclipticLongitude l g
    let epsilon = calcObliquityOfEcliptic t

    let e = calcEquationOfTime g lambda
    let gha = calcGreenwichHourAngle ut e
    let dec = calcDeclination epsilon lambda

    (gha, dec)

-- Calculates the delta between sunrise and sunset times
calcDeltaTime :: Double -> Double -> Double -> Double -> Double
calcDeltaTime h lat lon jd = do
    let ut0 = 12 -- doesn't matter what this number is technically, but 12 is a good initial guess
    let sunriseTime = iterativeSolverSunrise jd h ut0 lat lon
    let sunsetTime = iterativeSolverSunset jd h ut0 lat lon

    sunsetTime-sunriseTime

-- Calculates the derivative of a discrete array
-- Assumes central difference
calcDerivative :: Int -> (Double, Double) -> Double
calcDerivative width doublet = (snd doublet - fst doublet) / (fromIntegral width)

-- Helper function for calculating discrete derivatives
extractDoublet :: [Double] -> Int -> (Double, Double)
extractDoublet arr i = (arr!!i, arr!!(i+1))

main = do
    let stepSize = 1  -- Units of days
    let jd = calcJulianDay 2021 1 1
    let dates = [jd,jd+(fromIntegral stepSize)..jd+365]

    let h = calcTrueAltitude 0
    let lat = 40
    let lon = 0

    putStrLn "Calculating delta sunrise sunset times..."
    let deltaTimes = map (calcDeltaTime h lat lon) dates

    putStrLn "Calculating derivatives..."

    -- minus 1 for zero index and minus 1 for derivative endpoint
    let indicies = [0,stepSize..((length deltaTimes) - 1 - 1)]
    let res = map (extractDoublet deltaTimes) indicies
    let derivatives = map (*60) (map (calcDerivative stepSize ) res)

    writeFile "delta_times.txt" . intercalate "\n" . map show $ deltaTimes
    writeFile "derivatives.txt" . intercalate "\n" . map show $ derivatives

    putStrLn "Wrote delta_times.txt and derivatives.txt!"
    putStrLn "Done!"

