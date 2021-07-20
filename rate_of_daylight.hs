import Data.Fixed
import Data.List
import Debug.Trace

toRadians = pi/180
toDegrees = 180/pi
toHours = 24/360
hoursToDegrees = 360/24

fst' (a, _, _) = a
snd' (_, a, _) = a
thd' (_, _, a) = a

-- this is a horrible function but it works!!!
calcJulianDate :: Double -> Double -> Double -> Double
calcJulianDate y m d = 367 * y - fromIntegral (floor (7 * (y + fromIntegral (floor ( (m + 9) / 12 )) ) / 4 )) - fromIntegral (floor ( 3 * ( fromIntegral (floor ( ( y + (m - 9) / 7 ) / 100 ) + 1) / 4))) + fromIntegral (floor (275 * m / 9 )) + d + 1721028.5

calcNCenturies :: Double -> Double -> Double
calcNCenturies jd ut = (jd + ut/24 - 2451545.0) / 36525.0

calcMeanLongitude :: Double -> Double
calcMeanLongitude t = (280.460 + 36000.770 * t) `mod'` 360

calcMeanAnomaly :: Double -> Double
calcMeanAnomaly t = (357.528 + 35999.050 * t) `mod'` 360

calcEclipticLongitude :: Double -> Double -> Double
calcEclipticLongitude l g = l + 1.915 * sin (g*toRadians) + 0.020 * sin (2*g*toRadians)

calcObliquityOfEcliptic :: Double -> Double
calcObliquityOfEcliptic t = 23.4393 - 0.01300 * t

calcEquationOfTime :: Double -> Double -> Double
calcEquationOfTime g lambda = -1.915 * sin (g*toRadians) - 0.020 * sin (2*g*toRadians) + 2.466 * sin (2*lambda*toRadians) - 0.053 * sin (4*lambda*toRadians)

calcGreenwichHourAngle :: Double -> Double -> Double
calcGreenwichHourAngle ut e = ut - 12 + e*toHours

calcDeclination :: Double -> Double -> Double
calcDeclination epsilon lambda = toDegrees * asin (sin (epsilon*toRadians) * sin (lambda*toRadians))

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

iterativeSolverSunrise :: Double -> Double -> Double -> Double -> Double -> Double
iterativeSolverSunrise jd h ut0 latitude longitude = do
    let tup = calcSolarGHADec jd ut0
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
        else iterativeSolverSunrise jd h utCorr latitude longitude

iterativeSolverSunset :: Double -> Double -> Double -> Double -> Double -> Double
iterativeSolverSunset jd h ut0 latitude longitude = do
    let tup = calcSolarGHADec jd ut0
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
        else iterativeSolverSunset jd h utCorr latitude longitude

calcTrueAltitude :: Double -> Double
calcTrueAltitude h0 = -50/60 - 0.0353 * sqrt h0

calcSolarGHADec :: Double -> Double -> (Double, Double)
calcSolarGHADec jd ut = do
    let t = calcNCenturies jd ut
    let l = calcMeanLongitude t
    let g = calcMeanAnomaly t
    let lambda = calcEclipticLongitude l g
    let epsilon = calcObliquityOfEcliptic t

    let e = calcEquationOfTime g lambda
    let gha = calcGreenwichHourAngle ut e
    let dec = calcDeclination epsilon lambda

    (gha, dec)

calcDeltaTime :: Double -> Double -> Double -> Double -> Double
calcDeltaTime h latitude longitude jd = do
    let ut0 = 12
    let sunriseTime = iterativeSolverSunrise jd h ut0 latitude longitude
    let sunsetTime = iterativeSolverSunset jd h ut0 latitude longitude
    sunsetTime-sunriseTime

calcDerivative :: Int -> (Double, Double) -> Double
calcDerivative width doublet = (snd doublet - fst doublet) / (fromIntegral width)

extractDoublet :: [Double] -> Int -> (Double, Double)
extractDoublet arr i = (arr!!i, arr!!(i+1))

main = do
    let stepSize = 1 
    let jd = calcJulianDate 2021 1 1
    let dates = [jd,jd+(fromIntegral stepSize)..jd+365]

    let h = calcTrueAltitude 0
    let latitude = 40
    let longitude = 0

    putStrLn "Calculating delta sunrise sunset times..."
    let deltaTimes = map (calcDeltaTime h latitude longitude) dates

    putStrLn "Calculating derivatives..."
    -- minus 1 for zero index and minus 1 for derivative endpoint
    let indicies = [0,stepSize..((length deltaTimes) - 1 - 1)]
    let res = map (extractDoublet deltaTimes) indicies
    let derivatives = map (*60) (map (calcDerivative stepSize ) res)

    writeFile "delta_times.txt" . intercalate "\n" . map show $ deltaTimes
    writeFile "derivatives.txt" . intercalate "\n" . map show $ derivatives

    putStrLn "Wrote delta_times.txt and derivatives.txt!"
    putStrLn "Done!"

