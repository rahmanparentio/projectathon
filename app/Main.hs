{-#LANGUAGE ScopedTypeVariables#-}

module Main where

import Text.CSV ( parseCSVFromFile )
import Control.Monad.RWS
import Data.List
import System.Environment
import Data.Csv

import Helpers

main :: IO ()
main = do
    args <- getArgs

    putStrLn "\n\n================================================="
    putStrLn "       Predicting Carbon Emmission Price"

    putStrLn "=================================================\n"
    putStrLn "ketik 0 dua kali untuk keluar"

    loaded_file <- parseCSVFromFile "TES.csv"

    let
        noEmptyRows = either (const []) (filter (\row -> 2 <= length row))
        candles' = noEmptyRows loaded_file
        candles = map (map (\s -> read(s)::Float)) candles'
        startState = [0::Float, 0::Float, 0::Float]
    (bestRun, allRuns) <- execRWST (execRun 1) candles startState
    putStrLn "\n----- Testing sudah selesai ----------------------------------------\n"
    putStrLn "\n----- Hasil run anda tersimpan dengan nama file hasil coba.txt ----------------------------------------\n"
    putStr "[belum kelar]  : "
    print bestRun
    putStrLn "[tulis semua run] : "
    print allRuns
    writeFile "cek carbon.txt" (show allRuns)
    putStrLn ">>>> hasil cek anda tersimpan dengan nama : cek carbon.txt"
    
execRun :: Int -> RWST [[Float]] [[Float]] [Float] IO ()
execRun i =  -- cek ke stackoverflow
    do
        history <- ask        

        lift $ putStrLn $ "\nBulan # " ++ show (i)
        lift $ putStrLn "Persentase kenaikan (1-100%): "
        sup  <- lift getLine
        let up = read sup
        lift $ putStrLn "Persentase penurunan (1-100%) [0 to exit]: "
        sdown <- lift getLine
        let down = read sdown

        let startPosition = [1000::Float, 0::Float, getBuka (head history), 0::Float, 0::Float, 0::Float]
            result = (last (scanl (kalukasi up down) startPosition history) !! 5 - 1000.0)/10.0
        lift $ putStr $ "Naik = " ++ show(up) ++ "%, Turun = " ++ show(down) ++ "%"
        lift $ putStrLn $ "Prediksi perubahan harga carbon pada bulan adalah  = " ++ show (result) ++ "%"

        let newRunScore = [up, down, result]
        bestRunSoFar <- get
        if (newRunScore !! 2) > (bestRunSoFar !! 2) then put newRunScore
        else put bestRunSoFar    
            
        if down > 0 then do
            tell $ [newRunScore] -- Helpers line 33
            (execRun  (i + 1))
        else return () 
