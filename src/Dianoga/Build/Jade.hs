module Dianoga.Build.Jade where
import Development.Shake

jade :: [FilePath] -> Action ()
jade = cmd ("node_modules/jade/bin/jade.js --path jade --out out" :: String)

