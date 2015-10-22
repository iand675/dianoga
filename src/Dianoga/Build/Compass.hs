module Dianoga.Build.Compass where
import Development.Shake

compass :: [FilePath] -> Action ()
compass = cmd ("bundle exec compass compile" :: String)

