module Paths_dianoga where

getDataFileName :: FilePath -> IO FilePath
getDataFileName path = return ("./" ++ path)
