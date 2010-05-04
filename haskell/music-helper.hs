-- I use this to manage my tunes.
module Main where

import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.FilePath

main = do argv <- getArgs
          case argv of
            [] -> fail "Not enough arguments."
            (_:[]) -> fail "Not enough arguments."
            (playlist:albumsDir:_) -> do albums <- getAlbumsFrom albumsDir
                                         mapM_ (flip catch print . addOrRemove playlist . (albumsDir </>)) albums

addOrRemove playlistDir albumDir = do putStrLn ""
                                      contentsAlbum <- getMusicFrom albumDir
                                      contentsPlaylist <- getMusicFrom playlistDir
                                      putStrLn $ "In " ++ albumDir ++ ":"
                                      mapM_ putStrLn contentsAlbum
                                      case intersect contentsAlbum contentsPlaylist of
                                        [] -> do putStrLn "This album is not in your playlist."
                                                 putStrLn "Should I add this album to your playlist? (y/N)"
                                                 addToPlaylist contentsAlbum <?> return ()
                                        intersection -> if intersection == contentsAlbum then
                                                            do putStrLn "All songs in this album are in your playlist."
                                                               putStrLn "Should I remove this album from your playlist? (y/N)"
                                                               removeFromPlaylist contentsAlbum <?> return ()
                                                        else
                                                            do putStrLn "Songs from this album already in your playlist:"
                                                               mapM_ putStrLn intersection
                                                               putStrLn "Songs missing from your playlist:"
                                                               mapM_ putStrLn (contentsAlbum \\ intersection)
                                                               putStrLn "Should I add the rest of the songs? (y/N)"
                                                               addToPlaylist (contentsAlbum \\ intersection) <?> (do putStrLn "Should I remove the album completely? (y/N)"
                                                                                                                     removeFromPlaylist intersection <?> return ())
                                      where addToPlaylist = mapM_ (\x -> flip verboseCopy (playlistDir </> x) . (albumDir </>) $ x)
                                            removeFromPlaylist = mapM_ (verboseRemove . (playlistDir </>))

verboseCopy file1 file2 = do putStrLn $ "Copying " ++ file1 ++ " to " ++ file2
                             copyFile file1 file2

verboseRemove file = do putStrLn $ "Removing " ++ file
                        removeFile file

infixr 9 <?>
act1 <?> act2 = do response <- getLine
                   case response of
                     ('y':_) -> act1
                     _ -> act2

getAlbumsFrom dir = do contents <- liftM (filter (\x -> x /= "." && x /= "..")) $ getDirectoryContents dir
                       filterM (doesDirectoryExist . (dir </>)) contents

isOneOf x = or . map (x ==)

musicExtensions = [".ogg", ".OGG", ".mp3", ".MP3"]

getMusicFrom dir = liftM (sortBy compareByTrackNum . filter (\x -> takeExtension x `isOneOf` musicExtensions)) $ getDirectoryContents dir

compareByTrackNum x y = compare (trackNum x) (trackNum y)

trackNum x = case reads x of
               [] -> 0
               ((n, _):_) -> n
