import System.Directory
import System.FilePath
import Control.Monad
import System.Process
import Data.Char
import System.Environment
import qualified Control.Monad.Parallel as P

main :: IO ()
main = do
        args <- getArgs
        let dir = head args
        c <- listDirectory dir
        f <- filterM (\x -> return (map toLower (snd(splitExtension (dir ++ "/" ++ x))) == ".heic")) c
        _ <- P.mapM (\y -> let x  = dir ++ "/" ++ y 
                           in callProcess "convert" [x, (dropExtension x) ++ ".jpg"]) f
        return ()
