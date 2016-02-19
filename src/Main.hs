import Build.WalkDir
import Build.DTree
import Data.Serialize
import System.FilePath
import System.Directory
import Text.Regex.Posix
import qualified Data.ByteString as B

main = do
    x <- encode <$> walkDirPrune (=~ "/.snapshots/.*") "/home"
    file <- (</> "hlocate.db") <$> getHomeDirectory
    B.writeFile file x


