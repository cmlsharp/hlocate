import Build.WalkDir
import Build.DTree
import Data.Serialize
import System.FilePath
import System.Directory
import qualified Data.ByteString as B

main = do
    x <- encode <$> walkDir "/"
    file <- (</> "hlocate.db") <$> getHomeDirectory
    B.writeFile file x


