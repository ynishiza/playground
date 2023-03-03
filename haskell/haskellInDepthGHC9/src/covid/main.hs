import App
import Control.Monad.Trans.Resource
import TextShow

csvPath :: FilePath
csvPath = "./src/data/owid-covid-data.csv.gz"

main :: IO ()
main = runResourceT (parseZipppedCsv csvPath) >>= printT
