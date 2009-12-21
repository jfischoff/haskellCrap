import Text.HTML.Download
import Text.HTML.TagSoup
import Monad
main = do
    tags <- liftM parseTags $ openURL "http://haskell.org/haskellwiki/Haskell"
    let count = head $ sections (~== "<div class=printfooter>") tags
    putStrLn $ "haskell.org has been hit " ++ show count ++ " times"