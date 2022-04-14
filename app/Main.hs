import           Control.Monad             (when)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.State (StateT, evalStateT, get, modify',
                                            put)
import           Data.Foldable             (traverse_)
import           Data.Function             (fix)
import           Data.Version              (showVersion)
import           Data.Word                 (Word8)
import qualified Paths_bf_hs               as Auto
import           System.Environment        (getArgs)
import           System.Exit               (die)
import           System.IO                 (BufferMode (NoBuffering),
                                            hSetBuffering, stdout)
import           Text.Parsec               (between, eof, many, noneOf, parse,
                                            skipMany, string, (<|>))
import           Text.Parsec.String        (Parser)

---- Types ----
data Command = Inc | Dec | Next | Prev | Put | Get | Loop [Command]

newtype Program = Program [Command]

data Stream a = a :> Stream a
data Tape a = Tape (Stream a) a (Stream a)

---- Parser ----
progP :: Parser Program
progP = Program <$> (ignoreChars *> many commandP <* eof)
  where
    commandP =
      Inc <$ tok "+"
      <|> Dec <$ tok "-"
      <|> Next <$ tok ">"
      <|> Prev <$ tok "<"
      <|> Put <$ tok "."
      <|> Get <$ tok ","
      <|> loopP

    ignoreChars = skipMany (noneOf "+-><.,[]")
    tok s = string s <* ignoreChars
    loopP = Loop <$> between (tok "[") (tok "]") (many commandP)

---- Tape Manipulation ----
streamOf :: a -> Stream a
streamOf a = fix (a :>)

zeroTape :: Num a => Tape a
zeroTape = Tape (streamOf 0) 0 (streamOf 0)

incTape :: Num a => Tape a -> Tape a
incTape (Tape l focus r) = focus `seq` Tape l (focus + 1) r

decTape :: Num a => Tape a -> Tape a
decTape (Tape l focus r) = focus `seq` Tape l (focus - 1) r

moveL :: Tape a -> Tape a
moveL (Tape (l :> ls) focus rs) = Tape ls l (focus :> rs)

moveR :: Tape a -> Tape a
moveR (Tape ls focus (r :> rs)) = Tape (focus :> ls) r rs

---- Interpeter ----
interpretC :: Integral a => Command -> StateT (Tape a) IO ()
interpretC Inc = modify' incTape
interpretC Dec = modify' decTape
interpretC Next = modify' moveR
interpretC Prev = modify' moveL
interpretC Put = do
  Tape _ focus _ <- get
  lift $ putStr [toEnum $ fromIntegral focus]
interpretC Get = do
  c <- lift getChar
  let code = fromEnum c
  when (code >= 0 && code < 256) $ do
    Tape l _ r <- get
    put $ Tape l (fromIntegral code) r
interpretC loop@(Loop cs) = do
  Tape _ focus _ <- get
  when (focus /= 0) $ do
    traverse_ interpretC cs
    interpretC loop

interpret :: Program -> IO ()
interpret (Program cs) = do
  hSetBuffering stdout NoBuffering
  flip evalStateT (zeroTape :: Tape Word8) $ traverse_ interpretC cs

runBf :: String -> String -> IO ()
runBf name s = either (die . show) interpret $ parse progP name s

runBfFile :: FilePath -> IO ()
runBfFile f = runBf f =<< readFile f

main :: IO ()
main = do
  files <- getArgs
  case files of
    [] -> putStrLn $ "The Glorious Haskell Brainfuck Interpreter, version " <> showVersion Auto.version
    _ -> traverse_ runBfFile files
