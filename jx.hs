import Control.Monad
import Data.Char
import Data.List

main = do
  json <- getContents
  putStr (parse json)

data StackItem = ObjKey String | ArrIdx Integer deriving Show

parse json = topLevel json

topLevel "" = "" 
topLevel (ch:rest)
  | ch == '{' = objWantKey [] rest
  | ch == '[' = arrWantValue [] 0 rest
  | otherwise = topLevel rest

objWantKey stack (ch:rest)
  | isSpace ch = objWantKey stack rest
  | ch == '}' = dropLevel stack rest
  | ch == '"' = objReadKey stack "" rest

objReadKey stack kacc (ch:rest)
  | ch == '\\' = objReadKeyEsc stack (ch:kacc) rest
  | ch == '"' = objWantColon stack (reverse kacc) rest
  | otherwise = objReadKey stack (ch:kacc) rest

objReadKeyEsc stack kacc (ch:rest) = objReadKey stack (ch:kacc) rest

objWantColon stack key (ch:rest)
  | isSpace ch = objWantColon stack key rest
  | ch == ':' = objWantValue stack key rest

objWantValue stack key (ch:rest)
  | isSpace ch = objWantValue stack key rest
  | ch == '{' = objWantKey (ObjKey key : stack) rest
  | ch == '[' = arrWantValue (ObjKey key : stack) 0 rest
  | ch == '"' = objReadStr stack key [ch] rest
  | otherwise = objReadBare stack key [ch] rest

objReadStr stack key vacc (ch:rest)
  | ch == '\\' = objReadStrEsc stack key (ch:vacc) rest
  | ch == '"' = result ++ objAfterVal stack rest
  | otherwise = objReadStr stack key (ch:vacc) rest
  where result = genLine stack key (reverse $ ch:vacc)

objReadStrEsc stack key vacc (ch:rest) = objReadStr stack key (ch:vacc) rest

objReadBare stack key vacc (ch:rest)
  | isSpace ch = result ++ objAfterVal stack rest
  | ch == '}' = result ++ dropLevel stack rest
  | ch == ',' = result ++ objWantKey stack rest
  | otherwise = objReadBare stack key (ch:vacc) rest
  where result = genLine stack key (reverse vacc)

objAfterVal stack (ch:rest)
  | isSpace ch = objAfterVal stack rest
  | ch == '}' = dropLevel stack rest
  | ch == ',' = objWantKey stack rest

arrWantValue stack idx (ch:rest)
  | isSpace ch = arrWantValue stack idx rest
  | ch == '{' = objWantKey (ArrIdx idx : stack) rest
  | ch == '[' = arrWantValue (ArrIdx idx : stack) 0 rest
  | ch == '"' = arrReadStr stack idx [ch] rest
  | otherwise = arrReadBare stack idx [ch] rest

arrReadStr stack idx vacc (ch:rest)
  | ch == '\\' = arrReadStrEsc stack idx (ch:vacc) rest
  | ch == '"' = result ++ arrAfterVal stack idx rest
  | otherwise = arrReadStr stack idx (ch:vacc) rest
  where result = genLine stack (show idx) (reverse $ ch:vacc)

arrReadStrEsc stack idx vacc (ch:rest) = arrReadStr stack idx (ch:vacc) rest

arrReadBare stack idx vacc (ch:rest)
  | isSpace ch = result ++ arrAfterVal stack idx rest
  | ch == ']' = result ++ dropLevel stack rest
  | ch == ',' = result ++ arrWantValue stack (idx+1) rest
  | otherwise = arrReadBare stack idx (ch:vacc) rest
  where result = genLine stack (show idx) (reverse vacc)

arrAfterVal stack idx (ch:rest)
  | isSpace ch = arrAfterVal stack idx rest
  | ch == ']' = dropLevel stack rest
  | ch == ',' = arrWantValue stack (idx+1) rest

-- Pop the top of the stack and enter the state machine at the right spot
dropLevel [] str = topLevel str
dropLevel (tos:stack) str =
  case tos of
    ObjKey k -> objAfterVal stack str
    ArrIdx i -> arrAfterVal stack i str

genLine stack key val = mkKey stack key ++ "  " ++ val ++ "\n"

mkKey (ObjKey k : stack) key = mkKey stack (k ++ "." ++ key)
mkKey (ArrIdx i : stack) key = mkKey stack (show i ++ "." ++ key)
mkKey _ key = key
