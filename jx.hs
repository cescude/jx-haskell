import Control.Monad
import Data.Char
import Data.List

main = do
  json <- getContents
  putStr $ concat (parse json)

data StackItem = ObjKey String | ArrIdx Integer deriving Show

parse json = topLevel json

topLevel "" = []
topLevel (ch:rest)
  | ch == '{' = objStart [] rest
  | ch == '[' = arrStart [] rest
  | otherwise = topLevel rest

-- { <>
objStart stack (ch:rest)
  | isSpace ch = objStart stack rest
  | ch == '}' = dropLevel stack rest
  | ch == '"' = objReadKey stack "" rest

-- Only difference w/ objStart is that reading '}' is invalid
-- { "key" : val, <>
objWantKey stack (ch:rest)
  | isSpace ch = objWantKey stack rest
  | ch == '"' = objReadKey stack "" rest
  -- Uncomment to support commas after values, eg. {"key":"val",}
  -- | ch == '}' = dropLevel stack rest

-- { "<>
-- { "key" : val, "<>
objReadKey stack kacc (ch:rest)
  | ch == '\\' = objReadKeyEsc stack (ch:kacc) rest
  | ch == '"' = objWantColon stack (reverse kacc) rest
  | otherwise = objReadKey stack (ch:kacc) rest

-- { "\<>
objReadKeyEsc stack kacc (ch:rest) = objReadKey stack (ch:kacc) rest

-- { "key"<>
objWantColon stack key (ch:rest)
  | isSpace ch = objWantColon stack key rest
  | ch == ':' = objWantValue stack key rest

-- { "key" : <>
objWantValue stack key (ch:rest)
  | isSpace ch = objWantValue stack key rest
  | ch == '{' = objStart (ObjKey key : stack) rest
  | ch == '[' = arrStart (ObjKey key : stack) rest
  | ch == '"' = objReadStr stack key [ch] rest
  | otherwise = objReadBare stack key [ch] rest

-- { "key" : "<>
objReadStr stack key vacc (ch:rest)
  | ch == '\\' = objReadStrEsc stack key (ch:vacc) rest
  | ch == '"' = result ++ objAfterVal stack rest
  | otherwise = objReadStr stack key (ch:vacc) rest
  where result = genLine stack key (reverse $ ch:vacc)

-- { "key" : "\<>
objReadStrEsc stack key vacc (ch:rest) = objReadStr stack key (ch:vacc) rest

-- { "key" : 1<>
objReadBare stack key vacc (ch:rest)
  | isSpace ch = result ++ objAfterVal stack rest
  | ch == '}' = result ++ dropLevel stack rest
  | ch == ',' = result ++ objWantKey stack rest
  | otherwise = objReadBare stack key (ch:vacc) rest
  where result = genLine stack key (reverse vacc)

-- { "key" : 123 <>
-- { "key" : "v"<>
objAfterVal stack (ch:rest)
  | isSpace ch = objAfterVal stack rest
  | ch == '}' = dropLevel stack rest
  | ch == ',' = objWantKey stack rest

-- [<>
arrStart stack (ch:rest)
  | isSpace ch = arrStart stack rest
  | ch == ']' = dropLevel stack rest
  | ch == '{' = objStart (ArrIdx 0 : stack) rest
  | ch == '[' = arrStart (ArrIdx 0 : stack) rest
  | ch == '"' = arrReadStr stack 0 [ch] rest
  | otherwise = arrReadBare stack 0 [ch] rest

-- Only differene w/ arrStart is that reading ']' is an error
-- [ 123,<>
arrWantValue stack idx (ch:rest)
  | isSpace ch = arrWantValue stack idx rest
  -- Uncomment to support comma after values, eg. [123,]
  -- | ch == ']' = dropLevel stack rest
  | ch == '{' = objStart (ArrIdx idx : stack) rest
  | ch == '[' = arrStart (ArrIdx idx : stack) rest
  | ch == '"' = arrReadStr stack idx [ch] rest
  | otherwise = arrReadBare stack idx [ch] rest

-- [ "<>
arrReadStr stack idx vacc (ch:rest)
  | ch == '\\' = arrReadStrEsc stack idx (ch:vacc) rest
  | ch == '"' = result ++ arrAfterVal stack idx rest
  | otherwise = arrReadStr stack idx (ch:vacc) rest
  where result = genLine stack (show idx) (reverse $ ch:vacc)

-- [ "\<>
arrReadStrEsc stack idx vacc (ch:rest) = arrReadStr stack idx (ch:vacc) rest

-- [ 1<>
arrReadBare stack idx vacc (ch:rest)
  | isSpace ch = result ++ arrAfterVal stack idx rest
  | ch == ']' = result ++ dropLevel stack rest
  | ch == ',' = result ++ arrWantValue stack (idx+1) rest
  | otherwise = arrReadBare stack idx (ch:vacc) rest
  where result = genLine stack (show idx) (reverse vacc)

-- [ 123 <>
-- [ "v"<>
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

genLine stack key val = mkKey stack [key] ++ ["  ", val, "\n"]

mkKey (ObjKey k : stack) keys = mkKey stack (k : "." : keys)
mkKey (ArrIdx i : stack) keys = mkKey stack (show i : "." : keys)
mkKey _ keys = keys
