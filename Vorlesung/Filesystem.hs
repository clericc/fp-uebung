{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Main where

import Control.Monad.State.Lazy hiding ( modify )
import System.IO

-- ------------------------------------------------------------------
-- syntactic domains

type Name = String
type Data = String
type ReturnCode = Int

data FsItem = File Name Data
            | Folder Name [FsItem]
              deriving Show

data FsCtx = Root
            | Dir Name [FsItem] [FsItem] FsCtx
              deriving Show

type FsZipper = (FsItem, FsCtx)


type FsOps a = StateT FsZipper IO a

-- ------------------------------------------------------------------
-- command line operations

newFile :: Name -> Data -> FsOps ReturnCode
newFile name content = applyToFolder $ \ ( (Folder fname fcontent) , ctx) ->
  if name `fselem` fcontent  -- Name schon vorhanden?
  then do
    liftIO $ putStrLn "*** file or folder exists already, do nothing"
    return 2
  else do
    put (Folder fname (mkFile name content : fcontent), ctx)
    return 0


touch      :: Name -> FsOps ReturnCode
touch name  = newFile name ""


mkdir :: Name -> FsOps ReturnCode
mkdir name = applyToFolder $ \ ( (Folder fname fcontent) , ctx) ->
  if name `fselem` fcontent
  then liftIO (putStrLn "*** name exists already, do nothing") >> return 2
  else put ( Folder fname (mkFolder name : fcontent) , ctx) >> return 0


cd :: Name -> FsOps ReturnCode
cd "/" = get >>= (put . upmost) >> return 0
cd ".." = get >>= (put . up) >> return 0
cd name = applyToFolder $ \ zipper@( (Folder fname content) , ctx) ->
  if not . fselem name $ content
  then liftIO (putStrLn "invalid destination, ignore") >> return 2
  else
    let obj = down name zipper
    in case obj of
      (File _ _, c) -> liftIO (putStrLn "is file") >> return 3
      (Folder fn i, c) -> put (obj) >> return 0


pwd :: FsOps ReturnCode
pwd = do
  z <- get
  liftIO (putStrLn $ buildPwd z) >> return 0
  where
    buildPwd (v, Root) = showName v
    buildPwd l@(v, c) = (buildPwd (up l)) ++ "/" ++ (showName v)


cat :: Name -> FsOps ReturnCode
cat name = applyToFolder $ \ zipper@( (Folder fname content) , ctx) ->
  if not . fselem name $ content
  then liftIO (putStrLn "invalid filename") >> return 2
  else
    let obj = down name zipper
    in case obj of
      (File n d, c) -> liftIO (putStrLn d) >> return 0
      (Folder fn i, c) -> liftIO (putStrLn ("cat: " ++ fn ++ "/ is directory")) >> return 3


fileAppend :: Name -> Data -> FsOps ReturnCode
fileAppend name dat = applyToFolder $ \ zipper@( (Folder fname content) , ctx) ->
  if not . fselem name $ content
  then liftIO (putStrLn "invalid filename") >> return 2
  else
    let obj = down name zipper
    in case obj of
      (File n d, c) -> put (up $ fsattach dat obj) >> return 0
      (Folder fn i, c) -> liftIO (putStrLn ("target is a directory!")) >> return 3


ls :: FsOps ReturnCode
ls = applyToFolder $ \ ( (Folder name content) , ctx) ->
  mapM_ (liftIO . putStrLn . showName) content >> return 0


mv :: Name -> Name -> FsOps ReturnCode
mv oldName newName = applyToFolder $ \ (currItem, ctx) ->
  let newContent = (up . modify (rename newName) . down oldName) (currItem, ctx)
  in  put newContent >> return 0


-- provides the guarantee to work on a Folder. If the Zipper focus is on a
-- file, throw an error message and continue as if nothing happened
applyToFolder ::  (FsZipper -> FsOps ReturnCode) -> FsOps ReturnCode
applyToFolder f = do
  zipper@(currItem , ctx) <- get
  case currItem of
    (File      _ _) -> liftIO (putStrLn "*** focus on file, do nothing") >> return 1
    _          -> f zipper



-- ------------------------------------------------------------------
-- backend operations

bashFS = runStateT bash myDiskState

bash :: FsOps ()
bash = do
  (currItem, ctx) <- get
  liftIO . putStr . (++ " >> ") . showName $ currItem
  liftIO $ hFlush stdout
  cmd <- liftIO getLine
  case words cmd of
    ["ls"]                    -> ls
    ["cd", name]              -> cd name
    ["mv", fname, nname]      -> mv fname nname
    ["newFile", fname, fdata] -> newFile fname fdata
    ["mkdir", name]           -> mkdir name
    ["cat", name]             -> cat name
    ["pwd"]                   -> pwd
    ["exit"]                  -> return 0
    []                        -> return 0
    [dat, ">>", name]         -> fileAppend name dat
    [dat, ">", name]          -> newFile name dat
    _                         ->
      let (ls, rs) = break (">>"==) $ words cmd
      in
      if (null rs) || (null (tail rs))
      then liftIO $ putStrLn "unknown operation"  >> return 0
      else fileAppend (last rs) (concatPlus ls ' ')

  when (cmd /= "exit") bash


main = bashFS



-- ------------------------------------------------------------------
-- backend operations

concatPlus :: [[a]] -> a -> [a]
concatPlus [x] v = x
concatPlus (x:xs) v = x ++ [v] ++ (concatPlus xs v)

fselem :: Name -> [FsItem] -> Bool
fselem name [] = False
fselem name (x:xs) = case x of
  (File   n _) -> n == name || fselem name xs
  (Folder n _) -> n == name || fselem name xs

mkFile :: Name -> Data -> FsItem
mkFile = File

mkFolder :: Name -> FsItem
mkFolder name = Folder name []

fsattach :: Data -> FsZipper -> FsZipper
fsattach d = modify (\(File fn fd) -> File fn (fd ++ d))

isName :: Name -> FsItem -> Bool
isName n (Folder fn xs) = n == fn
isName n (File fn d) = n == fn

showName :: FsItem -> String
showName (Folder name _) = name
showName (File   name _) = name

rename :: Name -> FsItem -> FsItem
rename newName (File   name content) = File   newName content
rename newName (Folder name content) = Folder newName content



-- ------------------------------------------------------------------
-- zipper operations

top :: FsItem -> FsZipper
top x = (x, Root)

up :: FsZipper -> FsZipper
up (x, Dir n l r c) = (Folder n (l ++ x:r), c)
up z = z

upmost :: FsZipper -> FsZipper
upmost (n, Root) = (n, Root)
upmost z = upmost (up z)

down :: Name -> FsZipper -> FsZipper
down n (Folder fn fl, c) = (x, Dir fn ls rs c)
  where (ls, x:rs) = break (isName n) fl

--Uses a given function to modify the focused element
modify :: (FsItem -> FsItem) -> FsZipper -> FsZipper
modify f (i, c) = (f i, c)



-- ------------------------------------------------------------------
-- sample file system

myDiskState :: FsZipper
myDiskState  = (myDisk, Root)

myDisk :: FsItem
myDisk =
    Folder "/"
        [ File "hello.txt" "Hallo Welt"
        , File "fp.hs" "insert mega cool Haskell Code here"
        , Folder "pics"
            [ File "cat.jpg" "Awwww"
            , File "cat.gif" "OMG so cute!!"
            , File "cat.bmp" "AWWWWWWWWWWWWWWW"
            ]
        , File "cookies.pdf" "nomnomnom"
        , Folder "programs"
            [ File "hello.exe" "print('Hello World')"
            , File "hack.bat" "open('iexplorer.exe')"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source_code"
                [ File "best_hs_prog.hs" "main = print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]
