{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Main where

import Control.Monad.State.Lazy hiding ( modify )

-- ------------------------------------------------------------------
-- syntactic domains

type Name = String
type Data = String
type ReturnCode = Int

data FSItem = File Name Data
            | Folder Name [FSItem]
              deriving Show

data FSCtx = Root
           | Dir Name [FSItem] [FSItem] FSCtx
             deriving Show

type FSZipper = (FSItem, FSCtx)


type FsOps a = StateT FSZipper IO a
 
-- ------------------------------------------------------------------
-- command line operations
--{-

newFile :: Name -> Data -> FsOps ReturnCode
newFile newFileName newFileData = do
  (currItem , ctx) <- get
  case currItem of
    (File _ _) -> do
      liftIO $ putStrLn "*** focus is on file, do nothing"
      return 1
    (Folder fname items) ->
      if newFileName `fselem` items  -- Name schon vorhanden?
      then do
        liftIO $ putStrLn "*** file or folder exists already, do nothing"
        return 2
      else do
        put (Folder fname (mkFile newFileName newFileData : items), ctx)
        return 0


mkdir :: Name -> FsOps ReturnCode
mkdir name = do
  (currItem , ctx) <- get
  case currItem of
    (File _ _) -> liftIO (putStrLn "*** focus on file, do nothing") >> return 1
    (Folder fname items) -> 
      if name `fselem` items
      then liftIO (putStrLn "*** name exists already, do nothing") >> return 2
      else put ( Folder fname (mkFolder name : items) , ctx) >> return 0
      

cd :: Name -> FsOps ReturnCode
cd "/" = get >>= (put . upmost) >> return 0
cd ".." = get >>= (put . up) >> return 0
cd name = do
  l@(currItem, ctx) <- get
  case currItem of
    (File _ _) -> liftIO (putStrLn "*** focus on file, do nothing") >> return 1
    (Folder fname items) ->
      if not $ name `fselem` items
      then liftIO (putStrLn "invalid destination, ignore") >> return 2
      else
        let obj = down name l
        in case obj of
          (File _ _, c) -> liftIO (putStrLn "is file") >> return 3
          (Folder fn i, c) -> put (obj) >> return 0
          
pwd :: FsOps ReturnCode
pwd = do
  z <- get    
  liftIO (putStrLn $ buildPwd z) >> return 0
  where
    buildPwd (v, Root) = showName v
    buildPwd l@(v, c) = (buildPwd (up l)) ++ "\\" ++ (showName v)



cat :: Name -> FsOps ReturnCode
cat name = do
  l@(currItem, ctx) <- get
  case currItem of
    (File _ _) -> liftIO (putStrLn "*** focus on file, do nothing") >> return 1
    (Folder fname items) ->
      if not $ name `fselem` items
      then liftIO (putStrLn "invalid filename") >> return 2
      else
        let obj = down name l
        in case obj of
          (File n d, c) -> liftIO (putStrLn d) >> return 0
          (Folder fn i, c) -> liftIO (putStrLn ("cat: " ++ fn ++ "/ is directory")) >> return 3
          
fsAppend :: Name -> Data -> FsOps ReturnCode
fsAppend name dat = do
  l@(currItem, ctx) <- get
  case currItem of
    (File _ _) -> liftIO (putStrLn "*** focus on file, do nothing") >> return 1
    (Folder fname items) ->
      if not $ name `fselem` items
      then liftIO (putStrLn "invalid filename") >> return 2
      else
        let obj = down name l
        in case obj of
          (File n d, c) -> put (up $ fsattach dat obj) >> return 0
          (Folder fn i, c) -> liftIO (putStrLn ("target is a directory!")) >> return 3



ls :: FsOps ReturnCode
ls = do
  (currItem , ctx) <- get
  case currItem of
    (File _ _)       -> liftIO (putStrLn "*** focus on file, do nothing") >> return 1
    (Folder _ items) ->
      mapM_ (liftIO . putStrLn . showName) items >> return 0

--{-
mv :: Name -> Name -> FsOps ReturnCode
mv oldName newName = do
  (currItem , ctx) <- get
  case currItem of
    (File      _ _) -> liftIO (putStrLn "*** focus on file, do nothing") >> return 1
    (Folder name _) -> 
      let newContent = (up . modify (rename newName) . down oldName) (currItem, ctx)
      in  put newContent >> return 0

--}








--Concats Data to File

--(:->>) :: Data -> Name -> FSZipper -> IO FSZipper
--(:->>) nd n (Folder fn fl, c) = (newFile n "" (Folder fn fl, c)) >>= (fsConcat nd)
--(:->>) nd n (File fn d, c) = (newFile n "" (File fn d, c)) >>= (fsConcat nd)

--}

-- ------------------------------------------------------------------
-- backend operations

bashFS = runStateT bash myDiskState

bash :: FsOps ()
bash = do
  (currItem, ctx) <- get
  liftIO . putStr . (++ " >> ") . showName $ currItem
  cmd <- liftIO getLine
  case words cmd of
    ["ls"]                    -> ls
    ["cd", name]              -> cd name
    ["mv", fname, nname]      -> mv fname nname
    ["newFile", fname, fdata] -> newFile fname fdata
    ["mkdir", name]           -> mkdir name 
    ["cat", name]             -> cat name
    ["pwd"]                   -> pwd
    [dat, ">>", name]         -> fsAppend name dat
    [dat, ">", name]          -> newFile name dat
    _                         ->
      let (ls, rs) = break (">>"==) $ words cmd
      in 
      if (null rs) || (null (tail rs))
      then liftIO $ putStrLn "unknown operation"  >> return 0
      else fsAppend (last rs) (concatPlus ls ' ')
  bash


main = bashFS


-- ------------------------------------------------------------------
-- backend operations
--{-

concatPlus :: [[a]] -> a -> [a]
concatPlus [x] v = x
concatPlus (x:xs) v = x ++ [v] ++ (concatPlus xs v)

fselem :: Name -> [FSItem] -> Bool
fselem name [] = False
fselem name (x:xs) = case x of
  (File   n _) -> n == name || fselem name xs
  (Folder n _) -> n == name || fselem name xs

mkFile :: Name -> Data -> FSItem
mkFile = File

mkFolder :: Name -> FSItem
mkFolder name = Folder name []

fsattach :: Data -> FSZipper -> FSZipper
fsattach d = modify (\(File fn fd) -> File fn (fd ++ d))

isName :: Name -> FSItem -> Bool
isName n (Folder fn xs) = n == fn
isName n (File fn d) = n == fn

showName :: FSItem -> String
showName (Folder name _) = name
showName (File   name _) = name

rename :: Name -> FSItem -> FSItem
rename newName (File   name content) = File   newName content
rename newName (Folder name content) = Folder newName content

--}
-- ------------------------------------------------------------------
-- zipper operations


top :: FSItem -> FSZipper
top x = (x, Root)

up :: FSZipper -> FSZipper
up (x, Dir n l r c) = (Folder n (l ++ x:r), c)
up z = z

upmost :: FSZipper -> FSZipper
upmost (n, Root) = (n, Root)
upmost z = upmost (up z)

down :: Name -> FSZipper -> FSZipper
down n (Folder fn fl, c) = (x, Dir fn ls rs c)
  where (ls, x:rs) = break (isName n) fl


--Uses a given function to modify the focused element
modify :: (FSItem -> FSItem) -> FSZipper -> FSZipper
modify f (i, c) = (f i, c)

-- ------------------------------------------------------------------
-- sample file system

myDiskState :: FSZipper
myDiskState  = (myDisk, Root)

myDisk :: FSItem
myDisk =
    Folder "root"
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
            , Folder "source code"
                [ File "best_hs_prog.hs" "main = print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]
