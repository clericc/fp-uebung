module Filesystem where

import Control.Monad.State

-- ------------------------------------------------------------------
-- syntactic domains

type Name = String
type Data = String

data FSItem = File Name Data
            | Folder Name [FSItem]
              deriving Show

data FSCtx = Root
           | Dir Name [FSItem] [FSItem] FSCtx
             deriving Show

type FSZipper = (FSItem, FSCtx)

type FsOps a = State FSZipper a

-- ------------------------------------------------------------------
-- command line operations
--{-

newFile :: Name -> Data -> FSZipper -> IO FSZipper
newFile newFileName newFileData x@(item, ctx)
  = case item of
    (File _ _) -> do
      putStrLn "focus is on file, do nothing"
      return x
    (Folder name items) ->
      if newFileName `elem` items  -- Name schon vorhanden?
      then do
        putStrLn "file or folder exists already, do nothing"
        return x
      else
        return (Folder name (mkFile newFileName newFileData):items, ctx)

--}
-- ------------------------------------------------------------------
-- backend operations

top :: FSItem -> FSZipper
top x = (x, Root)

up :: FSZipper -> FSZipper
up (x, Dir n l r c) = (Folder n (l ++ x:r), c)

down :: Name -> FSZipper -> FSZipper
down n (Folder fn fl, c) = (x, Dir fn ls rs c)
  where (ls, x:rs) = break (isName n) fl

isName :: Name -> FSItem -> Bool
isName n (Folder fn xs) = n == fn
isName n (File fn d) = n == fn


mkFile :: Name -> Data -> FSItem
mkFile = File

mkFolder :: Name -> FSItem
mkFolder = Folder

modify :: (FSItem -> FSItem) -> FSZipper -> FSZipper
modify f (i, c) = (f i, c)


fsConcat :: Data -> FSZipper -> IO FSZipper
fsConcat nd (File fn d, c) = return (File fn (d ++ nd), c)

--Concats Data to File
(>>) :: Data -> Name -> FSZipper -> IO FSZipper
(>>) nd n (Folder fn fl, c) = (newFile n "" (Folder fn fl, c)) >>= (fsConcat nd)
(>>) nd n (File fn d, c) = (newFile n "" (File fn d, c)) >>= (fsConcat nd)

--(>>) nd = (=<<) (concat nd)



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
