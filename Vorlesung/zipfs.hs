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

-- ------------------------------------------------------------------
-- command line operations

touch :: Name -> FSZipper -> IO FSZipper
touch x@(item, ctx) = case item of
    (File _ _) = do
        print "focus is on file, do nothing"
        return x
    (Folder name items) = do
        case lookup name items of
            Nothing -> return ()

-- ------------------------------------------------------------------
-- syntactic domains / backend operations

top :: FSItem -> FSZipper
top x = (x, Root)

up :: FSZipper -> FSZipper
up (x, Dir n l r c) = (Folder n (l ++ [x] ++ r), c)

down :: Name -> FSZipper -> FSZipper
down n (Folder fn fl, c) = (x, Dir fn ls rs c)
  where (ls, x:rs) = break (isName n) fl

modify :: FSZipper -> FSZipper


isName :: Name -> FSItem -> Bool
isName n (Folder fn xs) = n == fn
isName n (File fn d) = n == fn


--Concats Data to File
(>>) :: Data -> Name -> FSZipper -> IO FSZipper
nd >> (Folder fn fl, c) = touch (Folder fn fl, c)


nd >> (File fn d, c) = return (File fn (nd ++ d), c)


--Overwrites Data in File
(>) :: Data -> Name -> FSZipper -> IO FSZipper
nd > (File fn d, c) = return (File fn nd, c)

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
