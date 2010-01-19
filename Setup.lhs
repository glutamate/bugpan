$ cat > Setup.lhs
#! /usr/bin/env runhaskell

> import Distribution.Simple
> import System.Cmd
> import System.Directory
> import System.Info
> import Control.Monad
>
> main = defaultMainWithHooks $ simpleUserHooks {postInst = postInstall}

> basedir = if os == "mingw32" 
>             then  "c:/bugdir/"
>             else  "/var/bugpan/"

> qcDir = basedir++"queryCache"

> postInstall _ _ _ _ = do
>   bex <- doesDirectoryExist basedir
>   qcex <- doesDirectoryExist qcDir
>   when (not bex ) $ do
>         createDirectory basedir
>   when (qcex) $ do
>         removeDirectoryRecursive qcDir
>   return ()
