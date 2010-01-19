$ cat > Setup.lhs
#! /usr/bin/env runhaskell

> import Distribution.Simple
> import System.Cmd
> import System.Directory
>
> main = defaultMainWithHooks $ simpleUserHooks {postInst = postInstall}

> postInstall _ _ _ _ = do
>   if os == "mingw32" 
>      then removeDirectoryRecursive "c:/bugdir/queryCache"
>      else removeDirectoryRecursive "/var/bugpan/queryCache"
>   return ()
