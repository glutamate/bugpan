$ cat > Setup.lhs
#! /usr/bin/env runhaskell

> import Distribution.Simple
> import System.Cmd
> main = defaultMainWithHooks $ simpleUserHooks {postInst = postInstall}

> postInstall _ _ _ _ = do
>   system "rm -rf /var/bugpan/queryCache/*"
>   return ()
