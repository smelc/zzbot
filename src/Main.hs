module Main where

import Config

main :: IO ()
main = do
  putStrLn $ show shellCmd0
  putStrLn $ show builder
  where shellCmd0 :: Step
        shellCmd0 = ShellCmd ["ls", "/media"]
        shellCmd1 = ShellCmd ["cd", "/media"]
        builder = Builder "bname" [shellCmd0, shellCmd1]
