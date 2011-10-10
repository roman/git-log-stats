% Haskell + Git + Stats
% [@romanandreg]
% 2011-10-11

# Purpose

* Why this project exists in the first place?

    * A Post by David Pollak titled
      [Scala use is less good than Java use for at least half of all Java projects]
      stated the following:

    ~~~
    We live in a world where the average developer writes 3,250 lines of
    code per year (about 20 per day).
    ~~~

# Goals


* Implement a Haskell program that serves as showcase of real world usage.


* This program will return an approximate avg LOC per day for a developer
  in multiple repositories, all this by using tools available in Haskell.

* Show kick-ass libraries that make Haskell such an awesome language


# [Attoparsec]

* A library that allows to combine low-level parsers with combinators
  to create new enhanced ones.

* Really intuitive when using Applicative Functors

* Used to parse the output of git log:

~~~
2f84c5d0a8552f2d374a436c873ca625127b38a8, Roman Gonzalez, Mon Oct 10 00:19:10 2011 -0700

2   1 git-stats.cabal
10  7 src/Data/Git/Enumerator.hs
15  4 src/Main.hs
13  5 src/System/Console/Options.hs
~~~

# [Attoparsec] (Git Data Types)

* From that output we want to create a Haskell type like the following:

~~~ {.haskell}
type CommitHash = ByteString
type CommitAuthor = ByteString
type CommitDate = ByteString

data GitCommit
  = GitCommit  {
    _commitRepo   :: String
  , _commitHash   :: CommitHash
  , _commitAuthor :: CommitAuthor
  , _commitDate   :: CommitDate
  , _commitFiles  :: Set CommitFile
  }
  deriving (Show)

data CommitFile
  = CommitFile {
    _cAddedLines   :: Integer
  , _cRemovedLines :: Integer
  , _cFileName     :: ByteString
  }
  deriving (Show, Eq, Ord)

~~~

# [Attoparsec] (Parser implementation)

* For this given output:

~~~
10  7 src/Data/Git/Enumerator.hs
~~~

* We are going to implement a Parser to transform this
  into a `GitFile` entry

~~~ {.haskell}
pCommitFiles :: Parser CommitFile
pCommitFiles =
    CommitFile <$> (skipSpace *>
                    decimal)
               <*> (skipSpace *>
                    decimal)
               <*> (skipSpace *>
                    tillNewline)
~~~

# [Attoparsec] (Parser implementation)

* For this complete output:

~~~
2f84c5d0a8552f2d374a436c873ca625127b38a8, Roman Gonzalez, Mon Oct 10 00:19:10 2011 -0700

2   1 git-stats.cabal
10  7 src/Data/Git/Enumerator.hs
15  4 src/Main.hs
13  5 src/System/Console/Options.hs
~~~

* We use the `GitFile` entry parser + other parsers to create
  a `GitCommit` parser.

~~~ {.haskell}
pGitCommit :: String -> Parser GitCommit
pGitCommit repoPath =
    GitCommit <$> pure repoPath
              <*> (skipSpace *>
                   tillComma <*
                   char ',')
              <*> (skipSpace *>
                   tillComma <*
                   char ',')
              <*> (skipSpace *>
                   tillNewline)
              <*> (Set.fromList <$>
                   many1 pCommitFiles)
~~~


# [Attoparsec] (Parser implementation)

* The final parser that will get all the contents from the git log:

~~~
parse :: String -> Parser [GitCommit]
parse repoPath = many (pGitCommit repoPath)
~~~

# [HSH] - Haskell Shell

* Provides a natural API to interact with the Shell command

~~~ {.haskell}
module Main where

import Data.List (isSuffixOf)
import HSH ((-|-), runIO)
import Text.Printf (printf)

countLines :: [String] -> [String]
countLines = zipWith fn [1..]
  where
    fn i line = printf "%-5d %s" i line

main :: IO ()
main = do
    runIO $ ("ls", ["-l"]) -|- countLines -|- filter (isSuffixOf "hs")
    runIO $ "ls -l" -|- countLines -|- filter (isSuffixOf "hs") -|- "sed -e s/\-/*/g"
~~~

* The output of this code would be:

~~~
3     -rw-r--r-- 1 vagrant users     418 Oct 10 02:38 Main.hs
3     *rw*r**r** 1 vagrant users     418 Oct 10 02:38 Main.hs
~~~


# [HSH] + git-log

* [HSH] will return the stream of data lazily, using the parser
  shown a bit earlier we will have the output transformed into `GitCommit`
  entries


~~~
getGitCommits :: [CommandFlags] -> String -> IO [GitCommit]
getGitCommits flags repoPath = do
  output <- HSH.runIO $ "git log --numstat --no-merges " ++ map show flags
  let result = Attoparsec.parse (Git.parse repoPath) output
  case result of
    Success commits -> return commits
    Failure e -> error e
~~~

# [Enumerator]

* A Library that provides an easy way to manage streams of data by using
  one or multiple producers, filters and a consumer

* Iteratee

    * takes input and does calculation.

    * is executed by `run_` function.

    * can perform side-effects if IO is specified.

    * can be considered as automaton.

    * can be composed with another Iteratee by (>>=):

      `Iteratee >>= Iteratee -> Iteratee`


* Enumerator

    * can be composed with an Iteratee by ($$) resulting in another Iteratee:

      `Enumerator $$ Iteratee -> Iteratee`

    * carries forward the state of Iteratee (automaton) by feeding inputs.

    * can be composed with another Enumerator by (<==<):

      `Enumerator <==< Enumerator -> Enumerator`

* Enumeratee

    * locates in between Enumerator and Iteratee, and may modify input.

    * can be composed with Iteratee by =$ resulting in another Iteratee:

      `Enumeratee =$ Iteratee --> Iteratee`

    * can be composed with Enumerator by $= resulting in another Enumerator:

      `Enumerator $= Enumeratee --> Enumerator`



# [Enumerator] (Example)

* Read a list of `Int` from stdin and return the sum of them

~~~
module Main where

main :: IO ()
main = do
    putStrLn "q to quit"
    sum <- readSum
    print sum

readSum :: IO Int
readSum = go 0
  where
    go accum = do
      n0 <- getLine
      if n0 == "q"
        then return accum
        else do
          let n = fst . head $ reads n0
          readSum (accum + n)
~~~

# [Enumerator] (Example)

* Using the Enumerator library

~~~ {.haskell}
module Main where

import Data.Enumerator hiding (head)
import qualified Data.Enumerator.List as EL

sumIt :: Iteratee Int IO Int
sumIt = go 0
  where
    go accum = do
      n0 <- EL.head
      case n0 of
        Nothing -> return accum
        Just n -> go (accum + n)

main :: IO ()
main = do
  -- returns 0
  run_ sumIt >>= print
  -- returns sum of stdin
  run_ (enumListFromTerminal $$ sumIt) >>= print
  -- returns sum of stdin plus 10
  run_ (enumListFromTerminal $$ (enumList 1 [10] $$ sumIt)) >>= print


enumListFromTerminal :: Enumerator Int IO b
enumListFromTerminal step@(Yield {}) = returnI step
enumListFromTerminal step = Iteratee $ do
    putStrLn "q and hit enter to stop"
    runIteratee (go step)
  where
    go step@(Yield {}) = enumListFromTerminal step
    go step@(Continue consumer) = Iteratee $ do
      n0 <- getLine
      if n0 == "q"
        then return step
        else do
          let n = fst . head $ reads n0
          runIteratee $ go $$ consumer (Chunks [n])

~~~

# Enumeratees

* Say I have an infinite stream of data and I want to halt
  it at a certain point... `Enumeratees` to the rescue!

~~~ {.haskell}
main = run_ (enum $$ it)
  where
    -- Crawl Wikipedia...
    -- OH Wait no, just first 10 results
    enum = enumCrawler "http://wikipedia.org" $= isolate 10
    it = consume
~~~

* `Enumeratees` can transform the streams by halting them until certain
  point


# Back to our problem...

* We are going to enumerate the git log
* We are going to use an Enumeratee to track the data that we
  care about in an internal state

~~~ {.haskell}
1|  main :: IO ()
2|  main = parseOpts $ \repos options -> do
3|      let forbiddenPaths = fromMaybe [] $ haveForbiddenPaths options
4|      let enum = enumRepos options repos
5|      let it = trackLinesPerDay forbiddenPaths
6|             =$ EL.consume
7|      (_, state) <- runAppMonad $ run_ (enum $$ it)
8|      let result = avgLinesPerDay "Roman Gonzalez" state
9|      putStrLn $ "The avg code per day for Roman is " ++ show result
~~~

# Final Thoughts

* Haskell is a powerful (and normally miss-understood) language.

* A lot of real world code can be done in Haskell, the second part of this
  project is to connect to github to fetch your repos automatically
  and start analyzing data.

* Haskell's libraries are freaking powerful

# Thanks

* If you have any questions regarding Haskell, ping me at

  [@romanandreg]

* You can check the code of this presentation and slides code (branch: gh-pages) at:

  [http://github.com/roman/git-stats]

* The slides at:

  [http://roman.github.com/git-stats]
  

[@romanandreg]: http://twitter.com/romanandreg
[http://roman.github.com/git-stats]: http://roman.github.com/git-stats
[http://github.com/roman/git-stats]: http://github.com/roman/git-stats
[Attoparsec]: http://hackage.haskell.org/package/attoparsec
[HSH]: http://hackage.haskell.org/package/HSH
[Enumerator]: http://hackage.haskell.org/package/enumerator
