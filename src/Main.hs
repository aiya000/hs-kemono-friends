{-# LANGUAGE OverloadedStrings #-}

module Main where

--FIXME: error関数はTextをひょうじできないけど、へーきへーき！

import Brainhack.Evaluator
import Brainhack.Parser
import Brainhack.Parser.Items
import Control.Exception.Safe (SomeException)
import Control.Monad (void)
import System.Environment (getArgs)
import qualified Data.Text as T

-- | The nico-lang's expression of the brainf*ck's ><+-.,[]
data Kemono = Tanoshi
            | Tanoshii
            | Sugoi
            | Sugooi
            | Uwa
            | Wai
            | Nanikore
            | Omoshiro
  deriving (Eq)

instance BrainfuckOperation Kemono where
  forward   = Tanoshi
  incr      = Tanoshii
  backword  = Sugoi
  decr      = Sugooi
  loopBegin = Uwa
  loopEnd   = Wai
  output    = Nanikore
  input     = Omoshiro
  toToken Tanoshi  = "たのしー！"
  toToken Tanoshii = "たーのしー！"
  toToken Sugoi    = "すごーい！"
  toToken Sugooi   = "すっごーい！"
  toToken Uwa      = "うわー！"
  toToken Wai      = "わーい！"
  toToken Nanikore = "なにこれなにこれ！"
  toToken Omoshiro = "おもしろーい！"
  toToken _ = error "キミはエラーを出してしまうフレンズなんだね！"
  fromToken "たのしー！"         = Just Tanoshi
  fromToken "たーのしー！"       = Just Tanoshii
  fromToken "すごーい！"         = Just Sugoi
  fromToken "すっごーい！"       = Just Sugooi
  fromToken "うわー！"           = Just Uwa
  fromToken "わーい！"           = Just Wai
  fromToken "なにこれなにこれ！" = Just Nanikore
  fromToken "おもしろーい！"     = Just Omoshiro
  fromToken _                    = Nothing

type KemonoProgram = BrainfuckProgram Kemono

main :: IO ()
main = do
  file <- head <$> getArgs
  code <- T.pack <$> readFile file
  case (parse code :: Either SomeException KemonoProgram) of
    Left  e -> error $ "BF方言のコードを書くのが苦手なフレンズなんだね！へーきへーき！フレンズによってとくいなことちがうから！" ++ show e
    Right a -> void $ flip runBrainState emptyMachine $ eval a
