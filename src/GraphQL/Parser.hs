module GraphQL.Parser
 ( parseQuery
 , ParseResult
 ) where

import GraphQL.Core (Query(..))

import Control.Monad.State
import Data.Char (isSpace, isAlphaNum)
import qualified Data.ByteString.Lazy.Char8 as BC

type ParserState = (BC.ByteString, Int)
type ParserM = State ParserState
type ParseResult a = Either String a

chomp :: ParserM (Maybe Char)
chomp = state $ \(str, p) -> case (BC.uncons str) of
  Just (c, tl) -> (Just c, (tl, p + 1))
  _            -> (Nothing, (BC.empty, p))

peek :: ParserM (Maybe Char)
peek = state $ \(str, p) -> case (BC.uncons str) of
  Just (c, _)  -> (Just c, (str, p))
  _            -> (Nothing, (BC.empty, p))

chompIf :: (Char -> Bool) -> ParserM (Maybe Char)
chompIf p = do
  maybeC <- peek
  case maybeC of
    Just c
      | p c         -> chomp
      | otherwise   -> return Nothing

    _ -> return Nothing

chompUntil = chompUntil' ""
  where
    chompUntil' :: String -> (Char -> Bool) -> ParserM String
    chompUntil' str p = do
      maybeC <- peek
      case maybeC of
        Just c
          | p c       -> return str
          | c == '\n' -> chomp >> (chompUntil' str p)
          | otherwise -> chomp >> (chompUntil' (str ++ [c]) p)
        _             -> return str

chompWord = chompUntil isSpace
chompLine = chompUntil (== '\r')

chompWhile = chompWhile' ""
  where
    chompWhile' :: String -> (Char -> Bool) -> ParserM String
    chompWhile' str p = do
      maybeC <- peek
      case maybeC of
        Just c
          | p c       -> chomp >> (chompWhile' (str ++ [c]) p)
          | c == '\n' -> chomp >> (chompWhile' str p)
          | otherwise -> return str
        _             -> return str

chompAll = chompAll' ""
  where
    chompAll' :: String -> ParserM String
    chompAll' str = do
      maybeC <- chomp
      case maybeC of
        Just c  -> chompAll' (str ++ [c])
        _       -> return str

succeed :: a -> State ParserState (ParseResult a)
succeed x = return $ Right x

failMsg :: String -> State ParserState (ParseResult a)
failMsg msg = do
  (_, pos) <- get
  return $ Left (msg ++ " at position " ++ (show pos))

parseFields :: ParserM (ParseResult [Query])
parseFields = do
    maybeC <- peek
    case maybeC of
      Just '{' -> do
        chomp >> next []
      Just c ->
        failMsg $ "expected '{', got '" ++ [c] ++ "'"
      _      ->
        failMsg "expected '{', got end of input"
  where
    next :: [Query] -> ParserM (ParseResult [Query])
    next fs = do
      chompWhile isSpace
      maybeC <- peek
      case maybeC of
        Just '}' -> chomp >> succeed fs
        Nothing  -> failMsg "unexpected end of input"
        _ -> do
          nameResult <- parseNameAndAlias
          case nameResult of
            Left err            -> return $ Left err
            Right (name, alias) -> do
              chompWhile isSpace
              maybeC <- peek
              case maybeC of
                Just '{' -> do
                  result <- parseFields
                  case result of
                    Left err     -> return $ Left err
                    Right fields -> next (fs ++ [Query name alias fields])
                Just ',' -> chomp >> next (fs ++ [Query name alias []])
                Nothing  -> failMsg "unexpected end of input"
                _        -> next (fs ++ [Query name alias []])

    parseNameAndAlias :: ParserM (ParseResult (String, Maybe String))
    parseNameAndAlias = do
        nameResult <- parseFieldName
        aliasResult <- parseFieldAlias
        case (nameResult, aliasResult) of
            (Left err, _)   -> return $ Left err
            (_, Left err)   -> return $ Left err
            (Right alias, Right (Just field)) -> return $ Right (field, Just alias)
            (Right field, Right Nothing) -> return $ Right (field, Nothing)

    parseFieldName :: ParserM (ParseResult String)
    parseFieldName = do
      maybeC <- peek
      case maybeC of
        Just c
          | isAlphaNum c -> do
              name <- chompWhile isAlphaNum
              succeed name
          | otherwise ->
              failMsg $ "unexpected '" ++ [c] ++ "'"

        _ -> failMsg "unexpected end of input"

    parseFieldAlias :: ParserM (ParseResult (Maybe String))
    parseFieldAlias = do
      maybeC <- peek
      case maybeC of
        Just ':' -> do
            chomp >> (chompWhile isSpace)
            alias <- chompWhile isAlphaNum
            succeed (Just alias)

        Just ',' ->
            succeed Nothing
        Just '}' ->
            succeed Nothing

        Just c
          | not (isSpace c) -> failMsg $ "5 unexpected '" ++ [c] ++ "'"
          | otherwise       -> succeed Nothing

        _       -> failMsg "unexpected end of input"


parseQuery :: String -> ParseResult [Query]
parseQuery queryStr = evalState parseFields (BC.pack queryStr, 0)