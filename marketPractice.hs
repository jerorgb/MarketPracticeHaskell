{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Data.List (find)
import Control.Exception (IOException, try, catch)
import Control.Concurrent (threadDelay)
import Data.Functor ((<&>))
import System.IO (withFile, IOMode(WriteMode, ReadMode), hPutStr, hGetContents)
import System.IO.Error (isDoesNotExistError, isPermissionError)

-- Definición del tipo de datos Product
data Product = Product
  { name     :: String
  , category :: String
  } deriving (Show, Read)

-- Insertar un producto en el catálogo
insertProduct :: String -> String -> [Product] -> [Product]
insertProduct name category catalog =
  Product name category : catalog

-- Buscar un producto por categoría
searchProduct :: String -> [Product] -> Maybe Product
searchProduct keyWord catalog =
  case filter (\x -> keyWord == category x) catalog of
    [] -> Nothing
    (x:_) -> Just x

-- Mostrar un producto como cadena
showProduct :: Product -> String
showProduct (Product name category) = name ++ " : " ++ category

-- Leer el catálogo desde el archivo
loadCatalog :: IO [Product]
loadCatalog = do
  result <- try (readFile "marketList.txt") :: IO (Either IOException String)
  case result of
    Left ex -> do
      putStrLn $ "Error loading catalog: " ++ show ex
      return []
    Right text -> return (map readCatalog (lines text))
  where
    readCatalog line = read line :: Product

-- Mostrar todo el catálogo
showCatalog :: IO [Product]
showCatalog = do
  text <- readFile "marketList.txt"
  let linesOfText = lines text
  return (map read linesOfText)

-- Leer un producto desde una línea de texto
readCatalog :: String -> Product
readCatalog line = case words line of
  [name, category] -> Product name category
  _ -> error "Malformed line in catalog file"

-- Guardar el catálogo en el archivo
saveCatalog :: IO [Product]
saveCatalog = do
  putStrLn "Loading..."
  result <- try (withFile "marketList.txt" ReadMode $ \handle -> do
    putStrLn "Loading complete."
    contents <- hGetContents handle
    putStrLn "Content loaded."
    return $! lines contents) :: IO (Either IOException [String])

  case result of
    Left exc
      | isDoesNotExistError exc -> do
          putStrLn "Archive does not exist, one will be created..."
          return []
      | isPermissionError exc -> do
          putStrLn "Error: permission error"
          return []
      | otherwise -> do
          putStrLn $ "Error: Error loading" ++ show exc
          return []
    Right linesOfText -> do
      putStrLn $ "Products loaded: " ++ show (length linesOfText)
      return (map readCatalog linesOfText)

-- Guardar un producto en el archivo
saveProduct :: [Product] -> IO ()
saveProduct catalog = do
  withFile "marketList.txt" WriteMode $ \handle -> do
    mapM_ (hPutStr handle . show) catalog

-- Actualizar el catálogo escribiéndolo en el archivo
updateCatalog :: [Product] -> IO ()
updateCatalog catalog = do
  result <- retryOp 5 (writeFile "marketList.txt" (unlines (map showProduct catalog)))
  case result of
    Left ex  -> putStrLn $ "Error saving product: " ++ show ex
    Right _  -> putStrLn "Product saved in marketList.txt."

-- Reintentar una operación IO con un número dado de intentos
retryOp :: Int -> IO x -> IO (Either IOException x)
retryOp 0 action = catch (action <&> Right) (\(ex :: IOException) -> return (Left ex))
retryOp n action = catch (action <&> Right) (\(ex :: IOException) -> do
    threadDelay 1000000  --wait a sec
    retryOp (n - 1) action
  )

-- Bucle central de la aplicación
centralLoop :: [Product] -> IO ()
centralLoop catalog = do
  putStrLn "Select an option:"
  putStrLn "1. Register a product"
  putStrLn "2. Search a product by category"
  putStrLn "3. Show all catalog"
  putStrLn "4. List items by category"
  putStrLn "5. Exit"

  option <- getLine
  let opt = read option :: Int
  case opt of
    1 -> do
      putStrLn "Enter the name of the product:"
      name <- getLine
      putStrLn "Enter the category of the product:"
      category <- getLine
      let updatedCatalog = insertProduct name category catalog
      putStrLn "Product saved."
      updateCatalog updatedCatalog
      centralLoop updatedCatalog

    2 -> do
      putStrLn "Enter the category of the product:"
      keyWord <- getLine
      case searchProduct keyWord catalog of
        Just product -> putStrLn $ "Product with category " ++ keyWord ++ " found: " ++ showProduct product
        Nothing -> putStrLn $ "No product with category " ++ keyWord ++ " found."
      centralLoop catalog

    3 -> do
      putStrLn "Showing all catalog..."
      mapM_ (\x -> putStrLn $ "Name: " ++ name x ++ " Category: " ++ category x) catalog
      centralLoop catalog

    4 -> do
      putStrLn "Enter the category of the products to search:"
      userInput <- getLine
      let keyWord = userInput
      let products = filter (\x -> keyWord == category x) catalog
      if null products
        then putStrLn $ "No products found in category: " ++ keyWord
        else mapM_ (putStrLn . showProduct) products
      centralLoop catalog

    5 -> putStrLn "Finishing App..."

    _ -> do
      putStrLn "Error: Invalid Option"
      centralLoop catalog

-- Función principal de la aplicación
main :: IO ()
main = do
  catalog <- loadCatalog
  putStrLn "Welcome to the D1 Backend!"
  centralLoop catalog