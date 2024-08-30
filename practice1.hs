import Data.List
import System.IO
import System.Directory (doesFileExist)
import Control.Exception (catch, IOException)
import Control.DeepSeq (deepseq)

-- Definición del tipo 'Articulo'
data Articulo = Articulo {
    nombre :: String,
    categoria :: String
} deriving (Show, Read)

type Inventario = [Articulo]

-- Funciones del Inventario

-- Registrar un artículo en el inventario
registrarArticulo :: Articulo -> Inventario -> Inventario
registrarArticulo articulo inventario = articulo : inventario

-- Guardar el inventario en un archivo
guardarInventario :: Inventario -> IO ()
guardarInventario inventario = do
    withFile "inventario.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarArticulo inventario))
    putStrLn "Inventario guardado en el archivo inventario.txt."

-- Cargar el inventario desde un archivo
cargarInventario :: IO Inventario
cargarInventario = do
    exists <- doesFileExist "inventario.txt"
    if exists
        then withFile "inventario.txt" ReadMode $ \h -> do
            contenido <- hGetContents h
            let lineas = lines contenido
            return (map leerArticulo lineas)
        else return []
  where
    leerArticulo linea = read linea :: Articulo

-- Mostrar un artículo en formato String
mostrarArticulo :: Articulo -> String
mostrarArticulo (Articulo nombre categoria) =
    "Articulo {nombre = \"" ++ nombre ++ "\", categoria = \"" ++ categoria ++ "\"}"

-- Listar los artículos en el inventario
listarArticulos :: Inventario -> IO ()
listarArticulos [] = putStrLn "No hay artículos en el inventario."
listarArticulos inventario = do
    putStrLn "Artículos en el inventario:"
    mapM_ (putStrLn . mostrarArticulo) inventario

-- Buscar artículos por categoría
buscarPorCategoria :: String -> Inventario -> [Articulo]
buscarPorCategoria cat inventario = filter (\a -> categoria a == cat) inventario

-- Mostrar la cantidad de artículos por categoría
cantidadPorCategoria :: Inventario -> [(String, Int)]
cantidadPorCategoria inventario =
    let categorias = groupBy (\a b -> categoria a == categoria b) (sortOn categoria inventario)
    in map (\cat -> (categoria (head cat), length cat)) categorias

-- Menú principal
main :: IO ()
main = do
    putStrLn "¡Bienvenido al Sistema de Gestión de Inventario!"
    inventario <- cargarInventario
    cicloInventario inventario

-- Ciclo del Inventario
cicloInventario :: Inventario -> IO ()
cicloInventario inventario = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar artículo"
    putStrLn "2. Buscar artículo por categoría"
    putStrLn "3. Listar todos los artículos"
    putStrLn "4. Mostrar cantidad de artículos por categoría"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el nombre del artículo:"
            nom <- getLine
            putStrLn "Ingrese la categoría del artículo:"
            cat <- getLine
            let articulo = Articulo { nombre = nom, categoria = cat }
            let inventarioActualizado = registrarArticulo articulo inventario
            guardarInventario inventarioActualizado
            putStrLn $ "Artículo registrado con éxito: " ++ mostrarArticulo articulo
            cicloInventario inventarioActualizado

        "2" -> do
            putStrLn "Ingrese la categoría a buscar:"
            cat <- getLine
            let articulos = buscarPorCategoria cat inventario
            if null articulos
                then putStrLn "No se encontraron artículos en esa categoría."
                else mapM_ (putStrLn . mostrarArticulo) articulos
            cicloInventario inventario

        "3" -> do
            listarArticulos inventario
            cicloInventario inventario

        "4" -> do
            let cantidades = cantidadPorCategoria inventario
            mapM_ (putStrLn . (\(c, q) -> c ++ ": " ++ show q)) cantidades
            cicloInventario inventario

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloInventario inventario
