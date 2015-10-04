\section{ Main }

Este es el módulo principal desde donde se comienza a ejecutar el programa.

\begin{verbatim}

> module Main where

\end{verbatim}

\subsection{Import}

\begin{verbatim}

> import Haskore.Music as Music
> import qualified Sound.MIDI.File.Load as LoadMidi
> import System.Random
> import qualified System.Environment as Environment
> import Text.ParserCombinators.Parsec
>
> import General
> import qualified Parseadores
> import Proceso
>
> import qualified Tipos

\end{verbatim}

\subsection{Función main}

La función principal \textbf{Main} lee la entrada, la parsea (indicando los casos de error), inicializa el generador de aleatoriedad y llama a la función \textbf{proc} del módulo \textbf{Proceso}.

Para parsear la entrada se usan los parsers del módulo \textbf{Parseadores} y \textbf{parseInput} del módulo \textbf{Tipos} de \textbf{HasBass}.

\begin{verbatim}

> main :: IO ()
> main = do xs <- Environment.getArgs
>           case (length xs) of
>              1 -> do let [x] = xs
>                      param <- readFile x
>                      case parse Parseadores.parseParametrosInput "" param of
>                         Left err ->
>                            do putStr "Parametros: error de parseo en "
>                               print err
>                         Right (inf,ls) ->
>                            do let [lhvF,escF,fraF,melF,armF,outF] = ls
>                               lhvInp <- readFile lhvF
>                               escInp <- readFile escF
>                               fraInp <- readFile fraF
>                               case Parseadores.parsearInputs lhvInp escInp fraInp of
>                                  Left err ->
>                                     do putStr "Librerias: error de parseo en "
>                                        print err
>                                  Right (l,e,f) ->
>                                     do m <- LoadMidi.fromFile melF
>                                        a <- readFile armF
>                                        g <- newStdGen
>                                        case (parse Tipos.parseInput "" a) of
>                                           Left err ->
>                                              do putStr "Armonia: error de parseo en "
>                                                 print err
>                                           Right (s,bpm) -> proc g s bpm l e f m inf outF
>              _ -> do putStrLn "ERROR: Debe dar la ruta del archivo de param. generales"

\end{verbatim}
