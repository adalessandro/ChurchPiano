\section{ Parseadores }

\textbf{Parseadores} es el módulo que proporciona los parsers para leer las entradas del programa, que son utilizados en el módulo \textbf{Main}.

\begin{verbatim}

> module Parseadores where

\end{verbatim}

\subsection{Import}

\begin{verbatim}

> import Text.ParserCombinators.Parsec
> import qualified Haskore.Composition.ChordType as ChordType
> import qualified Control.Monad as Monad
> import qualified Haskore.Composition.Chord as Chord
> import qualified Haskore.Basic.Pitch as Pitch
> import qualified Haskore.Music as Music
> import qualified Haskore.Basic.Duration as Duration

> import General

> import qualified Tipos

\end{verbatim}

\subsection{Parsers generales}

Los parsers \textbf{sepCho} y \textbf{parseChordT} han sido copiados de HasBass y son utilizados para parsear los acordes.

\begin{verbatim}

> sepCho :: Parser String
> sepCho = (string "," <|> string " ")
>
> parseChordT :: Parser ChordType.T
> parseChordT = do s <- manyTill anyChar (lookAhead sepCho)
>                  if null (Tipos.chordTfromString s)
>                    then fail ("\""++s++"\" : Tipo de acorde inesperado")
>                    else return $ (fst.head) $ Tipos.chordTfromString s

\end{verbatim}

Definimos algunos parsers básicos.

\begin{verbatim}

> espacio :: Parser Char
> espacio = char ' '
>
> espacios :: Parser ()
> espacios = do skipMany espacio
>
> espacios1 :: Parser ()
> espacios1 = do skipMany1 espacio
>
> nuevaLinea :: Parser String
> nuevaLinea = string "\n"
>
> parseInt :: Parser Int
> parseInt = (Monad.liftM read $ many1 digit) <?> "entero"
>
> parseLista :: Parser [Int]
> parseLista = do string "["
>                 xs <- (parseInt `sepBy1` string ",") <?> "lista"
>                 string "]"
>                 return xs

\end{verbatim}

\subsection{Parsers para InputLHV}

Definimos los parsers que consumirán el archivo de InputLHV de acuerdo a la gramática ya especificada en la sección 1.3 .

\begin{verbatim}
 
> parseLHVIzq :: Parser [ChordType.T]
> parseLHVIzq = do (parseChordT `sepBy1` (string "," >> espacios)) <?> "parte izquierda"
> 
> parseLHVDer :: Parser [Chord.T]
> parseLHVDer = do (parseLista `sepEndBy1` espacio) <?> "parte derecha"
> 
> parseLHVLinea :: Parser ([ChordType.T],[Chord.T])
> parseLHVLinea = do i <- parseLHVIzq
>                    espacios
>                    string "->"
>                    espacios
>                    d <- parseLHVDer
>                    many espacio
>                    (string "#" >> manyTill anyChar nuevaLinea ) <|> nuevaLinea
>                    return (i,d)
> 
> parseLHVInput :: Parser LHVInput
> parseLHVInput = do ls <- many parseLHVLinea
>                    eof
>                    return ls

\end{verbatim}

\subsection{Parsers para InputESC}

Definimos los parsers que consumirán el archivo de InputESC de acuerdo a la gramática ya especificada en la sección 1.3 .

\begin{verbatim}

> parseESCIzq :: Parser [ChordType.T]
> parseESCIzq = parseLHVIzq
> 
> parseESCDer :: Parser EscalaRel
> parseESCDer = parseLista
> 
> parseESCLinea :: Parser ([ChordType.T],EscalaRel)
> parseESCLinea = do i <- parseESCIzq
>                    espacios
>                    string "->"
>                    espacios
>                    d <- parseESCDer
>                    many espacio
>                    (string "#" >> manyTill anyChar nuevaLinea ) <|> nuevaLinea
>                    return (i,d)
> 
> parseESCInput :: Parser ESCInput
> parseESCInput = do ls <- many parseESCLinea
>                    eof
>                    return ls

\end{verbatim}

\subsection{Parsers para InputFRA}

Definimos los parsers que consumirán el archivo de InputFRA de acuerdo a la gramática ya especificada en la sección 1.3 .

\begin{verbatim}

> char2Dur :: Char -> Music.Dur
> char2Dur c = case c of
>                   'b' -> Duration.bn
>                   'w' -> Duration.wn
>                   'h' -> Duration.hn
>                   'q' -> Duration.qn
>                   'e' -> Duration.en
>                   's' -> Duration.sn
>                   't' -> Duration.tn
> 
> parseFRAFigura :: Parser [Figura]
> parseFRAFigura = do f <- oneOf "bwhqest"
>                     (string "r" >> return [Silencio (char2Dur f)]) <|>
>                        return [Nota (char2Dur f)]

\end{verbatim}

Definimos el parser de los tresillos dentro de una frase rítmica. Con estas funciones queda abierta la posibilidad de implementar fácilmente otras figuras de valores irregulares (dosillo, tresillo, quintillo, septillo).

\begin{verbatim}
 
> escalar :: Music.Dur -> Figura -> Figura
> escalar k f = case f of
>                    Silencio d -> Silencio (k * d)
>                    Nota d -> Nota (k * d)
> 
> parseFRATresillo :: Parser [Figura]
> parseFRATresillo = do char '('
>                       espacios
>                       f1 <- parseFRAFigura
>                       espacios1
>                       f2 <- parseFRAFigura
>                       espacios1
>                       f3 <- parseFRAFigura
>                       espacios
>                       char ')'
>                       char '3'
>                       return (map (escalar (2 Duration.%+ 3)) (f1++f2++f3))
> 
> parseFRALinea :: Parser Frase
> parseFRALinea = do ts <- ( parseFRATresillo <|> parseFRAFigura ) `sepEndBy1` espacios
>                    rs <- parseFRALinea
>                    return (concat ts ++ rs)
>                 <|>
>                 do nuevaLinea
>                    return []
> 
> parseFRAInput :: Parser FRAInput
> parseFRAInput = do ls <- many parseFRALinea
>                    eof
>                    return ls

\end{verbatim}

\subsection{Parsers para Parametros Generales}

Definimos los parsers que consumirán el archivo de InputLHV de acuerdo a la gramática ya especificada en la sección 1.3 .

\begin{verbatim}

> parseInf :: Parser Int
> parseInf = do string "Inf"
>               espacios
>               string "="
>               espacios
>               string "("
>               n <- parseInt
>               string ","
>               c <- Tipos.parseNote
>               string ")"
>               espacios
>               nuevaLinea
>               return (Pitch.toInt (n,c))
> 
> parseParametro :: String -> Parser String
> parseParametro s = do string s
>                       espacios
>                       string "="
>                       espacios
>                       r <- manyTill anyChar nuevaLinea
>                       return r
> 
> parseParametrosInput :: Parser (Int,[String])
> parseParametrosInput = do inf <- parseInf
>                           lhv <- parseParametro "InputLHV"
>                           esc <- parseParametro "InputESC"
>                           fra <- parseParametro "InputFRA"
>                           mel <- parseParametro "InputMelodia"
>                           arm <- parseParametro "InputArmonia"
>                           out <- parseParametro "OutputFile"
>                           eof
>                           return (inf,[lhv,esc,fra,mel,arm,out])

\end{verbatim}

Combinamos los parsers de las inputs que contienen las armonías, escalas y frases rítmicas.

\begin{verbatim}

> parsearInputs :: String -> String -> String -> Either ParseError (LHVInput, ESCInput, FRAInput)
> parsearInputs inpLHV inpESC inpFRA = let x = parse parseLHVInput "LHV" inpLHV
>                                          y = parse parseESCInput "ESC" inpESC
>                                          z = parse parseFRAInput "FRA" inpFRA
>                                      in parsearInputs' x y z
>
> parsearInputs' :: Either err a -> Either err b -> Either err c -> Either err (a,b,c)
> parsearInputs' (Left err) _ _ = Left err
> parsearInputs' _ (Left err) _ = Left err
> parsearInputs' _ _ (Left err) = Left err
> parsearInputs' (Right a) (Right b) (Right c) = Right (a,b,c)

\end{verbatim}
