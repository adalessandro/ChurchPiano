\section{ Tipos }

\begin{verbatim}

> module Tipos where
> import Text.ParserCombinators.Parsec
> import qualified Haskore.Basic.Pitch  as Pitch
> import qualified Haskore.Composition.ChordType as ChordType
> import qualified Text.ParserCombinators.ReadP as ReadP
> import Control.Monad

\end{verbatim}

\subsection {Notas}

El tipo Note, lo definimos como sinónimo del tipo Pitch.Class suministrado por la librería Haskore. 
Es una representación del conjunto de equivalencia de todas las frecuencias 
que determinan una misma nota. Es decir, Note no distingue entre distintas teclas del piano, 
pero sí distingue las teclas que producen un Do de las que producen un Re.

Por otro lado, el tipo AbsNote se define como sinónimo del tipo Pitch.Absolute, 
que sí representa cada tecla de un piano, donde no es lo mismo un Do grave que uno agudo.

\begin{verbatim}

> type Note = Pitch.Class
> type AbsNote = Pitch.Absolute

\end{verbatim}

En la entrada solo es necesario reconocer notas no absolutas. Para hacer el parser de ellas se
definen unas funciones auxiliares :

\begin{itemize}

\item extr se usa para extraer el valor parseado de una lista de posibles parseos

\item sToNote usa la función classParse de haskore para convertir un String en una nota.
      Debe usarse solo cuando se está seguro que no va a fallar.

\item noteNat parsea el caracter de una nota sin accidentales

\item acc parsea el caracter de un accidental

\end{itemize}  

parseNote parsea una nota, primero tratando de parsear una nota con accidental, y luego una sin.

\begin{verbatim}

> extr :: [(a,b)] -> a
> extr = fst.head
> 
> sToNote :: String -> Note
> sToNote = extr . Pitch.classParse
> 
> noteNat :: Parser Char
> noteNat = oneOf "ABCDEFG"
> 
> acc :: Parser Char
> acc = oneOf "#b"
> 
> parseNote :: Parser Note
> parseNote = do n <- noteNat
>                ( (acc >>= (\c-> return $ sToNote (n:[c]))) <|> (return $ sToNote [n]))

\end{verbatim}

\subsection{Acordes}

Para reprensentar los acordes se usan datos correspondientes a su notacion simbólica. 
La nota tónica, la nota más grave del acorde, y el tipo de acorde, representado con un tipo de Haskore.
Por ejemplo : Am/G representa el acorde de tonica A, con bajo en G y tipo de acorde menor.

\begin{verbatim}

> data Chord = Cons { root      :: Note,
>                     bassnote  :: Note,
>                     chordType :: ChordType.T } deriving (Eq,Show)

\end{verbatim}

Para parsear un acorde se debe parsear el tipo. Lo que se hace es extraer el substring que se encuentra
entre el final de una nota y un separador que puede ser un espacio, el caracter '/' que indica cual es la nota
más grave del acorde, y el caracter '|' que separa compases. Al extraer este String, se corre el parser de tipo de acordes
implementado en Haskore de tipo ReadP y si falla, se invoca manualmente al fail de parsec.

\begin{verbatim}

> chordTfromString :: String -> [(ChordType.T,String)]
> chordTfromString = filter (null . snd) . ReadP.readP_to_S ChordType.parse 
> 
> sepAc :: Parser Char
> sepAc = (oneOf "/|" <|> space)
> 
> parseChordT :: Parser ChordType.T
> parseChordT = do s <- manyTill anyChar (lookAhead sepAc)
>                  if null (chordTfromString s) then fail ("\""++s++"\" : Tipo de acorde inesperado") 
>                                               else return $ extr $ chordTfromString s  
> 
> parseBassNote :: Parser Note
> parseBassNote = do char '/'
>                    n <- parseNote
>                    return n
> 
> parseChord :: Parser Chord
> parseChord = do r <- parseNote
>                 ct <- parseChordT
>                 b <- parseBassNote <|> (return r)
>                 return $ Cons r b ct

\end{verbatim}

\subsection{Compases y Canciones}

Un Compás se puede pensar como una sucesión de acordes, y una canción es claramente una sucesión de compases.

\begin{verbatim}

> type Bar = [Chord]
> type Song = [Bar]

\end{verbatim}

Para implementar los parsers se usan las funciones de parsec :
\begin{itemize}

\item space, consume un whitespace.
\item spaces, consume reiteradamente un whitespace hasta que falla. 
\item sepEndBy1 p s, que corre al menos un parser p separado y opcionalmente terminado por el parser s.
\item endby1, es igual a sepEndBy1, excepto que no es opcional que termine con el parser separador.

\end{itemize}
\begin{verbatim}

> spaces1 :: Parser ()
> spaces1 = skipMany1 space
> 
> parseBar :: Parser Bar
> parseBar =  (parseChord `sepEndBy1` spaces1) <?> "secuencia de acordes"
> 
> parseSong :: Parser Song
> parseSong = (parseBar) `endBy1` ((char '|') >> spaces)

\end{verbatim}

\subsection{Parseando la Entrada}

La entrada del programa es un número indicando el tempo, y la progresion de acordes, 
asi que el parser final queda :

\begin{verbatim}

> parseInt :: Parser Integer
> parseInt = (liftM read $ many1 digit ) <?> "tempo (bpm)"
> 
> parseInput :: Parser (Song,Integer)
> parseInput = do bpm <- parseInt
>                 spaces
>                 s <- parseSong
>                 eof
>                 return (s,bpm)

Como parseSong consume whitespaces al final, se puede revisar si ya termino la entrada, o quedo algo sin parsear.
\end{verbatim}
