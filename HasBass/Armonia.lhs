\section {Armon\'{i}a}

\begin{verbatim}

> module Armonia(harmony,seqChords) where
> import Tipos
> import Basicas
> import qualified Haskore.Basic.Duration  as Duration
> import Ratio

\end{verbatim}


En sí, obtener una secuncia de acordes a partir de la cancion ya parseada es muy simple: 
solo hay que concatenar los compases.
Luego, para aproximarnos a algo que sea música, hay que asignarle a cada acorde una duración.
 
Para eso está la función withDurations : dado un compás, cuya duracion es una redonda, 
divide esta duración entre los acordes que lo componen. La división es equitativa, 
salvo cuando un compás esta compuesto de 3 acordes, donde se reparten las duraciones cortas al final. 

\begin{verbatim}

> seqChords :: Song -> [(Chord,Duration.T)]
> seqChords = concatMap withDurations 
> 
> withDurations :: [Chord] -> [(Chord,Duration.T)]
> withDurations xs | length xs == 1          = map (\t -> (t,Duration.wn)) xs
>                  | length xs == 2          = map (\t -> (t,Duration.hn)) xs
>                  | length xs == 3          = (head xs,Duration.hn) : 
>                                              (map (\t -> (t,Duration.qn)) (tail xs)) 
>                  | length xs == 4          = map (\t -> (t,Duration.qn)) xs
>                  | otherwise               = withDurations (take 4 xs)

\end{verbatim}

Ahora solo resta obtener notas absolutas a partir de los acordes para tener 
la informacion concreta de qué se va a tocar, similar a la de una partitura.

\begin{verbatim}

> harmony :: [(Chord,Duration.T)] -> [([AbsNote],Duration.T)]
> harmony = map (\(x,y) -> (chordNotes x 1,y))

\end{verbatim}
