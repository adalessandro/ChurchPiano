\section {Walking Bass}

\begin{verbatim}

> module Bajo(composeBass) where
> import Tipos
> import Basicas
> import Armonia
> import System.Random
> import qualified Haskore.Basic.Pitch  as Pitch
> import qualified Haskore.Basic.Duration  as Duration
> import Ratio

\end{verbatim}

Primero defino unas funciones que nos serán útiles más adelante : 

\begin{itemize}
\item leadTone toma una nota absoluta, y devuelve una lista de notas que se encuentran a un semitono de ella. Esto se hace para que el Walking Bass incorpore cromatismos (notas consecutivas que difieren en un semitono).

\item cerca elimina aquellas notas que estén lejos (a muchos semitonos) de una nota dada. Evita que haya saltos demasiado grandes en la melodia.

\item laMasCerca toma una nota n, una nota absoluta s y calcula cual es la nota absoluta perteneciente a la misma clase que n, que esté más cerca
de s. Para lograrlo solo hace falta comparar con 2 notas ya que el bajo no toca notas fuera de las dos primeras octavas.

\item oneOf elige un elemento al azar de una lista.
\end{itemize}
\begin{verbatim}

> leadTone :: AbsNote -> [AbsNote]
> leadTone s = [s-1,s-1,s-1,s+1,s+1,s+1]
>
> cerca :: Int -> AbsNote -> [AbsNote] -> [AbsNote]
> cerca maxinterval n ct = let r = filter (\x -> abs(n - x) < maxinterval) ct in
>                              if (null r) then (take 5 ct) else r					
>
> laMasCerca :: Note -> AbsNote -> AbsNote
> laMasCerca n s = if (abs(n0 - s) < abs(n1 - s)) then n0 else n1  
>                  where n0 = Pitch.toInt (0,n)
>                        n1 = Pitch.toInt (-1,n)
>
> oneof :: (RandomGen g) => g -> [a] -> (a,g) 
> oneof g xs = (xs !! ri,rg) 		  
>              where (ri,rg) = randomR (0,length xs - 1) g

\end{verbatim}


Ahora ya podemos hacer un walking bass : 

\begin{verbatim}

> walkingBass :: (RandomGen g) => g -> Int -> Note -> AbsNote -> [AbsNote] -> ([AbsNote],g)
> walkingBass g 1 t s _ = ([laMasCerca t s],g)
> walkingBass g n t s cn | mod n 2 == 0      = let (ne,g') = oneof g ((cerca 5 s cn) ++ 
>                                                                             leadTone s) 
>                                                  (w,g'') = walkingBass g' (n-1) t ne cn 
>                                              in (w ++ [ne] , g'')
>                                                 
>                        | otherwise         = let (ne,g') = oneof g (cerca 9 s cn)
>                                                  (w,g'') = walkingBass g' (n-1) t ne cn 
>                                              in (w ++ [ne] , g'')

\end{verbatim}

En un walking bass es importante saber cual es la primer nota que se debe tocar y cual es la ultima (un punto de partida y un objetivo de llegada). 
En este caso se contruye de atrás para adelante, es decir walkingBass g n t s cn, 
es igual a una secuencia de n notas tal que t es la nota por la que se empieza y 
s es la nota objetivo, donde las notas del acorde son cn. (Es decir, s se tocaría despues de la última nota del walking bass). 

Ahora solo resta definir las notas candidatas a ser tocadas antes que s, de las cuales se elegira una al azar.

La función sigue dos reglas : 
\begin{itemize}
\item Los tiempos impares son fuertes armonicamente, es decir, solo se tocan notas que pertenecen al acorde (alguna de las notas de cn)
\item Los tiempos pares son fuertes rítmicamente, así que se pueden tocar tanto notas del acorde o también cromatismos (notas que difieren de un semitono)
\end{itemize}

Ahora que ya se tiene una funcion para caminar por un acorde dado, faltan definir las funciones que generen el bajo de toda una cancion :

Dado un acorde y su duracion, bassOverChord retorna las notas (negras = quarter notes = qn) que se tocaran sobre ese acorde

bassLine genera el bajo para una sucesión de acordes, y composeBass usa a bassLine para poder 
generarlo a partir de una canción. No es necesario que composeBass le de duración a cada nota, porque el estilo requiere que sean todas negras.
\begin{verbatim}

> bassOverChord :: (RandomGen g) => g -> Chord -> Duration.T -> AbsNote -> ([AbsNote],g)
> bassOverChord g c d dest = walkingBass g (fromInteger(Duration.divide d Duration.qn)) 
>                                        (bassnote c) dest (chordNotes c (-2))
>
> bassLine :: (RandomGen g) => g -> [(Chord,Duration.T)] -> [AbsNote]		
> bassLine _ [] = []
> bassLine _ (x : [])     = [Pitch.toInt (-1,bassnote (fst x))]
> bassLine g (x : y : xs) = b ++ bassLine g' (y:xs)
>                           where (b,g') = bassOverChord g (fst x) (snd x) 
>                                                        (Pitch.toInt (-1,bassnote (fst y)))
>
> composeBass :: (RandomGen g) => g -> Song -> [AbsNote]	
> composeBass g s = bassLine g (seqChords s)

\end{verbatim}



