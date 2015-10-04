\section{ Improvisacion }

El módulo \textbf{Improvisacion} aporta la función \textbf{imp0} simulando una pseudo-improvisación de un piano de jazz.

\begin{verbatim}

> module Improvisacion where

\end{verbatim}

\subsection{Import}

\begin{verbatim}
 
> import Haskore.Music as Music
> import qualified Haskore.Composition.ChordType as ChordType
> import qualified Haskore.Melody as Melody
> import qualified Haskore.Composition.Chord as Chord
> import qualified Haskore.Basic.Pitch as Pitch
> import Haskore.Basic.Duration as Duration
>           ((%+),fromRatio,divide)
> import qualified Haskore.Melody.Standard as Standard
>
> import System.Random
>
> import qualified Tipos
> import General

\end{verbatim}

\subsection{Armando las escalas}

En base al Input de Escalas y dado un acorde, devolvemos la Escala Relativa asociada.

\begin{verbatim}

> escala :: ESCInput -> ChordType.T -> EscalaRel
> escala [] ch = error "escala: inputESC no es exhaustivo"
> escala (x:xs) ch = let (as,b) = x
>                    in if null $ Prelude.filter (==ch) as
>                         then escala xs ch
>                         else b

\end{verbatim}

Armamos la escala absoluta partiendo de la tónica del acorde. Además la extendemos sobre la octava inferior y superior, y filtramos las notas inferiores a \textbf{inf}.

\begin{verbatim}

> hacerEscala :: ESCInput -> AbsNote -> Tipos.Chord -> EscalaAbs
> hacerEscala inp inf (Tipos.Cons n b c) = Prelude.filter func $ 
>                                          map (inf + Pitch.classToInt n +) $ 
>                                          map (\n->n-12) esc ++ esc ++ map (12+) esc
>                                             where esc = escala inp c
>                                                   func = ( \x -> (x < inf) || (x >= inf ) )
>
> ac2es :: ESCInput -> AbsNote -> Acorde -> (EscalaAbs,Dur)
> ac2es inp inf (c,d) = (hacerEscala inp inf c, d)
 
\end{verbatim}

\subsection{Armando las frases}

Una función auxiliar para saber la duración de una Figura.

\begin{verbatim}

> durFi :: Figura -> Dur
> durFi (Silencio d) = d
> durFi (Nota d) = d

\end{verbatim}

Armamos una frase combinando aleatoriamente las frases del input.

\begin{verbatim}

> -- TO DO: evitar el peligro de terminar la improvisacion en medio de la frase
> armarFrase :: RandomGen g => g -> FRAInput -> Frase
> armarFrase g fra = let (f,ng) = oneof g fra
>                    in f ++ armarFrase ng fra

\end{verbatim}

\textbf{ponerAc f e} toma las figuras de \textbf{f} y las va asociando a la escala correspondiente al momento de su ejecución.

De esta manera arma una lista, que sería equivalente a una gran frase rítmica donde se indica a qué escala pertenece cada figura de la misma.

La función pide que la frase tenga una longitud mayor igual que la armonía sobre la que se va a improvisar.

\begin{verbatim}

> ponerAc :: Frase -> [(EscalaAbs,Dur)] -> [(Figura, EscalaAbs)]
> ponerAc = ponerAc' 0
> 
> ponerAc' :: Dur -> Frase -> [(EscalaAbs,Dur)] -> [(Figura, EscalaAbs)]
> ponerAc' _ [] _ = error "ponerAc: Frase demasiado corta\n"
> ponerAc' _ _ [] = []
> ponerAc' acum (f:fs) ((e,d):as) = let df = durFi f
>                                   in if acum < d
>                                        then (f,e) : ponerAc' (acum + df) fs ((e,d):as)
>                                        else ponerAc' (acum - d) (f:fs) as

\end{verbatim}

\subsection{Funciones auxiliares}

Funciones auxiliares para trabajar sobre listas.

\begin{verbatim}

> primeras2 :: [a] -> [a]
> primeras2 [] = []
> primeras2 (x:[]) = [x]
> primeras2 (z:y:xs) = [z,y]
>
> ultimas2 :: [a] -> [a]
> ultimas2 = primeras2 . Prelude.reverse

\end{verbatim}

La función \textbf{oneof} de Javier Corti elige un elemento al azar de una lista.

\begin{verbatim}

> oneof :: (RandomGen g) => g -> [a] -> (a,g)
> oneof g xs = (xs !! ri,rg)
>                  where (ri,rg) = randomR (0,length xs - 1) g

\end{verbatim}

La función \textbf{cerca} toma una nota y una escala, devolviendo (en el caso de que existan) las 2 notas inmediatas inferiores y 2 inmediatas superiores dentro de la escala.

\begin{verbatim}

> cerca :: AbsNote -> EscalaAbs -> [Pitch.Absolute]
> cerca n e = let (lo,hi) = (Prelude.filter (<n) e, Prelude.filter(>n) e)
>             in ultimas2 lo ++ primeras2 hi

\end{verbatim}

\subsection{Pseudo-Improvisando}

La función principal del módulo es \textbf{imp0}. Comenzaremos por \textbf{imp3} e iremos subiendo hasta llegar a \textbf{imp0}.\\

En base a una nota dada, elegimos otra nota cercana dentro de la escala.

\begin{verbatim}

> imp3 :: RandomGen g => g -> AbsNote -> EscalaAbs -> (AbsNote,g)
> imp3 g n e = oneof g (cerca n e)
> 

\end{verbatim}

Definamos \textbf{imp2 g dur n e = (nn, m, ng)}. Siendo \textbf{nn} la nota obtenida hecha música, \textbf{m} es la misma nota como nota absoluta, \textbf{ng} es el nuevo generador de aleatoriedad.

La nota \textbf{nn} se elige en base a \textbf{n}, pasada como argumento, sobre la escala \textbf{e} ejecutando \textbf{imp3}.

\begin{verbatim}

> imp2 :: RandomGen g => g -> Dur -> AbsNote -> EscalaAbs -> (Musica, AbsNote, g)
> imp2 g dur n e = let (m,ng) = imp3 g n e
>                  in (Melody.note (Pitch.fromInt m) dur Standard.na, m, ng)
> 

\end{verbatim}

Con \textbf{imp1} vamos consumiendo las figuras, ejecutando reiteradas veces imp2, haciendo que la salida de una ejecución sea la entrada de la siguiente.

\begin{verbatim}

> imp1 :: RandomGen g => g -> AbsNote -> [(Figura, EscalaAbs)] -> Musica
> imp1 g n [x] = case x of
>                     (Silencio d, e) -> rest d
>                     (Nota d, e) -> f
>                           where (f,_,_) = imp2 g d n e
> imp1 g n (x:xs) = case x of
>                     (Silencio d, e) -> rest d +:+ imp1 g n xs
>                     (Nota d, e) -> f +:+ imp1 ng m xs
>                           where (f,m,ng) = imp2 g d n e

\end{verbatim}

Finalmente, en base a las inputs \textbf{esc} y \textbf{fra}, generamos una improvisación (música) armónicamente sobre \textbf{acs} cuyas notas son más altas que \textbf{inf}.

\begin{verbatim}

> imp0 :: RandomGen g => g -> ESCInput -> FRAInput -> AbsNote -> [Acorde] -> Musica
> imp0 g esc fra inf acs = imp1 g inf (ponerAc (armarFrase g fra) (map (ac2es esc inf) acs))

\end{verbatim}

