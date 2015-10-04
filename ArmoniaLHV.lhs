\section{ ArmoniaLHV }

El módulo \textbf{ArmoniaLHV} proporciona la función \textbf{principal}, que arma toda la composición del piano en el Piano de Church.

\begin{verbatim}

> module ArmoniaLHV  where

\end{verbatim}

\subsection{Import}

\begin{verbatim}

> import Haskore.Composition.ChordType as ChordType
> import Haskore.Music as Music
> import Haskore.Basic.Duration as Duration
>           ((%+),fromRatio,divide)
> import qualified Haskore.Composition.Chord as Chord
> import qualified Haskore.Basic.Pitch as Pitch
> import qualified Haskore.Melody as Melody
> import qualified Haskore.Melody.Standard as Standard
>
> import Medium.Controlled.List as List
>           (T(Primitive,Serial,Parallel,Control))

> import System.Random
>
> import General
> import qualified Improvisacion
> 
> import qualified Tipos

\end{verbatim}

\subsection{Armando la armonía de mano izquierda}

En base al Input de la Armonía de la Mano Izquierda y dado un acorde, devolvemos una lista con las posibles formas de ejecución.

\begin{verbatim}

> sugerirLHV :: LHVInput -> ChordType.T -> [Chord.T]
> sugerirLHV [] ch = error "sugerirLHV: inputLHV no es exhaustivo."
> sugerirLHV (x:xs) ch = let (as,bs) = x
>                        in if null $ Prelude.filter (==ch) as
>                             then sugerirLHV xs ch
>                             else bs

\end{verbatim}

Transformamos las posibles formas de ejecución en acordes absolutos.

\begin{verbatim}

> armarLHV :: LHVInput -> Tipos.Chord -> [AbsChord]
> armarLHV inp (Tipos.Cons n b c) = map ( map (Pitch.classToInt n +) ) $ sugerirLHV inp c

\end{verbatim}

La función \textbf{acomodarLHV} transporta el AbsChord \textbf{xs} tal que: inf \texttt{<=} nota más baja del acorde \texttt{<} inf + 12

\begin{verbatim}

> acomodarLHV :: AbsNote -> AbsChord -> AbsChord
> acomodarLHV inf xs = let h = head xs
>                          d = (div (h-inf) 12) * 12
>                      in map (\n -> n - d) xs

\end{verbatim}

Definimos \textbf{mediaTonal} como el promedio simple de todas las notas absolutas del acorde.

\begin{verbatim}

> mediaTonal :: AbsChord -> Double
> mediaTonal xs = sum (map fromIntegral xs) / fromIntegral (length xs)

\end{verbatim}

Vamos a necesitar una función auxiliar que dada una lista, determine cuál es el par ordenado cuya segunda componente sea la menor entre todos los pares.

\begin{verbatim}

> menor2da :: Ord b => [(a,b)] -> (a,b)
> menor2da = foldr1 (\(a,b) (c,d) -> if b <= d then (a,b) else (c,d))

\end{verbatim}

Entonces, con las funciones antes definidas podemos armar las posibles ejecuciones de la armonía.

Con \textbf{elegirLHV} hacemos esto y elegimos la ejecución -es decir, el acorde absoluto- cuya media tonal es la más cercana a la dada en \textbf{m}.

\begin{verbatim}

> elegirLHV :: LHVInput -> AbsNote -> Double -> Acorde -> AbsAcorde
> elegirLHV inp inf m (c,dur) = let h = map (acomodarLHV inf) $ armarLHV inp c
>                                   f = (\n->abs(n-m)) . mediaTonal
>                               in (fst $ menor2da $ zip h $ map f h, dur)

\end{verbatim}

Finalmente, para que resulte agradable al oído, queremos que la transición entre la armonízación de dos acordes sea lo más "suave" posible. Esto lo modelamos escogiendo la menor diferencia de media tonal.

\begin{verbatim}

> armonizarLHV :: LHVInput -> AbsNote -> [Acorde] -> [AbsAcorde]
> armonizarLHV inp inf = armonizarLHV' inp inf 0
> 
> armonizarLHV' :: LHVInput -> AbsNote -> Double -> [Acorde] -> [AbsAcorde]
> armonizarLHV' inp inf n [] = []
> armonizarLHV' inp inf n (x:xs) = c : armonizarLHV' inp inf m xs
>                                     where (c,m) = (elegirLHV inp inf n x, mediaTonal $ fst c)

\end{verbatim}

\subsection{Evitando superposiciones}

Antes de juntar la armonía de la mano izquierda con la melodía debemos asegurarnos que las mismas no se superpongan. Al momento de ejecutar una acorde la melodía debe encontrarse por sobre el mismo, nunca por debajo, como indica Mark Levine "...just make sure that the melody note is on top, where the melody belongs." Para esto vamos transportar la melodía tantas octavas hacia arriba como sea necesario para lograr nuestro objetivo.

La función \textbf{calcularDur} toma una grilla de acordes del tipo Tipos.Song utilizado en HasBass, y la transforma en una lista de acordes con sus respectivas duraciones, es decir, del tipo Acorde.

\begin{verbatim}

> calcularDur :: Tipos.Song -> [Acorde]
> calcularDur = concat . map ( \x -> ponerDur (2 %+ toInteger (length x)) x )
>                  where ponerDur dur = map (\x -> (x,dur))

\end{verbatim}

Dada una lista de acordes, obtenemos un lista con las posiciones (distancia del inicio) de cada acorde.

\begin{verbatim}

> armoniaPosiciones :: [AbsAcorde] -> [Music.Dur]
> armoniaPosiciones = armoniaPosiciones' 0
> 
> armoniaPosiciones' :: Dur -> [AbsAcorde] -> [Dur]
> armoniaPosiciones' n [x] = [n]
> armoniaPosiciones' n ((_,dur):xs) = n:(armoniaPosiciones' (n+dur) xs)

\end{verbatim}

La función \textbf{queSuena dist m} devuelve una lista con las notas que suenan a una distancia \textbf{dist} en \textbf{m}.

\begin{verbatim}

> -- TO DO: optimizarla para que tome [Music.Dur] y devuelva el queSuena en cada Music.Dur
> queSuena :: Music.Dur -> Musica -> [Pitch.Absolute]
> queSuena dist (Primitive (Atom dur x)) =
>       if dur > dist
>        then case x of
>                  Nothing -> []
>                  Just note -> [Pitch.toInt $ Melody.notePitch_ note]
>        else []
> queSuena dist (Serial xs) = 
>       case xs of
>            [] -> []
>            (y:ys) -> let d = Music.dur y in
>                      if d > dist then queSuena dist y else queSuena (dist - d) (Serial ys)
> queSuena dist (Parallel xs) = 
>       concat $ map (queSuena dist) xs
> queSuena dist (Control cont x) = 
>       case cont of
>            Tempo ratio -> queSuena (dist * ratio) x
>            Transpose relat -> map (relat+) $ queSuena dist x

\end{verbatim}

Una vez que obtenemos la lista de notas que suena en cada ejecución de un acorde, buscamos cuál es el menor intervalo existente entre estos. Consideramos los intervalos descendentes como negativos, y por lo tanto menores que los ascendentes.

La función \textbf{distMin} devuelve Nothing en caso de que no hubiera ninguna nota de la melodía que suene al momento de ejecutar un acorde; en caso contrario retorna (Just n), siendo n la mínima distancia.

\begin{verbatim}

> distMin :: [(AbsAcorde,[Pitch.Absolute])] -> Maybe Int
> distMin ls = foldr f Nothing $ map distMin' ls
>                 where f = (\x y -> case (x,y) of
>                                         (Nothing,y) -> y
>                                         (x,Nothing) -> x
>                                         (Just a, Just b) -> Just (min a b) )
>
> distMin' :: (AbsAcorde,[Pitch.Absolute]) -> Maybe Int
> distMin' (a,p) = if p == [] then Nothing else Just (minimum p - maximum (fst a))

\end{verbatim}

Juntamos todo en \textbf{comparar}, que toma una lista de acordes con duración y una melodía devolviendo cuántas octavas se debe alzar la melodía para quedar disjunta de los acordes.

\begin{verbatim}

> comparar :: [AbsAcorde] -> Musica -> Int
> comparar as m = case distMin $ zip as $ map (\p -> queSuena p m) $ armoniaPosiciones as of
>                      Nothing -> 0
>                      Just n -> if n <= 0 then -(div n 12) else 0

\end{verbatim}

\subsection{Algunas funciones útiles y necesarias}

Pasaje de un acorde a música de Haskore con atributos standares.

\begin{verbatim}

> aco2mus :: AbsAcorde -> Musica
> aco2mus (xs,dur)= chord $ map (\t -> Melody.note t dur Standard.na) $ map Pitch.fromInt xs

\end{verbatim}

Transportamos todo n octavas.

\begin{verbatim}

> subirOcts :: Int -> Musica -> Musica
> subirOcts n = Music.transpose (n*12)

\end{verbatim}

Modificamos la Dinámica para que al volumen se le aplique un factor de (2/3).

\begin{verbatim}

> atenuar :: Musica -> Musica
> atenuar m = Music.accent (0) $ Music.loudness1 (2/3) m

\end{verbatim}

\subsection{Juntando el Piano de Church - Principal}

La función \textbf{principal} arma la música final del Piano, compuesta por 3 partes: acordes más melodía, acordes más improvisación, acordes más melodía.

Los parámetros son: \textbf{g} generador de aleatoriedad; \textbf{lhv esc fra} inputs de armonía escalas y fraseos respectivamente; \textbf{inf} nota cota inferior; \textbf{m} melodía del standard de jazz; \textbf{s} armonía del standard de jazz.

Para la parte de la improvisación se llama a la función \textbf{imp0} que es la más importante del módulo \textbf{Improvisacion}.

\begin{verbatim}

> principal :: RandomGen g => g -> LHVInput -> ESCInput -> FRAInput -> 
>              AbsNote -> Musica -> Tipos.Song -> Musica
> principal g lhv esc fra inf m s = let acor = calcularDur s
>                                       armo = armonizarLHV lhv inf acor 
>                                       m' = subirOcts (comparar armo m) m
>                                       imp = Improvisacion.imp0 g esc fra (inf+24) acor
>                                       izq = atenuar (line $ map aco2mus armo)
>                                   in (izq =:= m') +:+
>                                      (izq =:= imp) +:+
>                                      (izq =:= m')

\end{verbatim}
