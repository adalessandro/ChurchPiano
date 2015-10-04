\section {Funciones Básicas}

\begin{verbatim}

> module Basicas(chordNotes) where
> import Tipos
> import qualified Haskore.Basic.Pitch  as Pitch
> import qualified Haskore.Composition.ChordType as ChordType

\end{verbatim}

chordNotes es una función fundamental para el programa ya que se mueve entre dos niveles 
de abstracción : Toma un acorde (elemento abstracto) y devuelve una lista de notas absolutas. 
Para esto se debe precisar en que octava se quiere desplegar el acorde,
asi que chordNotes también toma un entero. Para tener mayor cantidad de notas, 
el acorde se despliega sobre un rango de dos octavas, concatenando las notas de cada octava.

\begin{verbatim}

> chordNotes :: Chord -> Int -> [AbsNote]  
> chordNotes (Cons r _ ct) i = map (transpose (i,r)) (ChordType.toChord ct) ++ 
>                              map (transpose (i+1,r)) (ChordType.toChord ct)                              

\end{verbatim}

toChord es una funcion de ChordType que toma un ChordType.T y devuelve un Chord.T, 
que es una lista de los intervalos (en semitonos) que forman cada nota del acorde con la tónica r.
Es decir un acorde mayor en tipo Chord.T sería [0,4,7] (tónica, tercera mayor y quinta). 
Entonces para formar las notas concretas de un C mayor, 
voy a tener que transponer a la tónica (un C) la cantidad de semitonos indicada en Chord.T. 
Es decir, si elijo como tónica a C4, el acorde resultante sería [C4+0,C4+4,C4+7].

Para esto esta la funcion transpose, que relaciona una nota con una nota traspuesta una cantidad de i de semitonos.
Esta claro que para hacer la suma C4 + x, debo ver a C4 como un entero, por lo que se usa la función toInt (mapea cada tecla del piano a un entero).

\begin{verbatim}

> transpose :: Pitch.T -> Pitch.Relative -> AbsNote  
> transpose p i = Pitch.toInt p + i

\end{verbatim}
