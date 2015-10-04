\section{ General }

El objetivo del módulo \textbf{General} es definir ciertos tipos comunes usados a lo largo del programa en los demás módulos, facilitando así la comprensión del código.

\begin{verbatim}

> module General where

\end{verbatim}

\subsection{Import}

\begin{verbatim}

> import qualified Haskore.Composition.Chord as Chord
> import qualified Haskore.Basic.Pitch as Pitch
> import qualified Haskore.Music as Music
> import qualified Haskore.Melody as Melody
> import qualified Haskore.Melody.Standard as Standard
> import qualified Haskore.Composition.ChordType as ChordType

\end{verbatim}

Importamos el módulo \textbf{Tipos} de HasBass.

\begin{verbatim}

> import qualified Tipos

\end{verbatim}

\subsection{Tipos generales}

Definición de los tipos generales usados en ChurchPiano.\\

Nota de la escala musical en cifrado americano.

\begin{verbatim}

> type Note = Pitch.Class

\end{verbatim}

RelNote (Nota Relativa) es un intervalo en semitonos, representado con un entero.

\begin{verbatim}

> type RelNote = Pitch.Relative

\end{verbatim}

Nota absoluta.

\begin{verbatim}

> type AbsNote = Pitch.Absolute

\end{verbatim}

Haskore define el tipo Chord.T como una lista de notas relativas.

Llamamos Chord Absoluto a una lista de notas absolutas.

\begin{verbatim}

> type AbsChord = [AbsNote]

\end{verbatim}

En el informe de HasBass dice:\\

\fbox{\parbox[b]{\linewidth}{ Para reprensentar los acordes se usan datos correspondientes a su notacion simbólica. La nota tónica, la nota más grave del acorde, y el tipo de acorde, representado con un tipo de Haskore. Por ejemplo : Am/G representa el acorde de tónica A, con bajo en G y tipo de acorde menor.}}\\


Y define en Tipos.lhs:\\

\fbox{\parbox[b]{\linewidth}{ data Chord = Cons \{ root :: Note, bassnote :: Note, chordType :: ChordType.T \} deriving (Eq,Show)}}\\

Entonces, usamos acordes como en HasBass y le agregamos duración.

\begin{verbatim}

> type Acorde = (Tipos.Chord,Music.Dur)

\end{verbatim}

Llamamos Acorde Absoluto a un AbsChord y su duración.

\begin{verbatim}

> type AbsAcorde = (AbsChord,Music.Dur)

\end{verbatim}

Trabajamos música con atributos standard.

\begin{verbatim}

> type Musica = Melody.T Standard.NoteAttributes

\end{verbatim}

Una Escala Relativa a una nota tónica es una lista de Notas Relativas.

\begin{verbatim}

> type EscalaRel = [RelNote]

\end{verbatim}

Una Escala Absoluta es una lista de Notas Absolutas.

\begin{verbatim}

> type EscalaAbs = [AbsNote]

\end{verbatim}

Para representar una Figura Rítmica nos interesa saber su duración y si es un Silencio o una Nota.

\begin{verbatim}

> data Figura = Silencio Music.Dur | Nota Music.Dur
>    deriving (Show,Eq)

\end{verbatim}

Una Frase Rítmica es una lista de Figuras.

\begin{verbatim}

> type Frase = [Figura]

\end{verbatim}

\subsection{Tipos de las Inputs}

Para el procesado de las entradas del programa definimos tres tipos.\\

Para la Armonía de la Mano Izquierda definimos un conjunto de correspondencia entre acordes y sus posibles formas de ejecución.

\begin{verbatim}

> type LHVInput = [([ChordType.T],[Chord.T])]

\end{verbatim}

Para las Escalas definimos un conjunto de correspondencia entre acordes y la escala que se le asocia.

\begin{verbatim}

> type ESCInput = [([ChordType.T],EscalaRel)]

\end{verbatim}

Para las Frases Rítmicas definimos un conjunto de las mismas.

\begin{verbatim}

> type FRAInput = [Frase]

\end{verbatim}
