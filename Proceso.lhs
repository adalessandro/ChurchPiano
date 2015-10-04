\section{ Proceso }

El módulo \textbf{Proceso} proporciona la función \textbf{proc} utilizada en \textbf{Main}, sirviéndole de interfaz con el resto de ChurchPiano, terminando de llevar la entrada a una forma interna.

\begin{verbatim}

> module Proceso where

\end{verbatim}

\subsection{Import}

\begin{verbatim}

> import Haskore.Music as Music
> import qualified Haskore.Basic.Duration  as Duration
> import qualified Haskore.Music.GeneralMIDI as MidiMusic
> import qualified Haskore.Interface.MIDI.Render as Render
> import qualified Haskore.Melody as Melody
> import qualified Haskore.Melody.Standard as Standard
> 
> import Medium.Controlled.List as List
>           (T(Primitive,Serial,Parallel,Control))
> import qualified Sound.MIDI.File as MidiFile
> import qualified Sound.MIDI.File.Save as SaveMidi
> import qualified Haskore.Interface.MIDI.Read as ReadMidi
> import qualified Haskore.Example.Miscellaneous as Miscellaneous
> import qualified Haskore.Music.Rhythmic as RhyMusic
> 
> import System.Random
> import qualified System.Environment as Environment
> 
> import General
> import qualified ArmoniaLHV
>
> import qualified Tipos
> import qualified Sonido

\end{verbatim} 

\subsection{Procesando la entrada}

La función \textbf{hacerMusica} toma directamente el archivo midi y lo transforma en algo del tipo Musica. En este proceso se quitan todos los atributos con \textbf{quitarAttr} ya que ChurchPiano trabajo sólo con atributos standard.
 
\begin{verbatim} 

> type MidiArrange = Miscellaneous.MidiArrange
> 
> quitarAttr :: MidiMusic.T -> Musica
> quitarAttr = fmap f
>                 where f (Atom dur x) = 
>                          case x of
>                               Nothing -> (Atom dur Nothing)
>                               Just note -> let t = RhyMusic.pitch $ RhyMusic.body note
>                                            in Atom dur (Just(Melody.Note Standard.na t))
> 
> hacerMusica :: MidiFile.T -> Musica
> hacerMusica t = quitarAttr $ third (ReadMidi.toGMMusic t :: MidiArrange)
>                    where third (_,_,x) = x

\end{verbatim} 

\subsection{Procesando la salida}

Como proceso inverso a \textbf{hacerMusica}, tenemos la función \textbf{hacerMidi} que toma algo del tipo Musica y lo devuelve en una estructura Midi.

\begin{verbatim} 

> hacerMidi :: Musica -> MidiMusic.T
> hacerMidi m = let m' = Music.transpose (-12) (Music.changeTempo (2) m) in
>                  MidiMusic.fromStdMelody MidiMusic.AcousticGrandPiano m'

\end{verbatim}

\textbf{saveToFile f m} renderiza el midi \textbf{m} y lo almacena en la ubicación \textbf{f}.

\begin{verbatim}

> saveToFile :: String -> MidiMusic.T -> IO ()
> saveToFile f m = SaveMidi.toFile f (Render.generalMidiDeflt m)

\end{verbatim}

\subsection{Juntando los procesos}

Para combinar con el Contrabajo de HasBass usamos \textbf{javierElBajo} que devuelve 3 repeticiones de la línea de Contrabajo concatenadas. 

\begin{verbatim}

> javierElBajo :: RandomGen g => g -> Tipos.Song -> Integer -> MidiMusic.T
> javierElBajo g s i = x +:+ x +:+ x
>                         where x = Sonido.process g s i

\end{verbatim}

La función principal del módulo es \textbf{proc}. Ésta toma todas las entradas del programa y almacena en disco (con \textbf{saveToFile}) el midi con la ejecución en simultáneo del ContraBajo y el Piano, combinando el resultado de \textbf{javierElBajo} y la función \textbf{principal} del módulo \textbf{ArmoniaLHV}.

\begin{verbatim}

> proc :: RandomGen g => g -> Tipos.Song -> Integer -> LHVInput -> 
>         ESCInput -> FRAInput -> MidiFile.T -> Int -> String -> IO ()
> proc gen s bpm lhv esc fra mel inf outF = 
>    saveToFile outF $ 
>       javierElBajo gen s bpm
>       =:=
>       ( hacerMidi $ Music.changeTempo (bpm Duration.%+ 60) aux )
>          where aux = ArmoniaLHV.principal gen lhv esc fra inf (hacerMusica mel) s
 
\end{verbatim}
