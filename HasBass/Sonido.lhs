\section{Haciendo que suene}

\begin{verbatim}

> module Sonido(process,saveToFile) where
> import Tipos
> import Armonia
> import Bajo
> import System.Random
> import qualified Haskore.Basic.Pitch  as Pitch
> import qualified Haskore.Basic.Duration  as Duration
> import Ratio
> import qualified Haskore.Music.GeneralMIDI as MidiMusic
> import qualified Haskore.Melody         as Melody
> import qualified Haskore.Music          as Music
> import qualified Sound.MIDI.File.Save    as SaveMidi
> import qualified Haskore.Interface.MIDI.Render       as Render

\end{verbatim}

Hasta ahora tenemos una idea de como obtener las notas absolutas que formaran parte de la salida. 
Pero para Haskore una lista de notas no es Música, 
asi que se debe generar algo de tipo Melody.T antes de poder pasar a un formato 
MIDI de salida (MidiMusic.T) . Mucho del código de esta sección fue sacado de la parte de ejemplos (Haskore.Example.Miscellaneous), 
para obtener música directamente de los datos ya obtenidos.

Primero se genera música a partir de una nota, luego a partir de un acorde, y despues a partir de 
una sucesión de acordes, fijando el tempo :

\begin{verbatim}

> makeNote :: Duration.T -> AbsNote -> Melody.T ()
> makeNote d p = Melody.note' n o d ()
>               where (o,n) = Pitch.fromInt p

> makePChord :: ([AbsNote],Duration.T) -> Melody.T ()
> makePChord (xs,d) = Music.chord (map (makeNote d) xs)

> acompiano :: [([AbsNote],Duration.T)] -> Integer -> Melody.T ()
> acompiano xs t = Music.changeTempo (t Duration.%+ 60) (Music.line (map makePChord xs))

\end{verbatim}

Finalmente, se genera lo pasa a formato MIDI :

\begin{verbatim}

> pianoFromMusic :: Melody.T () -> MidiMusic.T
> pianoFromMusic = MidiMusic.fromMelodyNullAttr MidiMusic.AcousticGrandPiano

\end{verbatim}

Para generar la musica del bajo se usan funciones similares, 
notar que Music.line secuencializa la musica que se va generando, mientras que 
Music.chord que se uso en el piano la paraleliza.

\begin{verbatim}

> musicBass :: [AbsNote] -> Integer -> Melody.T ()
> musicBass xs t = Music.changeTempo (t Duration.%+ 60)  (Music.line ((map (makeNote Duration.qn) (init xs)) ++ 
>                                                                     (map (makeNote Duration.wn) [last xs])))
>
> bassFromMelody :: Melody.T () -> MidiMusic.T
> bassFromMelody = MidiMusic.fromMelodyNullAttr MidiMusic.AcousticBass

\end{verbatim}

La función final, process, junta todo, y usa el operador de composición paralela de Haskore, para que el piano y el bajo suenen simultáneamente.

\begin{verbatim}

> process :: (RandomGen g) => g -> Song -> Integer -> MidiMusic.T
> process g s t = bassFromMelody (musicBass (composeBass g s) t) 
>	--Music.=:=
>	--	pianoFromMusic (acompiano (harmony (seqChords s) ) t)
>
> saveToFile :: String -> MidiMusic.T -> IO ()
> saveToFile f m = SaveMidi.toFile f (Render.generalMidiDeflt m)

\end{verbatim}




