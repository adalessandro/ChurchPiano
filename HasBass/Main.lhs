\documentclass[10pt]{article}

\usepackage[a4paper, margin=3cm]{geometry}
\usepackage[spanish]{babel}
\selectlanguage{spanish}
\usepackage[utf8]{inputenc}




\newcommand{\STitle}{\texttt{HasBass}}

\title{\STitle}
\author{Javier M. Corti}

\begin{document}

\maketitle

\section{Introducción}

El propósito de este programa es generar un base para una sucesión de acordes, a un tempo dado.
De esta manera se obtine una pista de acompañiamiento ("backing track") que puede ser utilizada para la práctica de ejercicios, melodías y soleo sobre alguna progresión dada.

Para generar un acompañiamiento, ya hay una infinidad de programas que son capaces de componer música. El fuerte de HasBass es que recibe como entrada una lista de acordes, que es una vista más abstracta de una cancion que cada una de las notas que la componen. De esta manera, se puede producir un acompañiamiento rapidamente, sin preocuparse por los detalles de cada nota y su duraci\'{o}n correspondiente. La entrada del programa es casi una traducción directa del formato "leadsheet" de jazz. Es oportuno mencionar que se usarán los nombres de las notas 
y acordes del "cifrado americano", donde C = Do, D = Re..., G = Sol, A = La y B = Si, siguiendo el formato de los leadsheets. 

Una base completa está compuesta por ritmo y armonía (los acordes). 
En este caso, los acordes son suministrados por un sonido de piano. 
Se genera, además, una línea de bajo que depende del contexto armónico, pero además dota de 
ritmo a la base. El agregado de una batería daría mayor versatilidad rítmica, 
pero en este programa, la idea central radica en la sucesión de acordes, 
y el "swing" que proporciona la línea de bajo es un ritmo suficiente. 

\section{Instrucciones de Uso}

Se compila ejectuando el comando 

\begin{verbatim}
ghc Main.lhs
\end{verbatim}

El uso del programa es extemadamente simple. 
Solo recibe por línea de comandos el nombre del archivo de entrada y el de salida, 
y no más argumentos.
La sintaxis del archivo de entrada sigue la forma de los leadsheet tradicionales, 
y está dada por :

\begin{verbatim}

	input = tempo sucesion
	
	tempo = int
	
	sucesion = compas '|' sucesion | ''
	
	compas = acorde compas | ''
	
\end{verbatim}

Los Acordes están dados por :

\begin{verbatim}
	
	acorde = nota tipo | nota tipo '/' nota
	
	nota = notanat | notanat b | notanat #
	
	notanat = A | B | C | D | E | F | G

\end{verbatim}

Los tipos de acordes : 

\begin{verbatim}

"", "maj", "+", "aug", "-", "m", "m+", "m-", "0", "dim",
"sus2, "sus4", "4", "0+", "2", "6",
"7", M7", "Ma7", "9", "M9", "11", "13",
"3+", "-5", "5-", "+5", "5+", "-6", "6-", "7+", "-9", "+9", "+11"

\end{verbatim}

\section{Librerías}

\subsection{Haskore}

La librería más importante del programa es Haskore, que proporciona tanto los tipos de datos 
básicos como las funciones que permiten manipular música fácilmente. Haskore propone una 
especificación algebraica de la música ( en general representa "multimedia temporal")  
cuyas primitivas son la composición secuencial (:+:) y la composición paralela (:=:), dur 
la duración de un "temporal media" y none la ausencia de "temporal media" por una duración.
Fue utilizado el paquete haskore-0.2.0.2 .

\subsection {System.Random}
Esta librería es muy útil para simular improvisación mediante pseudo-aleatoridad. En este programa
se genera una secuencia pseudo aleatoria (gen) al inicio, y se la pasa a toda función que necesite
un valor aleatorio. Cada una de estas funciones debe ademas, retornar una nueva secuencia de donde
extraeran valores las funciones que se ejecuten despues. En este sentido la secuncia aleatoria se
trata como un estado en el programa.
Fue usado el paquete random-1.0.0.3 .

\subsection {System.Environment}

Esta libreria proporciona las funciones getArgs y getProgramName que le permiten al programa recibir
argumentos de linea de comandos y comunicarse con el SO. Incluida en el paquete base-4.3.1.0 .

\subsection {Parsec}

Se usa parsec para construir el parser del programa, ya que proporciona buenos mensajes de error.
Su uso en este programa no difiere demasiado al de la libreria de combinadores de parsers implementada 
en clase. Para correr el parser se usa la función parse, que da como resultado un tipo Either, donde
Left lleva el mensaje de error, en caso de un error en el parseo (Right contiene el resultado de un parseo exitoso). 
Se usa el paquete parsec-3.1.1 .

\subsection {ReadP}
Para usar la implementacion del parser que reconoce los tipos de acordes de Haskore, se usa el parser de tipo ReadP.
Cuando este parser falla, se invoca manualmente al fail de parsec. El ReadP esta incluida en el paquete base-4.3.1.0 .

\subsection {Control.Monad}
Cuando se quiere usar liftM o alguna otra función monadica, se importa esta librería, también incluida en el paquete base.

\section{Main}

Se extraen los nombres del archivo de entrada y el de salida de la línea de comandos y 
se obtiene una semilla para la sucesión pseudo-aleatoria. Luego de parsear 
y procesar el archivo de entrada, se almacena el resultado en el archivo de salida. 

\begin{verbatim}

> module Main where
> import Tipos
> import Sonido
> import System.Random
> import System.Environment
> import Text.ParserCombinators.Parsec
>
>
>
> main :: IO()
> main = do xs     <- getArgs
>           case (length xs) of
>               2 -> do let [f,g] = xs
>                       input <- readFile f 
>                       gen   <- newStdGen
>                       case (parse parseInput "" input) of
>                         Left err      -> do putStr "error de parseo en "
>                                             print err
>                         Right (s,bpm) -> saveToFile g (process gen s bpm)
>
>
>               _ -> do name <- getProgName 
>                       putStrLn("Uso : " ++ name ++ " inputFile outputFile")

\end{verbatim}

\input{Tipos.lhs}

\input{Basicas.lhs}

\input{Armonia.lhs}

\input{Bajo.lhs}

\input{Sonido.lhs}

\end{document}
