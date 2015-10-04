\documentclass[10pt]{article}

%include lhs2TeX.fmt
%include lhs2TeX.sty

\usepackage[a4paper, margin=2cm]{geometry}
\usepackage[spanish]{babel}
\usepackage{verbatim}
\selectlanguage{spanish}
\usepackage[utf8]{inputenc}

\title{\texttt{ChurchPiano}}

\author{Ariel R. D'Alessandro}

\begin{document}

\maketitle

\section{Introducción}

El programa \textbf{ChurchPiano} -también llamado El Piano de Church- tiene por objetivo simular la ejecución de un Piano sobre un Standard de Jazz. A su vez, fue diseñado para complementarse con el Contrabajo de \textbf{HasBass}, trabajo final ALPII 2011 de Javier M. Corti.

\subsection{Compilación}

Para compilar ejecutamos en una consola:

\begin{verbatim}
ghc Main.lhs -iHasBass
\end{verbatim}

La bandera -iHasBass agrega el subdirectorio ./HasBass/ (ubicado en el actual directorio) a la lista de búsqueda de módulos. De esta manera podemos importar los módulos de HasBass directamente en nuestro programa.

\subsection{Ejecución}

El usuario deberá pasar por línea de comandos el nombre del archivo de parámetros de entrada, como en el ejemplo:

\begin{verbatim}
./Main Parametros/InfantEyes.in
\end{verbatim}

Un archivo de parámetros de entrada se estructura de la siguiente manera.

Veamos \textbf{./Parametros/InfantEyes.in}:

\verbatiminput{./Parametros/InfantEyes.in}

\subsection{Inputs}

\subsubsection{InputMelodia}

Ruta del archivo midi que contiene la melodía del Standard de Jazz. El midi de entrada debe consistir de una pista individual y debe tener la misma longitud que el input de armonía.

\subsubsection{InputArmonia}

Ruta del archivo que contiene el tempo y la sucesión de acordes del Standard de Jazz. Este archivo es igual al que se utiliza en HasBass como entrada, por lo que a continuación se transcribe parte de su informe.\\

La sintaxis del archivo de entrada sigue la forma de los leadsheet tradicionales, y está dada por:

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

Veamos \textbf{./Midis/InfantEyes.co}:

\verbatiminput{./Midis/InfantEyes.co}

\subsubsection{OutputFile}

Ruta del archivo midi de salida.

\subsubsection{Inf}

Nota cota inferior. Todo lo que toque el piano se encontrará por sobre (más agudo) que esta nota.

Inf está dado por:

\begin{verbatim}
        Inf = '(' Int ',' Nota ')'
        Nota = Notanat | Notanat 'b' | Notanat '#'
        Notanat = 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G'
\end{verbatim}

\subsubsection{InputLHV}

Ruta del archivo que determina cómo se construye la armonía de la mano izquierda.

Veamos \textbf{./Inputs/InputLHV.in}:

\verbatiminput{./Inputs/InputLHV.in}

InputLHV está dado por:

\begin{verbatim}
        InputLHV = LHVLinea InputLHV | ''
        LHVLinea = LHVIzq '->' LHVDer ( '#' Comentario '\n' | '\n' )
        LHVIzq = tipo | tipo ',' LHVIzq
        LHVDer = Lista | Lista LHVDer
\end{verbatim}

\subsubsection{InputESC}

Ruta del archivo que determina las escalas utilizadas en la improvisación.

\begin{verbatim}
        InputESC = ESCLinea InputESC | ''
        ESCLinea = ESCIzq '->' ESCDer ( '#' Comentario '\n' | '\n' )
        ESCIzq = LHVIzq
        ESCDer = Lista
\end{verbatim}

Veamos \textbf{./Inputs/InputESC.in}:

\verbatiminput{./Inputs/InputESC.in}

\subsubsection{InputFRA}

Ruta del archivo que determina las posibles frases rítmicas utilizadas en la improvisación.

Las figuras rítmicas son representadas de la siguiente manera: b = redonda, w = blanca, h = negra, q = corchea, e = semicorchea, s = fusa, t = semifusa.

\begin{verbatim}
        InputFRA = FRALinea InputFRA | ''
        FRALinea = Figura ( 'r' | '' ) FRALinea | Tresillo FRALinea | '\n'
        Figura = 'b' | 'w' | 'h' | 'q' | 'e' | 's' | 't'
        Tresillo = '(' Figura Figura Figura ')3'
\end{verbatim}

Veamos \textbf{./Inputs/InputFRA.in}:

\verbatiminput{./Inputs/InputFRA.in}

\subsection{Outputs}

El programa procesará las entradas y devolverá escribirá en OutputFile el midi de salida, que constará de tres partes:

\begin{enumerate}
\item Melodía + Acompañamiento.
\item Improvisación + Acompañamiento.
\item Melodía + Acompañamiento.
\end{enumerate}

Además, en todo momento el Piano estará acompañado por el Contrabajo producido por HasBass.

\section{Librerías}

\subsection{Sound}

Fue usado el paquete \textbf{sound-0.1.7.1} .

Con esta librería cargamos y guardamos los archivos midi, permitiendo así que el programa lea y pueda manipular este tipo de estructuras.

\subsection{Haskore}

Fue utilizado el paquete \textbf{haskore-0.2.0.2} .

Esta es la librería más importante del programa, creada por Paul Hudak <paul.hudak@yale.edu>, Henning Thielemann.

Proporciona tanto los tipos de datos básicos como las funciones que permiten manipular música fácilmente, proponiendo especificación algebraica de la música cuyas primitivas son la composición secuencial (:+:) y la composición paralela (:=:).

En ChurchPiano utilizamos esta librería para realizar toda "creación" de música y su manipulación, generando finalmente la estructura midi de salida que el paquete \textbf{Sound} se encarga de renderizar y guardar en disco.

\subsection {System.Random}

Fue usado el paquete \textbf{random-1.0.0.3} . 

Esta librería es muy útil para simular improvisación mediante pseudo-aleatoridad. En este programa se genera una secuencia pseudo aleatoria (gen) al inicio que será utilizada en las funciones que requieron aleatoriedad. A su vez, estas funciones retornan una nueva secuencia pseudo aleatoria para las subsiguientes funciones del programa.

\subsection {System.Environment}

La librería está incluída en el paquete \textbf{base-4.3.1.0} .

Proporciona las funciones getArgs y getProgramName que le permiten al programa recibir argumentos por línea de comandos y comunicarse con el SO.

\subsection {Parsec}

Se usa el paquete \textbf{parsec-3.1.1} .

La librería se usa para construir los distintos parsers del programa. Su uso en este programa no difiere demasiado al que se le podría haber dado a la libreria de combinadores de parsers implementada en clase.

Para correr el parser se usa la función parse, que da como resultado un tipo Either. Left lleva el mensaje de error en caso de un error en el parseo, y en caso exitoso Right contiene el resultado del parseo.

\subsection {Control.Monad}

También está incluida en el paquete \textbf{base-4.3.1.0}.

La utilizamos duando se requieren algunas funciones monádicas, como liftM.

\newpage

\input{General.lhs}

\newpage

\input{Parseadores.lhs}

\newpage

\input{Main.lhs}

\newpage

\input{Proceso.lhs}

\newpage

\input{ArmoniaLHV.lhs}

\newpage

\input{Improvisacion.lhs}

\end{document}

