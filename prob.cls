% complementosMatematicos.cls
% Authors: Dante Zanarini, Taihú Pire
% Version: 0.2 2019/08/24
% Modificó: Gabina

% Esta es una clase para producir practicas y examenes
% de la catedra de Complementos Matemaicos I de la carrera LCC, FCEIA (UNR).

% Esta clase incluye a la clase estandar article.

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{complementosMatematicos}[2018/09/19 Clase para documentos de Complementos Matemaicos I]

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax
\LoadClass[10pt, a4paper]{article}

\RequirePackage{fancyhdr}
\RequirePackage{graphicx}
\RequirePackage{a4wide}
\RequirePackage{listings}
\RequirePackage{amssymb}
\RequirePackage{enumerate}
\RequirePackage{ulem}

\renewcommand{\figurename}{Figura}
\renewcommand{\tablename}{Tabla}
\renewcommand{\theenumi}{\bf\alph{enumi}}
\renewcommand{\theenumii}{\roman{enumii}}
\renewcommand{\labelenumi}{\theenumi)}
\renewcommand{\labelenumii}{\theenumii.}

\def\today{\number\day/\number\month/\number\year}
\setlength{\parindent}{0pt}
\renewcommand{\footrulewidth}{0.4pt}
\addtolength{\textheight}{-40pt}
\addtolength{\headheight}{22pt}

%%%%%%%%%%%%%% entornos, problema y problema*
\newcounter{problema}
\newenvironment{problema}{%
\vspace{2pt}\par\stepcounter{problema}\textbf{\arabic{problema}.\hspace{1ex}}}%
{\vspace{2pt}\par}


\newenvironment{problema*}{%
\vspace{2pt}\par\stepcounter{problema}\textbf{\arabic{problema}*.\hspace{1ex}}}%
{\vspace{2pt}\par}

%%%%%%%%%%%%%%%%%% para items dentro de problemas, parte
\newcounter{parte}[problema]
\newenvironment{parte}{\vspace{2pt}\par\hspace{2ex}\stepcounter{parte}\textbf{\alph{parte})\hspace{1ex}}}{\vspace{2pt}\par}


\def\logofile{}
\newif\iflogo
\newif\ifdatos
\logofalse
\datosfalse

\def\datosalumno{Nombre y Apellido:\hspace{200pt}Legajo:\vspace{2pt}\hrule\medskip}
\newcommand{\logo}[1]{\def\logofile{#1}\logotrue}

\title{}
\newcommand{\parcial}{%
	\title{Examen Parcial}
	\datostrue
}
\newcommand{\final}{%
	\title{Examen Final}
	\datostrue
}

\newcommand{\practica}[1]{%
	\title{Pr\'actica #1}
	\date{}
	\datosfalse
}


\newcommand{\practicalab}[1]{%
	\title{Pr\'actica de Laboratorio #1}
	\date{}
	\datosfalse
}

\newcommand{\apunte}[1]{%
	\title{Apunte de Clases #1}
	\date{}
	\datosfalse
}

\newcommand{\recuperatorio}{%
	\title{Examen Recuperatorio}
	\datostrue
}

\AtBeginDocument{\makeheadfoot}

\newcommand{\makeheadfoot}{%
\fancypagestyle{plain}{%
\iflogo%
	\lhead{\begin{minipage}{3cm}%
	\includegraphics[width=2cm,height=2cm]{\logofile}\vspace{2pt}\end{minipage}}%
	\chead{}%
	\rhead{\parbox{10cm}{%
    	\textsc{Facultad de Ciencias Exactas, Ingenier\'ia y Agrimensura\\
    	Escuela de Ciencias Exactas y Naturales\\
%     	Departamento de Ciencias de la Computaci\'on\\
    	Probabilidad y Estad\'istica}}}%
\else%
	\lhead{\parbox{10cm}{%
    	\textsc{Facultad de Ciencias Exactas, Ingenier\'ia y Agrimensura\\
    	Escuela de Ciencias Exactas y Naturales\\
%     	Departamento de Ciencias de la Computaci\'on\\
    	Probabilidad y Estad\'istica}}}%
   	\chead{}\rhead{\begin{minipage}{3cm}\vspace{2cm}\end{minipage}}%
\fi%
\lfoot{\small \@title}%
\cfoot{\small \@date}%
\rfoot{\small P\'agina \thepage}}%
\pagestyle{fancy}{%
    \lhead{\small Probabilidad y Estad\'istica} \chead{}%
    \rhead{}%
    \lfoot{\small \@title}%
    \cfoot{\small \@date}%
    \rfoot{\small P\'agina \thepage}}
}


\lstnewenvironment{imperativo}
    {\lstset{
     language=C,
      mathescape=true,
		 tabsize=2,
	identifierstyle=,
	commentstyle=\color{black},
	stringstyle=\ttfamily,
	showstringspaces=false}
 }
 {}

\lstnewenvironment{funcode}
   {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
      basicstyle=\ttfamily,
      flexiblecolumns=false,
      basewidth={0.5em,0.45em},
      literate={-}{{$-$}}4 {+}{{$+$}}4 {/}{{$/$}}2 {*}{{$*$}}2
               {>}{{$>$}}4 {<}{{$<$}}4 %{\\}{{$\lambda$}}1
               {->}{{$\rightarrow$}}3 {>=}{{$\geq$}}3 {<-}{{$\leftarrow$}}2
               {<=}{{$\leq$}}3 {=>}{{$\Rightarrow$}}2
               {\ .}{{$\cdot$}}3 {\ .\ }{{$\cdot$}}4
               {>>}{{>>}}2 {>>=}{{>>=}}2
               {|}{{$\mid$}}1 {++}{{$+\!+$}}4
    }
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PARA PYTHON
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some definitions to use color
% Default fixed font does not support bold face
\DeclareFixedFont{\ttb}{T1}{txtt}{bx}{n}{9} % for bold
\DeclareFixedFont{\ttm}{T1}{txtt}{m}{n}{9}  % for normal

% Custom colors
\usepackage{color}
\definecolor{deepblue}{rgb}{0,0,0.5}
\definecolor{deepred}{rgb}{0.6,0,0}
\definecolor{deepgreen}{rgb}{0,0.5,0}

\lstnewenvironment{pythoncode}
{\lstset{}%
	\csname lst@SetFirstLabel\endcsname}
{\csname lst@SaveFirstLabel\endcsname}
	\lstset{language=Python,
		basicstyle=\footnotesize,
		otherkeywords={self},
		tabsize=1,
		keywordstyle=\ttb\color{deepblue},
		emph={__init__, update_port},
		emphstyle=\ttb\color{deepred},
		stringstyle=\ttb\color{deepgreen},
		frame=single,
		showstringspaces=false,
		float=htpb,
		numbers=left,
		captionpos=b,
		numbersep=5pt,
		linewidth=0.95\linewidth,
		xleftmargin=0.05\linewidth,
		breaklines=true
		breakwhitespace=false}
	
	
\lstnewenvironment{bash}%
{\lstset{}%
	\csname lst@SetFirstLabel\endcsname}
{\csname lst@SaveFirstLabel\endcsname}
	\lstset{language=bash,
		keywordstyle=\sffamily\ttm,
		basicstyle=\sffamily\ttm,
		numbersep=5pt,
		frame=tb,
		columns=fullflexible,
		backgroundcolor=\color{yellow},
		linewidth=0.95\linewidth,
		xleftmargin=0.05\linewidth,
		breaklines=true,
		captionpos=b}
       
\def\maketitle{%
	\thispagestyle{plain}%
	\ifdatos\datosalumno\else\hspace{200pt}\vspace{2pt}\fi%
	\begin{center}%
		\Large\bf \@title%
	\end{center}%
}
