%%---------------------------------------------------
%% BASED ON: http://netcommons.eu/
%% Edited by David Palma, SEDESOLab: https://sinet.item.ntnu.no
%%---------------------------------------------------
%\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{SEDESOLab}
\LoadClass[a4paper,twoside,final,12pt,headings=small]{scrreprt}
% --------------------------------------------
% Load packages
% --------------------------------------------
\usepackage[utf8]{inputenc}
\usepackage{xspace}
\usepackage[spanish,mexico]{babel}
\usepackage{url}
\usepackage{latexsym}
\usepackage{enumerate}
\usepackage{rotating}
\usepackage{framed}
\usepackage{multicol}
\usepackage{fancybox}
%REMOVED \usepackage{fancyhdr}
\usepackage[automark,headsepline]{scrlayer-scrpage}
%\usepackage{fancyvrb}
\usepackage{scrhack}
\usepackage{float}
\usepackage{times}
\usepackage{array}
\usepackage{booktabs}
\usepackage{amsmath,amsthm, amsfonts, amssymb, amsxtra,amsopn}
\usepackage{setspace}
\usepackage{multirow}
\usepackage{nccmath}
\usepackage{tikz}
\usepackage[center,bf]{caption}
\usepackage{lastpage}
\usepackage{longtable}
\usepackage{xcolor}
\usepackage{listings}
\lstset{basicstyle=\ttfamily,
  showstringspaces=false,
  commentstyle=\color{red},
  keywordstyle=\color{blue}
}
\usetikzlibrary{calc}
% Color Links
\usepackage[pdftex,hyperfigures,hyperindex,bookmarksnumbered,colorlinks, bookmarks, breaklinks, linktocpage,citecolor=blue,urlcolor=blue,linkcolor=black,pdftitle={SEDESOLab},pdfauthor={SEDESOLab}]{hyperref}
\usepackage{graphicx}
\usepackage[a4paper,margin=2cm,heightrounded]{geometry}
\graphicspath{{images/}}
\DeclareGraphicsExtensions{.pdf,.png,.jpg}
\renewcommand{\UrlFont}{\scriptsize}

%%--Set the section number depth.
\setcounter{secnumdepth}{3}
\setcounter{tocdepth}{4}
\setcounter{page}{2}
\setstretch{1.7}

\bibliographystyle{plain}
\sloppy
%%--------------------------------------------
%%-- Definition and Quotation commands.
%%--------------------------------------------
\newtheorem{dbdefine}{Definition}[section]
\newtheorem{dbtheorem}{Theorem}[section]
\newtheorem{dblemma}{Lemma}[section]
\newtheorem{dbnotate}{Notation}[section]
\newtheorem{dbaxiom}{Axiom}[section]
\newenvironment{dbproof}[1][Proof]
{\noindent \textbf{#1: }}{\hfill $\blacksquare$}

%%-----------------------------------------------
%%-- Float related environments and commands.
%%-----------------------------------------------
%%--Modify behavior of the figure and table environment.
\def\fps@figure{hbt}
\def\fnum@figure{\textbf{\figurename~\thefigure}}
\def\fps@table{htb}
\def\fnum@table{\textbf{\tablename~\thetable}}
\def\mathfigure{\thesisfiguretrue\figure}
\def\endmathfigure{\endfigure\thesisfigurefalse}

% --------------------------------------------
% Headers
% --------------------------------------------
%REMOVED \pagestyle{fancyplain}
\pagestyle{scrheadings}
\renewcommand{\chaptermark}[1]{\markboth{\thechapter.\ #1}{}}
\renewcommand{\sectionmark}[1]{\markright{\thesection.\ #1}}

\makeatletter
\makeindex
\makeglossary
\setcounter{tocdepth}{4}
\setcounter{secnumdepth}{4}
\newcommand{\indic}{\mathtt{1}\!\!\mathtt{l}}
\newcommand{\proba}{\mathbb{P}}
\newcommand{\esper}{\mathbb{E}}
\newcommand{\Nats}{I\!\!N}
\newcommand{\nats}{I\!\!N}
\newcommand{\Reals}{I\!\!R}
\newcommand{\reals}{I\!\!R}
\newcommand{\espalm}{\mathbb{E}_N^{o}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% No indent and small paragraph sep.
\setlength{\parindent}{0mm}
\addtolength{\parskip}{1mm}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Floating figures
\renewcommand\floatpagefraction{.9}
\renewcommand\topfraction{.9}
\renewcommand\bottomfraction{.9}
\renewcommand\textfraction{.1}
\setcounter{totalnumber}{50}
\setcounter{topnumber}{50}
\setcounter{bottomnumber}{50}

\endinput

%% --The \tableofcontents.
\renewcommand\tableofcontents{%
  \renewcommand{\contentsname}{Table of Contents}
  \if@twocolumn
  \@restonecoltrue\onecolumn
  \else
  \@restonecolfalse
  \fi
  \oldchapter*{\contentsname
    \@mkboth{\uppercase{\contentsname}}{\uppercase{\contentsname}}}%
  \@starttoc{toc}%
  \if@restonecol\twocolumn\fi}

%% --The \listoffigures command.
\renewcommand\listoffigures{%
  \if@twocolumn
  \@restonecoltrue\onecolumn
  \else
  \@restonecolfalse
  \fi
  \oldchapter*{\listfigurename
    \@mkboth{\uppercase{\listfigurename}}{\uppercase{\listfigurename}}}%
  \@starttoc{lof}%
  \if@restonecol\twocolumn\fi}

%% --The \listoftables command.
\renewcommand\listoftables{%
  \if@twocolumn%
  \@restonecoltrue\onecolumn%
  \else%
  \@restonecolfalse%
  \fi%
  \oldchapter*{\listtablename%
    \@mkboth{\uppercase{\listtablename}}{\uppercase{\listtablename}}}%
  \@starttoc{lot}%
  \if@restonecol\twocolumn\fi}

\endinput

\lstdefinelanguage{json}{
    basicstyle=\normalfont\ttfamily,
    numbers=left,
    numberstyle=\scriptsize,
    stepnumber=1,
    numbersep=8pt,
    showstringspaces=false,
    breaklines=true,
    frame=lines,
    backgroundcolor=\color{background},
    literate=
     *{0}{{{\color{numb}0}}}{1}
      {1}{{{\color{numb}1}}}{1}
      {2}{{{\color{numb}2}}}{1}
      {3}{{{\color{numb}3}}}{1}
      {4}{{{\color{numb}4}}}{1}
      {5}{{{\color{numb}5}}}{1}
      {6}{{{\color{numb}6}}}{1}
      {7}{{{\color{numb}7}}}{1}
      {8}{{{\color{numb}8}}}{1}
      {9}{{{\color{numb}9}}}{1}
      {:}{{{\color{punct}{:}}}}{1}
      {,}{{{\color{punct}{,}}}}{1}
      {\{}{{{\color{delim}{\{}}}}{1}
      {\}}{{{\color{delim}{\}}}}}{1}
      {[}{{{\color{delim}{[}}}}{1}
      {]}{{{\color{delim}{]}}}}{1},
  }
