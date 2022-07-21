% memoire.sty 8-septembre-1994

\typeout{Document Style `memoire' <15 septembre 1995>.}

\def\presentation#1{\gdef\@presentation{#1}}
\def\faculte#1{\gdef\@faculte{#1}}
\def\universite#1{\gdef\@universite{#1}}
\def\ville#1{\gdef\@ville{#1}}
\def\province#1{\gdef\@province{#1}}
\def\pays#1{\gdef\@pays{#1}}

\@twosidefalse 

\def\maketitle{\begin{titlepage}%
\let\footnotesize\small \let\footnoterule\relax \setcounter{page}{0}%
\null%
\vfil
%\vskip 1.5in 
\begin{center}
{\larges \bf \@title \par} \vskip .9in {\larges par \par}
\vskip .9in {\large \lineskip .75em 
\begin{tabular}[t]{c}\@author 
\end{tabular}\par} 
\vskip .9in {\larges\@presentation \par}
\vskip .9in {\larges\@faculte \par}
\vskip .2em {\larges \@universite \par}
\vskip .9in {\larges \@ville, \@province, \@pays, \@aujourdhui \par}
\end{center} \par
\@thanks
\vfil
\null
\end{titlepage}
\setcounter{footnote}{0} \let\thanks\relax
\gdef\@thanks{}\gdef\@author{}\gdef\@title{}\gdef\@presentation{}\gdef\@ville{}
\gdef\@faculte{}\gdef\@universite{}\gdef\@province{}\gdef\@pays{}\let\maketitle\relax} 


\def\@aujourdhui{\ifcase\month\or
 janvier\or f\'evrier\or mars\or avril\or mai\or juin\or
 juillet\or ao\^ut\or septembre\or octobre\or novembre\or d\'ecembre\fi \space
 \number\year}

% double interligne
\def\@normalsize{\@setsize\normalsize{21.75pt}\xiipt\@xiipt
\abovedisplayskip 12pt plus3\p@ minus7\p@
\belowdisplayskip \abovedisplayskip
\abovedisplayshortskip \z@ plus3\p@
\belowdisplayshortskip 6.5\p@ plus3.5\p@ minus3\p@
\let\@listi\@listI} 

\def\small{\@setsize\small{20.4pt}\xipt\@xipt
\abovedisplayskip 11\p@ plus3\p@ minus6\p@
\belowdisplayskip \abovedisplayskip
\abovedisplayshortskip \z@ plus3\p@
\belowdisplayshortskip 6.5\p@ plus3.5\p@ minus3\p@
\def\@listi{\leftmargin\leftmargini 
	\parsep 4.5\p@ plus 2\p@ minus 1\p@
	\itemsep \parsep
 	\topsep 9\p@ plus 3\p@ minus 5\p@}}

\def\footnotesize{\@setsize\footnotesize{12pt}\xpt\@xpt
\abovedisplayskip 10\p@ plus2\p@ minus5\p@
\belowdisplayskip \abovedisplayskip
\abovedisplayshortskip \z@ plus3\p@
\belowdisplayshortskip 6\p@ plus3\p@ minus3\p@
\def\@listi{\leftmargin\leftmargini 
	\topsep 6\p@ plus 2\p@ minus 2\p@\parsep 3\p@ plus 2\p@ minus 1\p@
	\itemsep \parsep}}

\def\scriptsize{\@setsize\scriptsize{14.25pt}\viiipt\@viiipt}
\def\tiny{\@setsize\tiny{10.5pt}\vipt\@vipt}
\def\large{\@setsize\large{27pt}\xivpt\@xivpt}
\def\larges{\@setsize\large{18pt}\xivpt\@xivpt}
\def\Large{\@setsize\Large{33pt}\xviipt\@xviipt}
\def\LARGE{\@setsize\LARGE{37.5pt}\xxpt\@xxpt}
\def\huge{\@setsize\huge{45pt}\xxvpt\@xxvpt}
\let\Huge=\huge

\normalsize

\oddsidemargin .25 true in 
\evensidemargin .25 true in
\marginparwidth 1in 
\marginparsep 7pt 
\topmargin 0 true in 
\headheight 12pt 
\headsep .275in 
\footskip 30pt 

\textheight = 8 true in
\textwidth 6.25 true in 
\columnsep 10pt 
\columnseprule 0pt 

\def\@chapter[#1]#2{\ifnum \c@secnumdepth >\m@ne
                       \if@mainmatter
                         \refstepcounter{chapter}%
                         \typeout{\@chapapp\space\thechapter.}%
                         \addcontentsline{toc}{chapter}%
                                   {\@chapapp\space\thechapter\space---\space#1}%
                       \else
                         \addcontentsline{toc}{chapter}{#1}%
                       \fi
                    \else
                      \addcontentsline{toc}{chapter}{#1}%
                    \fi
                    \chaptermark{#1}%
                    \addtocontents{lof}{\protect\addvspace{10\p@}}%
                    \addtocontents{lot}{\protect\addvspace{10\p@}}%
                    \if@twocolumn
                      \@topnewpage[\@makechapterhead{#2}]%
                    \else
                      \@makechapterhead{#2}%
                      \@afterheading
                    \fi}

\def\refname{R\'EF\'ERENCES}%
\def\abstractname{R\'ESUM\'E}%
\def\bibname{BIBLIOGRAPHIE}%
\def\contentsname{TABLE DES MATI\`ERES}%
\def\listfigurename{LISTE DES FIGURES}%
\def\listtablename{LISTE DES TABLEAUX}%
\def\indexname{INDEX}%
\def\sommairename{SOMMAIRE}%
\def\glossaryname{GLOSSAIRE}%
\def\chaptername{CHAPITRE}%
\def\appendixname{ANNEXE}%
