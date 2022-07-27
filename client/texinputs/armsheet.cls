% armsheet.cls 
% Class file for Ars Magica Character Sheets
% (C) 1998: Hans Georg Schaathun <georg@ii.uib.no>
% $Id$
%
% This file is distributed under the GNU public license.

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{armsheet}[2021/09/01 v$Revision$ ArM Character Sheet]
\LoadClass[a4paper]{report}

\newif\iffourth\fourthfalse
\DeclareOption{4ed}{\fourthtrue}
\ProcessOptions

\RequirePackage{armsheet,ifthen,array,arrayjob}
\nofiles


\def\CopyRight{{%
  \realsmall\selectfont
  \normalfont\copyright\ 
  \swabfamily 1998 Hans Georg Schaathun 
  $\langle$georg{\normalfont @}ii.uib.no$\rangle$}}

%
% Boxes
%

\newsavebox{\notebox}
\newsavebox{\hnotebox}
\newsavebox{\hermeticmiscbox}
\newsavebox{\abilitybox}
\newsavebox{\familiarusbox}
\newsavebox{\combatbox}
\newsavebox{\bodybox}
\newsavebox{\statbox}
\newsavebox{\armourbox}
\newsavebox{\personalitybox}
\newsavebox{\reputationbox}
\newsavebox{\vfbox}
\newsavebox{\equipmentbox}
\newsavebox{\troupebox}
\newsavebox{\topbox}
\newsavebox{\spellbox}
\newsavebox{\oldartsbox}
\newsavebox{\artsbox}

%
% Grog
%

\newif\ifgrogreputations\grogreputationstrue
\let\grog\document
\let\endgrog\enddocument
\AddToHook{env/grog/begin}{%
  \def\std@head#1{}
  \def\grog@head#1{#1}
  \def\minorheading{}
  \def\std@Ptrait{}
  \global\def\stdEqCount{11}
  \global\def\std@Ptrait[#1]#2{%
    \handfamily \fillRule #1\ 
    \parbox[b]{2em}{\hbox to\hsize{\handfamily #2\fillRule}}}
  \def\CharacterType{Grog Character}
  \def\AbilityNumber{18}
  \def\vfNumber{5}
  \def\repNumber{2}
  \grog@reputations
}
\AddToHook{env/grog/end}{%
  \input{grogbox.tex}
  \input{grog.tex}
}

%
% Companion
%

\def\mundane@counts{%
  \def\AbilityNumber{30}
  \def\vfNumber{11}
  \def\repNumber{3}
}

\let\bgrog\document
\let\endbgrog\enddocument
\AddToHook{env/bgrog/begin}{%
  \def\CharacterType{Grog Character}
  \mundane@counts
}
\AddToHook{env/bgrog/end}{%
%%%   \input{boxdef.tex}
  \input{nogrogbox.tex}
  \input{mundane.tex}
}

\let\companion\document
\let\endcompanion\enddocument
\AddToHook{env/companion/begin}{%
  \def\CharacterType{Companion Character}
  \mundane@counts
}
\AddToHook{env/companion/end}{%
  \input{nogrogbox.tex}
  \input{mundane.tex}
}

% 
% Magus
% 

\AtBeginDocument{\printfamily\usual}

\let\magus\document
\let\endmagus\enddocument
\AddToHook{env/magus/begin}{%
  \def\CharacterType{Magu\s\ Character}
  \mundane@counts
  \def\spellNumber{18}
}
\AddToHook{env/magus/end}{%
  \input{nogrogbox.tex}
  \input{hboxes.tex}
  \input{mundane.tex}\newpage
  \hermetic@page\newpage
  \@familiarus
  \usebox{\spellbox}
}

\newenvironment{spells}{%
}{%
}

%
% Empty sheets
%

\def\m@default{%
  \begin{abilities}\end{abilities}
  \begin{equipment}\end{equipment}
  \begin{personality}\end{personality}
  \begin{vf}\end{vf}
  \begin{reputations}\end{reputations}
}
\def\h@default{%
  \m@default
  \begin{grimoire}\end{grimoire}
}

\def\makenpccompanion{
  \begin{companion}
    \def\CharacterType{NPC Companion} \m@default
  \end{companion}
}
\def\makenpcgrog{
  \begin{grog}
    \def\CharacterType{NPC Grog} \m@default
  \end{grog}
}
\def\makenpcmagus{
  \begin{magus}
    \def\CharacterType{NPC Magu\s} \h@default
  \end{magus}
}
\def\makecompanion{
  \begin{companion} \m@default \end{companion}
}
\def\makegrog{
  \begin{grog} \m@default \end{grog}
}
\def\makemagus{
  \begin{magus} \h@default \end{magus}
}

%
% Abilities
%

\newcommand{\AbilityLine}[4][\hspace{64pt}]{#2 & \lb #1\rb & #3 & #4 \\\hline}
\newcounter{abilityline}

\def\Anability{\addtocounter{abilityline}{-1}\AbilityLine}
\def\std@head#1{\minorheading #1}
\def\grog@head#1{}

\newenvironment{hnotes}{%
  \boxpage{\hnotebox}{0.9\textwidth}
  \flushleft
  \handfamily
}{
  \endboxpage
}
\newenvironment{notes}{%
  \boxpage{\notebox}{20pc}
  \flushleft
  \handfamily
}{
  \endboxpage
}
  

\newenvironment{abilities}{%
  \boxpage{\abilitybox}{21pc}
  \setcounter{abilityline}{\AbilityNumber}
  \std@head{Abilitie\s\\}
  \usual
  \begin{tabular}{>{\handfamily}l>{\handfamily}c|%
                  >{\handfamily}c|>{\handfamily}c}
    \multicolumn{2}{l}{\grog@head{Abilitie\s}}&
    \multicolumn{1}{c}{Scr}&
    \multicolumn{1}{c}{Exp}\\ \hline
}{%
    \whiledo{\value{abilityline} > 0}{%
      \addtocounter{abilityline}{-1}%
      \AbilityLine{}{}{}%
    }
    \hbox{\hspace{80pt}}&\multicolumn{1}{c}{\smaller Specialty}&
    \multicolumn{2}{c}{}\\
  \end{tabular}
  \endboxpage
}

%
% Virtues and flaws
%

\newcommand{\vfLine}[2]{\c@r\addtocounter{vfline}{-1} 
  \hbox to 160pt{\fillRule #1} & \hbox to30pt{#2\fillRule}\ignorespaces}
\def\c@r{\gdef\c@r{\\}}
\newcounter{vfline}

\newenvironment{vf}{%
  \setcounter{vfline}{\vfNumber}
  \boxpage{\vfbox}{18pc}
    {\minorheading Virtue\s\ \a\ Flaw\s}\\[0.5ex]
    \begin{tabular*}{\linewidth}{%
      >{\handfamily}r@{\extracolsep{\fill}}>{\handfamily}r}
}{%
    \whiledo{\value{vfline} > 0}{\vfLine{}{}}
    \end{tabular*}
  \endboxpage
}

%
% Reputations
%

\def\grog@reputations{
  \def\rep@head{\relax}
  \def\rep@foot{\relax}
  \def\rep@grog@head{\print{Reputation} &\smaller\print{Target area}}
  \def\@reputation##1##2##3{\\\addtocounter{repline}{-1}%
                            ##1&\strut\lb##2\rb&##3}
}
\def\@reputation#1#2#3{\addtocounter{repline}{-1} #1&\strut\lb#2\rb&#3\\}
\newcounter{repline}
\def\Areputation#1#2#3{\@reputation{#1}{#2}{#3}}
\def\repLine{\Areputation%
  {\rule{7.8pc}{1pt}}{\rule{5.0pc}{1pt}}{\rule{2.3pc}{1pt}}}

\def\rep@head{\centerline{\minorheading\strut Reputation}}
\def\rep@foot{&\smaller\print{Target area}&}
\def\rep@grog@head{\relax}
\newenvironment{reputations}{%
  \setcounter{repline}{\repNumber}
  \boxpage{\reputationbox}{21pc}
  \rep@head
  \begin{tabular}{>{\handfamily}c>{\handfamily}c>{\handfamily}c}
  \rep@grog@head
}{%
    \whiledo{\value{repline} > 0}{\repLine}
    \rep@foot
  \end{tabular}
  \hfill
  \endboxpage
}


%
% Spells
%

\newcounter{spelline}
\newcommand{\SpellLine}[6][\hbox{\hspace{28pc}}]%
           {#1 & #2 & #3 & #4 & #5 \\\hline              %
            \multicolumn{5}{l}{Foci \a\ Note\s : {\handfamily #6} }\\\hline}

\def\Aspell#1#2#3#4#5#6{%
  \addtocounter{spelline}{-1}
  \SpellLine[#1]{#2}{#3}{#4}{#5}{#6}
}

\newenvironment{grimoire}{%
  \setcounter{spelline}{\spellNumber}
  \boxpage{\spellbox}{\textwidth}
    \pageheader{Magu\s\ Grimoire}
    \begin{tabular}{>{\handfamily}l|>{\handfamily}c
                   |>{\handfamily}c|>{\handfamily}c|>{\handfamily}c}\hline
                                      &Tech \a &     &Total &\\
      \multicolumn{1}{c|}{Spell Title}&Form    &Level&Bonu\s&Mastery\\\hline
}{%
      \whiledo{\value{spelline} > 0}{%
        \addtocounter{spelline}{-1}%
        \SpellLine{}{}{}{}{}%
      }
      \SpellLine{}{}{}{}{}%
    \end{tabular}
  \endboxpage
}


%
% Art score definitions
%

\def\techniques#1,#2,#3,#4,#5.{%
  \def\artcreo{#1}%
  \def\artintellego{#2}%
  \def\artmuto{#3}%
  \def\artperdo{#4}%
  \def\artrego{#5}%
}

\def\forms#1,#2,#3,#4,#5,#6,#7,#8,#9.{%
  \def\artanimal{#1}%
  \def\artaquam{#2}%
  \def\artauram{#3}%
  \def\artcorpus{#4}%
  \def\artherbam{#5}%
  \def\artignem{#6}%
  \def\artimaginem{#7}%
  \def\artmentem{#8}%
  \Forms#9.%
}
\def\Forms#1,#2.{%
  \def\artterram{#1}%
  \def\artvim{#2}%
}
  \def\xpcreo{}%
  \def\xpintellego{}%
  \def\xpmuto{}%
  \def\xpperdo{}%
  \def\xprego{}%
  \def\xpanimalem{}%
  \def\xpaquam{}%
  \def\xpauram{}%
  \def\xpcorpus{}%
  \def\xpherbam{}%
  \def\xpignem{}%
  \def\xpimagonem{}%
  \def\xpmentem{}%
  \def\xpterram{}%
  \def\xpvim{}%

  \def\artcreo{}%
  \def\artintellego{}%
  \def\artmuto{}%
  \def\artperdo{}%
  \def\artrego{}%
  \def\artanimal{}%
  \def\artaquam{}%
  \def\artauram{}%
  \def\artcorpus{}%
  \def\artherbam{}%
  \def\artignem{}%
  \def\artimaginem{}%
  \def\artmentem{}%
  \def\artterram{}%
  \def\artvim{}%

\def\vis#1#2{\expandafter\def\csname vis#1\endcsname{#2}}
\def\artxp#1#2{\expandafter\def\csname xp#1\endcsname{#2}}
  \def\viscreo{}%
  \def\visintellego{}%
  \def\vismuto{}%
  \def\visperdo{}%
  \def\visrego{}%
  \def\visanimal{}%
  \def\visaquam{}%
  \def\visauram{}%
  \def\viscorpus{}%
  \def\visherbam{}%
  \def\visignem{}%
  \def\visimaginem{}%
  \def\vismentem{}%
  \def\visterram{}%
  \def\visvim{}%

%
% Primary data
%

\def\name#1{\def\Thename{\hand{#1}}}\name{\hspace{-1ex}}
\def\age#1{\def\Theage{\hand{#1}}}\age{\hspace{-1ex}}
\def\gender#1{\def\Thegender{\hand{#1}}}\gender{\hspace{-1ex}}
\def\player#1{\def\Theplayer{\hand{#1}}}\player{\hspace{-1ex}}
\def\born#1{\def\Theborn{\hand{#1}}}\born{\hspace{-1ex}}
\def\size#1{\def\Thesize{\hand{#1}}\def\Valsize{#1}}
  \def\Thesize{\hspace{-1ex}}
\def\covenant#1{\def\Thecovenant{\hand{#1}}}\covenant{\hspace{-1ex}}
\def\year#1{\def\Theyear{\hand{#1}}}\year{\hspace{-1ex}}
\def\conf#1{\def\Theconf{\handfamily #1}}\conf{}
\def\saga#1{\def\Thesaga{\hand{#1}}}\saga{\hspace{-1ex}}
\def\nationality#1{\def\Thenationality{\hand{#1}}}\nationality{\hspace{-1ex}}
\def\concept#1{\def\Theconcept{\hand{#1}}}\concept{\hspace{-1ex}}
\def\decr#1{\def\Thedecr{\hand{#1}}}\decr{\hspace{-1ex}}
\def\cconf#1{\def\Thecconf{\handfamily #1}}\cconf{}

%
% Characteristics
%

\newcommand\Characteristic[3][\rule{64pt}{1pt}]{%
  \expandafter\def\csname #3spec\endcsname{#1}
  \expandafter\def\csname #3score\endcsname{#2}}

\Characteristic{\rule{30pt}{1pt}}{int}
\Characteristic{\rule{30pt}{1pt}}{per}
\Characteristic{\rule{30pt}{1pt}}{str}
\Characteristic{\rule{30pt}{1pt}}{sta}
\Characteristic{\rule{30pt}{1pt}}{com}
\Characteristic{\rule{30pt}{1pt}}{pre}
\Characteristic{\rule{30pt}{1pt}}{qui}
\Characteristic{\rule{30pt}{1pt}}{dex}

%
% Equipment
%

\newarray\eqTable
\newcounter{equipcount}
\def\eqPiece#1#2{\eqTable(\value{equipcount})={\std@Equipment{#1}{#2}}%
    \addtocounter{equipcount}{1}}
\def\std@Equipment#1#2{\hbox to 68mm{\handfamily #1\hfill} 
    & \handfamily #2}
\def\std@eqPiece{\eqPiece{}{}}
\def\stdEqCount{20}
\def\stdEqHalfCount{10}
\newenvironment{equipment}{%
  \setcounter{equipcount}{1}
}{%
  \whiledo{\value{equipcount} < \stdEqCount}{\std@eqPiece{}{}}
}

%
% Personality traits
%

\newarray\Ptable
\newcounter{ptrait}
\def\aPtrait#1#2{\Ptable(\value{ptrait})={\std@Ptrait[\strut #1]{#2}}%
    \addtocounter{ptrait}{1}}
\newcommand\std@Ptrait[2][]%
  {\handfamily \fillRule #1\ \smallbox[\handfamily #2]}
\newenvironment{personality}{%
  \setcounter{ptrait}{1}
}{%
  \whiledo{\value{ptrait} < 7}{\aPtrait{}{}}
}
\begin{personality}
  \aPtrait{\strut}{}
\end{personality}

%
% Savebox
%

\newenvironment{boxpage}[2]{%
  \lrbox{#1}\minipage{#2}\center
}{%
  \endcenter\endminipage\endlrbox
}

%
% Wapons and Armour
%

\def\armour#1#2#3{\def\ArmourType{#1}\def\ArmourLoad{#2}\def\Protection{#3}}
\armour{}{}{}

%
% Misc.
%

\def\sigil#1{\def\theSigil{#1}}\sigil{}
\newcounter{count}
\def\Repeat#1#2{%
  \setcounter{count}{#1}%
  \whiledo{\value{count} > 0}{\addtocounter{count}{-1}#2}%
}
\def\soak#1{\def\soaktotal{#1}}\soak{}
\def\loadenc#1#2{\def\loadtotal{#1}\def\enctotal{#2}}\loadenc{}{}
\def\fatigue#1{\def\fatiguetotal{#1}}\fatigue{}

\def\familiarus{\def\@familiarus{\usebox{\familiarusbox}\newpage}}
\let\@familiarus=\relax
\def\hermeticbox{\usebox{\artsbox}}

\def\hermetic@page{
  \pageheader{Hermetic Trait\s}
  \hrule height1pt
  \vspace{6pt}
  \hermeticbox
  \vspace{6pt} \hrule height1pt \vspace{6pt}
  \usebox{\hermeticmiscbox}
  \vspace{6pt} \hrule height1pt \vspace{6pt}
  \centerline{\minorheading\strut Note\s}
  \usebox{\hnotebox}
  \vfill
  \hrule height1pt
}

\def\TwilightP#1{\def\twlp{#1}}
\TwilightP{}

\newarray\ctW
\newarray\ctInit
\newarray\ctAtk
\newarray\ctDef
\newarray\ctDam
\newarray\ctRng
\iffourth\newarray\ctSpc\fi
\newarray\ctStr
\newcounter{ctcnt}
\newenvironment{combat}{%
  \setcounter{ctcnt}{1}
  \iffourth
  \def\weapon:##1:init=##2,atk=##3;def=##4,dam=##5,rng=##6,spc=##7,str=##8.{
    \ctW(\value{ctcnt})={\handfamily ##1}
    \ctInit(\value{ctcnt})={\handfamily ##2}
    \ctAtk(\value{ctcnt})={\handfamily ##3}
    \ctDef(\value{ctcnt})={\handfamily ##4}
    \ctDam(\value{ctcnt})={\handfamily ##5}
    \ctRng(\value{ctcnt})={\handfamily ##6}
    \ctSpc(\value{ctcnt})={\handfamily ##7}
    \ctStr(\value{ctcnt})={\handfamily ##8}
    \addtocounter{ctcnt}{1}
  }
  \else
  \def\weapon:##1:init=##2,atk=##3;def=##4,dam=##5,rng=##6,str=##7.{
    \ctW(\value{ctcnt})={\handfamily ##1}
    \ctInit(\value{ctcnt})={\handfamily ##2}
    \ctAtk(\value{ctcnt})={\handfamily ##3}
    \ctDef(\value{ctcnt})={\handfamily ##4}
    \ctDam(\value{ctcnt})={\handfamily ##5}
    \ctRng(\value{ctcnt})={\handfamily ##6}
    \ctStr(\value{ctcnt})={\handfamily ##7}
    \addtocounter{ctcnt}{1}
  }
  \fi
}{%
}

\begin{combat}
  \iffourth
    \weapon:Brawl:init=,atk=;def=,dam=,rng=Touch,spc=,str=-.
    \weapon::init=,atk=;def=,dam=,rng=,spc=,str=.
    \weapon::init=,atk=;def=,dam=,rng=,spc=,str=.
    \weapon::init=,atk=;def=,dam=,rng=,spc=,str=.
    \weapon::init=,atk=;def=,dam=,rng=,spc=,str=.
  \else
    \weapon:Brawl:init=,atk=;def=,dam=,rng=-,str=-.
    \weapon::init=,atk=;def=,dam=,rng=,str=.
    \weapon::init=,atk=;def=,dam=,rng=,str=.
    \weapon::init=,atk=;def=,dam=,rng=,str=.
    \weapon::init=,atk=;def=,dam=,rng=,str=.
  \fi
\end{combat}

\newenvironment{background}{
  \global\newsavebox{\bgbox}
  \begingroup
  \lrbox{0\null\global\setbox\bgbox}
     \minipage{0.8\linewidth}
     \parindent=0pt
     \parskip=1ex
     \normalfont
}{
   \endminipage
   \endlrbox
   \endgroup
}


\endinput


