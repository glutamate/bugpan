\documentclass[11pt]{article}
%include lhs2TeX.fmt
%include lhs2tex-braincurry-preamble
\usepackage[a4paper, top=2.5cm, bottom=2.5cm, left=2.5cm, right=2.5cm]{geometry}
\usepackage{amsmath, amsthm, amssymb}
\usepackage{setspace} 
\usepackage{verbatim} 
\usepackage[final]{pdfpages}
\usepackage[super]{natbib}
\usepackage{graphicx}

\begin{document}
\doublespacing 
\begin{tabular}{l  l  p{8cm}}
\hline
  Function & Type & Description\\ 
\hline
  |peak| & |Signal alpha -> Event alpha| & Peak value of each signal segment\\

  |freqDuring| & 
\parbox{4cm}{\begin{singlespace}
|Duration alpha -> Event beta| \\|-> Duration Real|
\end{singlespace}} 
& Count events in each occurrence\\

  |around| &
\parbox{4cm}{\begin{singlespace}
|Event alpha -> Signal beta| \\|-> Signal beta|
\end{singlespace}} 
& Align signal around event occurrences\\

  |inout| &
\parbox{4cm}{\begin{singlespace}
|Event alpha -> Event beta| \\|-> Duration alpha times beta|
\end{singlespace}} 
& Create a duration from start and stop events\\

  |convolveSE| &
\parbox{4cm}{\begin{singlespace}
|Signal Real -> Event Real| \\|-> Signal Real|
\end{singlespace}} 
& Convolve a signal with an event\\

  |during| &
\parbox{4cm}{\begin{singlespace}
|Duration beta -> f alpha | \\ |-> f alpha|
\end{singlespace}} 
& \parbox{6cm}{\begin{singlespace}Events/Durations/Signals that lie within occurrences in a duration\end{singlespace}}\\

  |burst| &
\parbox{4cm}{\begin{singlespace}
|Real -> Event alpha | \\|-> Duration ()|
\end{singlespace}} 
& \parbox{6cm}{\begin{singlespace}Durations when successive inter-event occurrence intervals fall below a minimum\end{singlespace}}\\

  |adjustDur| &
\parbox{4cm}{\begin{singlespace}
|Time times Time| \\ | -> Time times Time | \\ |-> Duration alpha | \\|-> Duration alpha|
\end{singlespace}} 
& \parbox{6cm}{\begin{singlespace}Apply a function to adjust the beginning and end of each duration occurrence\end{singlespace}}\\

 & & \\

\hline


\end{tabular}

\vskip1ex 

%\end{comment}

Table S1. Some common operations for generic manipulation of signals, events and durations.

\pagebreak

\begin{tabular}{l p{10cm}}
\hline
  Expression & Denotes\\ 
\hline
  |\x->e| & The function that takes argument |x| and returns |e|\\

  |f x| & \parbox{9cm}{\begin{singlespace}Apply the function (or function-value expression) |f| to the value |x|\end{singlespace}} \\

  |x| & The value of the variable |x|\\

  |e::t| & Annotation: the expression |e| has type |t|\\

  |let x = e in y| & Define |x| as the value of |e| in the expression |y|\\

  |if p then c else a| & If |p| is |True| then yield |c|; if |p| is |False| yield |a|\\

  |(x,y)| & The pair (Cartesian product) of x and y \\

  |sopen e sclose| & The signal whose value is given by expression |e|\\

  |<: s :>| & \parbox{9cm}{\begin{singlespace}The value of the signal, in the temporal context of the surrounding |sopen ... sclose| brackets\end{singlespace}}\\

  |D s| & \parbox{9cm}{\begin{singlespace}The derivative of signal |s|. When used on the left-hand side of a definition, it introduces a differential equation\end{singlespace}}\\

  |s_0| & \parbox{9cm}{\begin{singlespace}The initial value of the signal |s| (can also be used on the left-hand side of a definition)\end{singlespace}}\\

  |delay s| & The signal |s|, dealyed by a short time period\\

  |p ?? s| & Events that occur when the value of |s| satisfies the predicate |p| \\

  |x <* src p| & \parbox{9cm}{\begin{singlespace}(Top-level only) Bind the value x to the observation of the source |src|, with parameter |p|\end{singlespace}}\\

  |e *> snk p| & \parbox{9cm}{\begin{singlespace}(Top-level only) Send the value x to the sink |snk|, with parameter |p|\end{singlespace}}\\


 & \\





\end{tabular}

\vskip1ex 

%\end{comment}

Table S2. Syntax of CoPE

\pagebreak
\begin{verbatim}

module Looming where

lov = 4.0000e-2
l = 0.2980
v = l/(lov*2)

distance = {: (min (v*(<: seconds :> - 5))) (-0.1800) :}
loomObj = {: colour black (translate (0, 0, <: distance :>) (cube l)) :}
loomObj *> screen ""

_tmax=6

ecVoltage <* adc (0,20000,6) 
ecVoltage *> store ""

\end{verbatim}

\flushleft Listing 1. Code for experiment in Example 1, as used to
obtain the data (not typeset). There are minor syntactical differences
between equations the paper and this code, which were run by an early
implementation of CoPE.

\pagebreak
\singlespacing 
\begin{verbatim}
module DynamicClamp where

gampa = 5.0e-10
rate = 50
gmaxk = 0
_tmax = 2
_dt = 5.0e-5

alpha tau t = if t<0.0 then 0.0 else (t/tau)*exp (1-t/tau)
gsyn = {: gampa* (alpha 0.005 <: seconds:>) :}
stage gsyn -1		

rawv, celli, vm, a, b, ika :: Signal Real

rndSpike :: [(Real, ())]
rndSpike <* poisson rate

rawv <* ADC 0
vm = {: <: rawv:>  *0.10 :}

gcellsyn = {: convolution gsyn (forget 0.1 rndSpike ) <:seconds:> :}

D a = {: 1000*(((alphaa ((<: vm:>*1000)+60))* ( 1-<:a:>)) -
              ((betaa ((<:vm:>*1000)+60)) * <: a :>)) :}
a_0 = 0.025

D b = {: 1000*(((alphab ((<: vm:>*1000)+60)) * ( 1-<:b:>)) -
              ((betab ((<:vm:>*1000)+60)) * <: b :>)) :}
b_0 = 0.9

alphaa, betaa, alphab, betab :: Real -> Real

alphaa v = (0.02*(13.1-v))/((exp ((13.1 -v)/10)) -1)
betaa v =  (0.0175*(v-40.1))/((exp ((v-40.1)/10))-1)

alphab v = 0.0016*exp((0-13-v)/18)
betab v = 0.05/(1+(exp((10.1-v)/5))) 

ika = {: gmaxk * <:a:> * <:b:>*(0.08+<:vm:>) :}
ika_0 = 0

celli = {: (0-<:vm:>) * <:gcellsyn:>  - <: ika:> :}
outv = {: <:celli:> * 1.0e9 :} 
outv *> DAC 0

vm *> store ""
\end{verbatim}

\flushleft Listing 2. Code for experiment in Example 2. 


\end{document}