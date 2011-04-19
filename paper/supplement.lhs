\documentclass[11pt]{article}
%include lhs2TeX.fmt
%include lhs2tex-braincurry-preamble
\usepackage[a4paper, top=2.5cm, bottom=2.5cm, left=2.5cm, right=2.5cm]{geometry}
\usepackage{amsmath, amsthm, amssymb}
\usepackage{setspace} 
\usepackage{verbatim} 
%\usepackage[final]{pdfpages}
%\usepackage[super]{natbib}
%\usepackage{graphicx}

\begin{document}
\doublespacing 

\section*{Supplementary Information}
\begin{tabular}{l p{10cm}}
\hline
  Expression & Denotes\\ 
\hline
  |\x->e| & The function that takes argument |x| and returns |e|\\

  |f x| & \parbox{9cm}{\begin{singlespace}Apply the function (or function-value expression) |f| to the value |x|\end{singlespace}} \\

  |x| & The value of the variable |x|\\

  |e::t| & Annotation: the expression |e| has type |t|\\

  |let x = e in y| & Define |x| as the value of |e| in the expression |y|\\

  |if p then e_1 else e_2| & If |p| is |True| then yield |e_1|; if |p| is |False| yield |e_2|\\

  |(x,y)| & The pair (Cartesian product) of x and y \\

  |sopen e sclose| & The signal whose value is given by expression |e|\\

  |<: s :>| & \parbox{9cm}{\begin{singlespace}The value of the signal, in the temporal context of the surrounding |sopen ... sclose| brackets\end{singlespace}}\\

  |D s| & \parbox{9cm}{\begin{singlespace}The derivative of signal |s|. When used on the left-hand side of a definition, it introduces a differential equation\end{singlespace}}\\

  |s_0| & \parbox{9cm}{\begin{singlespace}The initial value of the signal |s| (can also be used on the left-hand side of a definition)\end{singlespace}}\\

  |delay s| & The signal |s|, dealyed by a short time period\\

  |p ?? s| &  \parbox{9cm}{\begin{singlespace}Events that occur when the value of |s| satisfies the predicate |p| \end{singlespace}}\\

  |x <* src| & \parbox{9cm}{\begin{singlespace}(Top-level only) Bind the value x to the observation of the source |src|\end{singlespace}}\\

  |e *> snk| & \parbox{9cm}{\begin{singlespace}(Top-level only) Send the value |e| to the sink |snk|\end{singlespace}}\\


 & \\
\hline




\end{tabular}

\vskip1ex 

%\end{comment}

\noindent Table S1. Syntax of CoPE

\pagebreak
\begin{tabular}{l  l  p{8cm}}
\hline
  Function & Type & Description\\ 
\hline

  |adjustDur| &
\parbox{4cm}{\begin{singlespace}
(|Time times Time| \\ | -> Time times Time|) \\ |-> Duration alpha | \\|-> Duration alpha|
\end{singlespace}} 
& \parbox{8cm}{\begin{singlespace}Apply a function to adjust the beginning and end of each duration occurrence\end{singlespace}}\\

  |area| & |Signal Float -> Event Float|
& \parbox{8cm}{\begin{singlespace}Calculate the centre of mass (time of the event) and area (tag of the event) of a signal\end{singlespace}}\\

  |around| &
\parbox{4cm}{\begin{singlespace}
|Event alpha -> Signal beta| \\|-> Signal beta|
\end{singlespace}} 
& Align signal around event occurrences\\

  |baseline| & 
\parbox{4cm}{\begin{singlespace}|Float -> Float -> Signal Float | \\ |-> Signal Float|
\end{singlespace}}
& \parbox{8cm}{\begin{singlespace}Subtract from a signal its mean value between two time points\end{singlespace}}\\

  |before| &
\parbox{4cm}{\begin{singlespace}
|f alpha|\\|-> g beta -> g beta|
\end{singlespace}} 
& \parbox{8cm}{\begin{singlespace}All Events/Durations/Signals (|g|) occurring before the first occurrence of an Event/Duration/Signal (|f|), with an analogous |after|\end{singlespace}} \\

  |burst| &
\parbox{4cm}{\begin{singlespace}
|Float -> Event alpha | \\|-> Duration ()|
\end{singlespace}} 
& \parbox{8cm}{\begin{singlespace}Durations when successive inter-event occurrence intervals are smaller than a set minimum\end{singlespace}}\\

  |convolveSE| &
\parbox{4cm}{\begin{singlespace}
|Signal Float -> Event Float| \\|-> Signal Float|
\end{singlespace}} 
& Convolve a signal with an event\\

  |during| &
\parbox{4cm}{\begin{singlespace}
|Duration beta -> f alpha | \\ |-> f alpha|
\end{singlespace}} 
& \parbox{8cm}{\begin{singlespace}Events/Durations/Signals (|f|) that lie within occurrences in a duration\end{singlespace}}\\

  |freqDuring| & 
\parbox{4cm}{\begin{singlespace}
|Duration alpha -> Event beta| \\|-> Duration Float|
\end{singlespace}} 
& Count events in each occurrence\\

  |inout| &
\parbox{4cm}{\begin{singlespace}
|Event alpha -> Event beta| \\|-> Duration alpha times beta|
\end{singlespace}} 
& Create a duration from start and stop events\\

  |intervals| & |Event alpha -> Event Float|
& \parbox{8cm}{\begin{singlespace}Replace the tag of each occurrence with the time period to the next occurrence\end{singlespace}}\\


  |later| &
\parbox{4cm}{\begin{singlespace}
|Float -> Event alpha| \\ |-> Event alpha|
\end{singlespace}} 
& \parbox{8cm}{\begin{singlespace}Delay each event occurrence by a fixed amount of time\end{singlespace}}\\

  |peak| & |Signal alpha -> Event alpha| & Peak value of each signal segment\\

  |smoothN| & 
\parbox{4cm}{\begin{singlespace}|Int -> Signal Float | \\ |-> Signal Float|
\end{singlespace}}
& \parbox{8cm}{\begin{singlespace}Smooth a signal with the binomial filter\end{singlespace}}\\

  |tag| & |alpha -> f beta -> f alpha|
& \parbox{8cm}{\begin{singlespace}Change all tags of events or durations (|f|) to a fixed value\end{singlespace}}\\

  |//| &
\parbox{4cm}{\begin{singlespace}
|(alpha -> Bool) -> f alpha | \\ | -> f alpha|
\end{singlespace}} 
& \parbox{8cm}{\begin{singlespace}Exclude events or durations (|f|) where the tag does not satisfy a predicate\end{singlespace}}\\

 & & \\

\hline


\end{tabular}

\vskip1ex 

%\end{comment}

\noindent Table S2. Examples of common operations in CoPE for generic manipulation of signals, events and durations.

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
_dt = 5.0e-5

ecVoltage <* ADC 0 20000
ecVoltage *> store ""

\end{verbatim}

\begin{flushleft} Listing 1. Entire unformatted code for the experiment in
Example 1, related to Figure 1 and 2.
\end{flushleft}
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

rawv, celli, vm, a, b, ika :: Signal Float

rndSpike :: [(Float, ())]
rndSpike <* poisson rate

rawv <* ADC 0 20000
vm = {: <: rawv:>  *0.10 :}

gcellsyn = {: convolution gsyn (forget 0.1 rndSpike ) <:seconds:> :}

D a = {: 1000*(((alphaa ((<: vm:>*1000)+60))* ( 1-<:a:>)) -
              ((betaa ((<:vm:>*1000)+60)) * <: a :>)) :}
a_0 = 0.025

D b = {: 1000*(((alphab ((<: vm:>*1000)+60)) * ( 1-<:b:>)) -
              ((betab ((<:vm:>*1000)+60)) * <: b :>)) :}
b_0 = 0.9

alphaa, betaa, alphab, betab :: Float -> Float

alphaa v = (0.02*(13.1-v))/((exp ((13.1 -v)/10)) -1)
betaa v =  (0.0175*(v-40.1))/((exp ((v-40.1)/10))-1)

alphab v = 0.0016*exp((0-13-v)/18)
betab v = 0.05/(1+(exp((10.1-v)/5))) 

ika = {: gmaxk * <:a:> * <:b:>*(0.08+<:vm:>) :}
ika_0 = 0

celli = {: (0-<:vm:>) * <:gcellsyn:>  - <: ika:> :}
outv = {: <:celli:> * 1.0e9 :} 
outv *> DAC 0 20000

vm *> store ""
\end{verbatim}

\begin{flushleft}Listing 2. Entire code for the experiment in Example 2, related to Figure 3.
\end{flushleft}
\pagebreak


\subsubsection*{Sources and Sinks}
\doublespacing 

Sources and sinks 
% HN 2010-11-24: Yes, I can see they can be considered as "annotations"
% in that they are somewhat peripheral to an experiment definition. However,
% they are certainly not void of sematic content relevant to the experiment
% as such! Cf the discussion of idempotency for example. I suggest simply
% not saying anything about the "nature" of sourecs and sinks here.
%
% are annotations to a program that 
%
link the functional equations with the physical world. We stress that they are
not expressions, do not evaluate to values (unlike all other
constructs), and can only be used at the top level. What happens if the
same source is observed more than once in a description of an
experiment?  If the source refers to a single, physical input port
such as a channel of an analog-to-digital converter, the result will
necessarily be the same, because the same entity is being observed
within a single run of the experiment. Such sources are called
\emph{idempotent}.  Idempotency ensures that separate experiments
referring to a common external variable can be composed easily with a
predictable outcome. However, there are other kinds of sources,
notably random sources as discussed below, where idempotency is
\emph{not} desirable. Each occurrence of a non-idempotent source is
thus a separate, independent source, even if the name of the source
and the parameters happen to be the same.

What happens if the same \emph{sink} is defined more than once? One could
imagine combining the defining signals in various ways. For example, in the
case of a simple numerical signal, they could simply be added, mirroring
superposition of waves in the physical world. However, as our signals are more
general, it is not always clear what the appropriate notion of ``addition''
should be. For example, if we have signals carrying images, and we wish to
output these to a single graphical display, it is likely that we also need to
describe aspects such as which one should be ``on top''. Thus, for flexibility
and clarity, combination of output signals has to be described explicitly, and
it is an error to define a sink more than once in an experimental description.

There are also operations in experiments that are not related to real-world
observation or to purely functional computation. One example is sampling from
probability distributions. We have implemented sources corresponding to common
parametrised probability distributions, such that experiments can sample
values from the distributions and use these values in computations or
connect them to sinks. However, these sources are \emph{not} idempotent as it
is important that there are no accidental correlations. Sharing of a single random
signal, when needed, can  be described by binding that signal to a
variable as discussed above and using the variable to refer to the signal
instead of repeating the reference to the random source. In this more general
view, sources and sinks bridge referentially transparent and non-transparent
computations.



\end{document}