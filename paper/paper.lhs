\documentclass[11pt]{article}
%include lhs2TeX.fmt
%include lhs2tex-braincurry-preamble
\usepackage[a4paper, top=2.5cm, bottom=2.5cm, left=2.5cm, right=2.5cm]{geometry}
\usepackage{amsmath, amsthm, amssymb}
\usepackage{setspace}
\usepackage{verbatim}
\usepackage[final]{pdfpages}
\usepackage{natbib}
%\usepackage{citesupernumber}

\usepackage{graphicx}
%\linenumbers
%\usepackage{epsfig}
\doublespacing \title{A formal mathematical framework for
  physiological observations, experiments and analyses}

\author{Thomas A. Nielsen, Henrik Nilsson and Tom Matheson}
\pagestyle{myheadings}
\markboth{A formal framework for experiments in physiology}{A formal framework for experiments in physiology}
\begin{document}
\begin{titlepage}

\vspace{50 mm}
\begin{center}{\LARGE {\bf A formal mathematical framework for
  physiological observations, experiments and analyses}}
\end{center}
\vspace{50 mm}

\begin{center}{\large Thomas A. Nielsen$^{1}$, Henrik Nilsson$^2$ and Tom Matheson$^{1*}$}
\end{center}
\vspace{27 mm}

\begin{flushleft}
RUNNING TITLE: A formal framework for experiments in physiology
\end{flushleft}

\vspace{27 mm}


\begin{flushleft}
1. Department of Biology, University of Leicester, University Road, Leicester LE1 7RH

2. School of Computer Science, University of Nottingham, Jubilee Campus, Nottingham NG8 1BB
\vspace{50 mm}

$^*$ To whom correspondence should be sent (tm75@@le.ac.uk)

\end{flushleft}

\end{titlepage}
\input{absintro}

\section*{Results}

To introduce the calculus of physiological evidence (CoPE), we first
define some terminology and basic concepts. We assume that \emph{time}
is global and is represented by a real number, as in classical
physics. An \emph{experiment} is an interaction between an observer
and a number of organisms during a defined time period. An experiment
consists of one or more \emph{trials}: non-overlapping time periods
during which the observer is running a \emph{program} --- instructions
for manipulating the environment and for constructing mathematical
objects, the \emph{observations}. The \emph{analyses} are further
programs to be run during or after the experiment that construct other
mathematical objects pertaining to the experiment. In the sections
that follow, we give precise definitions to these concepts in terms
borrowed from programming language theory and type theory. We provide
a gentle introduction to those concepts for a general audience.

\subsubsection*{Type theory for physiological evidence}

What kinds of mathematical objects can be used as physiological
evidence? We answer this question within simple type theory
\citep{Pierce2002, Hindley2008}, which introduces an intuitive
classification of mathematical objects by assigning to every object
exactly one \emph{type}. These types include base types, such as
integers |Integer|, real numbers |Real|, text strings |String| and the
Boolean type |Bool| with the two values |True| and |False|. These base
types are familiar to users of most programming languages. In
addition, modern type systems, including simple type theory, allow
types to be arbitrarily combined in several ways. For instance, if
|alpha| and |beta| are types, the type |alpha times beta| is the pair
formed by one element of |alpha| and one of |beta|; |[alpha]| is a
list of |alpha|s; and |alpha -> beta| is the type of functions that
calculate a value in the type |beta| from a value in |alpha|. The
ability to write flexible type schemata and generic functions
containing type variables ($\alpha, \beta, \ldots$), which can later
be substituted with any concrete type, is called ``parametric
polymorphism''\citep{Pierce2002} and is essential to the simplicity
and flexibility of CoPE.

We distinguish three type schemata in which physiological evidence can
be values. These differ in the manner in which measurements appear in
a temporal context, but which all derive their flexibility from
parametric polymorphism. \emph{Signals} capture the notion of
quantities that change in time. In physiology, observed time-varying
quantities often represent scalar quantities, such as membrane
voltages or muscle force, but there are also examples of non-scalar
signals such as the two- or three dimensional location of an animal or
of a body part. Here, we generalise this notion such that for
\emph{any} type |alpha|, a signal of |alpha| is defined as a
\emph{function} from time to a value in |alpha|, written formally as:
\begin{code}
Signal alpha = Time -> alpha
\end{code}
For instance, the output of a differential
voltage amplifier might be captured in a |Signal Real|.

To model occurrences pertaining to specific instances in time,
FRP defines events as a list of pairs of time points and values in a
type |alpha|, called the ``tags'':
\begin{code}
Event alpha = [Time times alpha]
\end{code}
For example, an event can be constructed from a number-valued signal
that represents the time of the largest amplitude value of of the
signal, with that amplitude in the tag. Events that do not have a
value of interest to associate with the time point at which it
occurred, can be tagged with the unit type |()| which has only one
element (that is, no information). Events can therefore represent
measurements where the principal information is \emph{when} something
happened, or measurements that concern \emph{what} happened.

A third kind of information describes the properties of whole time
periods. We define a duration of type |alpha| as a set of triples, of
which the first two components denote a start time and an end
time. The last component is again a value of any type |alpha|:
\begin{code}
Duration alpha = [Time times Time times alpha]
\end{code}
Durations are useful for manipulating information about a whole trial
or a single annotation of an entire experiment, but could also be
observations in their own right, such as open times of individual ion
channels, or periods in which activity of a system exceeds a set
threshold (e.g during bursts of action potentials). Lastly, durations
could be used for information that spans multiple trials but not an
entire experiment --- for instance, the presence or absence of a drug.

Since signals, events and durations can be instantiated for any type,
they form a simple but flexible framework for representing many
physiological quantities. We show a list of such examples primarily
drawn from neurophysiology in Table 1. A framework in any type system
that does not support parametric polymorphism would have to represent
these quantities fundamentally differently, thus removing the
possibility of re-using common analysis procedures. Although
parametric polymorphism is conceptually simple and the distinctions we
are introducing are intuitive, common biomedical
ontologies\citep{owlref} \emph{cannot} accommodate these definitions.

\subsubsection*{Calculating with signals and events}

From direct observations, one often needs to process events and
signals, create new events from signals, filter data and calculate
statistics. Here, we formulate these transformations in terms of the
lambda calculus \citep{Church1941}, a family of formal languages for
computation based solely on evaluating functions.  These languages,
unlike conventional programming languages, retain an important
characteristic of mathematics: a term can freely be replaced by
another term with identical meaning.
% HN 2010-09-30: Always "substitute for"
This property \citep[referential transparency][]{Whitehead1927}
% HN 2010-11-10: "enables" is a bit too strong. For example, it is certainly
% *possible* to reason formally about imperative code. 
facilitates algebraic manipulation of and reasoning about
programs \citep{Bird1996}. The lambda calculus allows functions to be
used as first class entities: that is, they can be referenced by
variables and passed as arguments to other functions. On the other
hand, the lambda calculus disallows changing the value of variables or
global states. These properties together mean that the lambda calculus
combines verifiable correctness with a high level of abstraction,
leading to programs that are in 
% HN 2010-11-24: Note: practice is the noun, practise the verb 
practice more concise \citep{Hughes1989} than those written in
conventional programming languages. The lambda calculus or variants
thereof has been used as a foundation for mathematics
\citep{Martin-Lof1985}, classical \citep{Sussman2001} and quantum
\citep{Karczmarczuk2003} mechanics, evolutionary biochemistry
\citep{Fontana1994}, mechanized theorem provers \citep{DeBruijn1968,
  Harrison2009} and functional programming languages
\citep{McCarthy1960}.

In the lambda calculus, calculations are performed by function
abstraction and application. |\x->e| denotes the function with
argument |x| and body |e|, and |f e| the application of the function
|f| to the expression |e| (more conventionally written $f(e)$). For
instance, the function |add2 = \x -> x+2| adds two to its argument;
hence |add2 3 = (\x->x+2) 3 = 3+2| by substituting arguments in the
function body.

We now present the concrete syntax of CoPE, which extends the lambda
calculus with constructs to define and manipulate signals and
events. 
% HN 2010-11-24: Old:
% 
% This calculus borrows some concepts from earlier versions of
% FRP, but it emphasises signals and events as mathematical objects in
% themselves, rather than as control structures for creating reactive
% systems \citep{Elliott1997, Nilsson2002}. 
% 
% I think it's important to make it very clear that CoPE is not FRP:
%
This calculus borrows some concepts from earlier versions of FRP, but
focuses exclusively on signals and events as mathematical objects and
their relations.  It does noy have any control structures for
describing sequences of system configurations, where a signal
expression depends on the occurrence of events \citep{Elliott1997,
  Nilsson2002}, although such constructs may be useful for cell
simulation. As a result, CoPE is quite different from conventional
FRP, which is also reflected in its implementation.

Let the construct |sopen e sclose| denote a signal with the value of
the expression |e| at every time point, and let the construct |<: s
:>| denote the current value of the signal |s| in the temporal context
created by the surrounding |sopen| \ldots |sclose| braces. For
instance,
\begin{code}
sopen 1 sclose
\end{code}
denotes the signal that always has the value 1; and the function |smap|
defined as
\begin{code}
smap = \f -> \s -> sopen f <: s :> sclose
\end{code}
transforms, for any two types |alpha| and |beta|, the signal |s| of |alpha|
into a signal of |beta| by applying the function |f| of type |alpha
-> beta| to the value of the signal at every time point.

The differential operator |D| differentiates a real-valued signal with
respect to time, such that |D s| denotes its first derivative and |D D
s| the second derivative of the signal |s|. When the differential
operator appears on the left-hand side of a definition, it
introduces a differential equation (see Example 2 below).

Events and durations can be manipulated as \emph{lists}. Thus, a large
number of transformations can be defined with simple recursive
equations including filters, folds and scans familiar from functional
programming languages \citep{Hughes1989}. In addition, we have added a
special construct to detect events from existing signals. For
instance, a threshold detector generates an occurrence of an event
whenever the value of a signal crosses a specific level from below.
Here, we generalise the threshold detector to an operator |??| that takes
a predicate (i.e., a function of type |alpha->Bool|), applies it to the
instantaneous value of a signal, and generates an event whenever
the predicate \emph{becomes} true. For instance,
\begin{code}
(\x->x>5) ?? s
\end{code}
denotes the event that occurs whenever the value of the signal |s|
starts to satisfy the predicate |\x->x>5|; i.e., whenever it becomes greater
than 5 after having been smaller. The expression |(\x->x>5)
?? s| thus defines a threshold detector restricted to threshold
crossings with a positive slope.
 
Table S1 in the supplementary information presents an informal overview of
the syntax of CoPE; Table S2 details the types and names of some of
functions we have defined using these definitions.

% \subsubsection{Observing signals and events}
\subsubsection*{Interacting with the physical world}

In the previous examples, signals, events and durations exist as purely
mathematical objects. To describe experiments, however, it must also
be possible to observe particular values from real-world systems, and to
create controlled stimuli to perturb these systems. For this purpose, we
introduce \emph{sources} and \emph{sinks} that act as a bridge between 
purely mathematical equations and the physical world.

A source is an input port through which the value of some external
quantity can be observed during the course of an experiment by binding
it to a variable. If the quantity is time-varying, the bound variable
will denote a signal. For instance, binding a variable to source
denoting a typical analog-to-digital converter yields a signal of real
numbers. However, a source may also refer to a time-invariant quantity.

The construct
\begin{code}
identifier <* source
\end{code}
binds the value or signal resulting from the observation of the
\emph{source} during the course of an experiment to the variable
\emph{identifier}. For a concrete example, the following code defines
a simple experiment:
\begin{code}
v <* ADC 0
\end{code}
This describes the observation of the voltage signal on channel 0 of
an analog-to-digital converter, binding the whole signal to the
variable |v|. We have also used sources to sample value from
probability distributions (see Methods).

In addition to making appropriate observations, an experiment may also
involve a perturbation of the experimental preparation. For example,
the manipulation could control the amount of electric current injected
into a cell. Alternatively, non-numeric signals are used below to
generate visual stimuli on a computer screen. Such manipulations
require the opposite of a source, a \emph{sink}: an output port connected
to a physical device capable of effecting the
desired perturbation. The value at the output at any point in time
during an experiment is defined by connecting the corresponding sink
to a signal.  This is done through the the following construct,
mirroring the source construct introduced above:
\begin{code}
signal *> sink
\end{code}

As a concrete example, suppose we wish to output a sinusoidal
stimulus. We first construct a time-varying signal describing the
desired shape of the stimulus. In this case, we read a clock source
that yields a signal counting the number of seconds since the
experiment started:
\begin{code}
seconds <* clock 
\end{code}
The sine wave can now be defined as:
\begin{code}
sineWave = smap sin seconds
\end{code}
We then write
\begin{code}
sineWave *> DAC 0
\end{code}{}
to send the |sineWave| signal to channel channel 0 of a
digital-to-analog converter. Below we show how
these primitives can be used to define two detailed experiments in
neurophysiology.
% Sinks and sources are thus used to
% link values, which have been or will be used in purely mathematical
% expressions, to the real world. There are also operations in
% experiments that are not related to real-world observation or to
% purely functional computation --- for instance sampling from
% probability distributions, which violates referentially transparancy
% (if $rnd$ is a random number generator with an arbitratry distribution
% parametrised by $\theta$, it is not in general the case that $ rnd
% \theta + rnd \theta = 2*rnd \theta$). We have thus implemented sources
% corresponding to common parametrised probability distributions, such
% that experiments can sample values from these distributions and use
% these values in computations or connect them to sinks. In this more
% general view, sources and sinks bridge referentially transparent and
% non-transparent computations.

\subsubsection*{Example 1}

In locusts, the Descending Contralateral Movement Detector (DCMD)
neuron signals the approach of looming objects to a distributed
nervous system \citep{Rind1992}. We have constructed several
experiments in CoPE to record the response of DCMD to visual stimuli
that simulate objects approaching with different velocities. To
generate these stimuli, we augmented CoPE with primitive
three-dimensional geometric shapes. Let the expression
\begin{code}
cube l
\end{code}
denote a cube located at the origin, with side length |l|,
\begin{code}
translate (x,y,z) s
\end{code}
the shape that results from translating the shape |s| by the
vector |(x,y,z)| and
\begin{code}
colour (r,g,b) s
\end{code}
the shape identical to |s| except with the colour intensity red |r|,
green |g| and blue |b|. Additional constructors can be introduced for
more complex stimuli, but these are sufficient for the experiments
reported here. Since signals are polymorphic, they can carry not just
numeric values but also shapes, so we represent visual stimuli as
values in |Signal Shape|. The looming stimulus consists of a cube of
side length l approaching a locust with constant velocity v. The
time-varying distance from the locust to the cube in real-world
coordinates is a real-valued signal:
\begin{code}
distance = sopen v * (<: seconds :> - 5) sclose 
\end{code}

The |distance| signal is the basis of shape-valued signal
|loomingSquare| representing the approaching square:

\begin{code}
loomingSquare = 
     sopen colour  (0,0,0) 
                   (translate  (0,0, <: distance :> ) 
                               (cube l))               sclose
\end{code}

|loomingSquare| differs from conventional protocols
\citep{Gabbiani2001} for stimulating DCMD in that it describes an
object that passes through the physical screen and the observer, and
when displayed would thus disappear from the screen just before
collision. In order not to evoke a large OFF response
\citep{O'shea1976} immediately after simulated collision, the object
is frozen in space as it reaches the plane of the surface onto which
the animation is projected \citep{Hatsopoulos1995}. To achieve this
effect, we define a new signal that has a lower bound of the distance
from the eye to the visual display screen |zscreen|
\begin{code}
distance' = sopen max zscreen spc <: distance :>  sclose
\end{code}
where |max x y| returns the larger of the two numbers |x| and
|y|. |loomingSquare'| is identical to |loomingSquare| except for the
use of |distance'|.

Finally, |loomingSquare'| is connected to a screen signal sink that
represents a visual display unit capable of projecting
three-dimensional shapes onto a two-dimensional surface.
\begin{code}
loomingSquare' *> screen 
\end{code}
In our experiments, the extracellular voltage from the locust nerves
(connectives), in which the DCMD forms the largest amplitude spike,
was amplified, filtered (see methods) and digitised:
\begin{code}
voltage <* ADC 0
\end{code}
|loomingSquare'| and |voltage| thus define a single object approach
and the recording of the elicited response. This approach was repeated
every 4 minutes, with different values of $\frac{l}{||v||}$. Figure 1
shows $\frac{l}{||v||}$ as values with type |Duration Real|, together
with the |distance'| and |voltage| signals for the first five trials
of one experiment on a common time scale.

The simplest method for detecting spikes from a raw voltage trace is
to search for threshold crossings, which works well in
% HN 2010-11-24: Note: practice is the noun, practise the verb 
practice for calculating DCMD activity from recordings of the locust
connectives \citep{Gabbiani2001}. If the threshold voltage for spike
detection is |vth|, the event |spike| can be calculated with
\begin{code}
spike = tag () ((\v->v>vth) ?? voltage)
\end{code}
where |tag| replaces every tag in some event with a fixed value, so
that spike has type |Event ()|. This event is displayed on the common
time scale in Figure 1. The top row displays the spike rate histogram
\begin{code}
hspike  =
        sopen length (filter  (between  <: delay seconds:> 
                                        <: seconds:> . fst)
                              spikes) sclose
\end{code}
for each trial. This definition exploits the list semantics of events
by using the generic list-processing function |filter| which takes as
arguments predicate |p| and a list |xs|, and returns the list of
elements in |xs| for which the predicate holds. Here the predicate is
|fst| (which returns the first element of a pair, here the occurrence
time) composed (|.|) with the function |between = \x -> \y -> \z ->
z>x && z<=y|, which tests whether the last of three numbers lies
between the first two.

We examined how the DCMD spike response varied with changes in
$\frac{l}{||v||}$. The average of |hspike| for three different values
of $\frac{l}{||v||}$ are shown in Figure 2A; 2B and 2C show the total
number of spikes (|length spike|) and largest value of |hspike|, for
each approach, plotted against the value of $\frac{l}{||v||}$
\citep{Hatsopoulos1995}.

This experiment demonstrates that the calculus of physiological
evidence can adequately and concisely describe visual stimuli, spike
recording and relevant analyses for activation of a locust looming
detection circuit (see supplementary information for full code
listings.) To demonstrate the versatility of this framework, we next
show that it can be used to implement dynamic clamp in an \emph{in
  vivo} patch clamp recording experiment.

\subsubsection*{Example 2}

Dynamic clamp experiments\citep{Robinson1993, Sharp1993} permit the
observation of real neuronal responses to added simulated ionic
conductances; for instance, a synaptic conductance or an additional
Hodgkin-Huxley type voltage-sensitive membrane conductance. A dynamic
clamp experiment requires that the current injected into a cell is
calculated at every timepoint based on the recorded membrane
potential. Here, we use CoPE to investigate the effect of an A-type
potassium conductance \citep{Connor1971} on the response of a zebrafish
spinal motor neuron to synaptic excitation.

The output current
%
% HN 2010-11-24: I agree that one might call the syntactic category
% that encompass both equations and source/sink bindings "command" for
% want of a better term, but I think that talking about "commands"
% here is a bit confusing and might give the wrong impressions (imperative
% connotations). In any case, the notion of "command" has not been discussed
% before.
%
% command
|i| is calculated at each time-step from the simulated conductance |g|
and the measured membrane voltage |v|:
\begin{code}
v <* ADC 0
i = sopen (<: v :> - E)* <: g :> sclose
i *> DAC 0
\end{code}
The experiment is thus characterised by the conductance signal $g$
(for clarity, here we omit the amplifier-dependent input and output
gains).

In the simplest case, $g$ is independent of $v$; for instance, when
considering linear synaptic conductances \citep{Mitchell2003}. We
first consider the addition of a simulated fast excitatory synaptic
conductance to a real neuron. Simple models of synapses approximate
the conductance waveform with an alpha function
\citep{Carnevale2006}:
\begin{code}
alpha_f = \amp -> \tau -> sopen tau **2 * <: seconds :> *exp (- <: seconds :> *tau) sclose
\end{code}

To simulate a barrage of synaptic input to a cell, this waveform is
convolved with a simulated presynaptic spike train. The spike train
itself is first bound from a source representing a random probability
distribution, in this case series of recurrent events of type |Event
()| for which the inter-occurrence interval is Poisson distributed.
Our standard library contains a function |convolveSE| which
convolves an impulse response signal with a numerically-tagged event,
such that the impulse response is multiplied by the tag before
convolution.
\begin{code}
preSpike <* poissonTrain rate
gsyn =  convolveSE (alpha_f amp tau) (tag 1 preSpike)
\end{code}
The signal |gsyn| could be used directly in a dynamic clamp experiment
using the above template (i.e. |g=gsyn|). Here, we will examine other conductances
that modulate the response of the cell to synaptic excitation.

Here, we will examine additional conductances. Both the subthreshold
properties of a cell and its spiking rate can be regulated by active
ionic conductances, which can also be examined with the dynamic
clamp. In the Hodgkin-Huxley formalism for ion channels, the
conductance depends on one or more state variables, for which the
forward and backward rate constants depend on the membrane
voltage. We show the equations for the activation gate of an
A-type potassium current (\citealt{Connor1971}; following \citealt{Traub1991}, but
using SI units and absolute voltages). The equations for
inactivation are analogous (see Listing 2 in supplementary information).

We write the forward and backward rates as functions of the membrane voltage

\begin{tabbing}
\qquad\=\hspace{\lwidth}\=\hspace{\cwidth}\=\+\kill
${\alpha_a\mathrel{=}\lambda \Varid{v}\to \frac{\mathrm{20}\!\cdot\!(\mathbin{-}\mathrm{46.9}\mathbin{-}\Varid{v}\!\cdot\!\mathrm{1000})}{\Varid{exp}\;\frac{\mathbin{-}\mathrm{46.9}\mathbin{-}\Varid{v}\!\cdot\!\mathrm{1000}}{\mathrm{10}}\mathbin{-}\mathrm{1}}}$\\
\\
${\beta_a\mathrel{=}\lambda \Varid{v}\to \frac{\mathrm{17.5}\!\cdot\!(\Varid{v}\!\cdot\!\mathrm{1000}\mathbin{+}\mathrm{19.9})}{\Varid{exp}\;\frac{\Varid{v}\!\cdot\!\mathrm{1000}\mathbin{+}\mathrm{19.9}}{\mathrm{10}}\mathbin{-}\mathrm{1}}}$
\end{tabbing}

%
%\begin{code}
%alphaa = \v->  20*(-46.9-v*1000)/(exp ((-46.9-v*1000)/10) -1)
%betaa = \v->   17.5*(v*1000+19.9)/(exp ((v*1000+19.9)/10) -1)
%\end{code}

The time-varying state of the activation gate is given by a
differential equation. We use the notation |D x = sopen f
(x,<:seconds:>) sclose | to denote the ordinary differential equation
that is conventionally written $\frac{dx}{dt} = f(x,t) $ with starting
conditions explicitly assigned to the variable $x_0$. The
differential equation for the activation variable $a$ is
\begin{code}
D a = sopen  alphaa <: vm :> * (1- <:a:> ) -
             betaa <: vm :> * <: a :> sclose
a_0 = 0
\end{code}
The inactivation state signal |b| is defined similarly.

The current signal from this channel is calculated from Ohm's law:
\begin{code}
ika = sopen gmaxk * <:a:> * <:b:> * (<:v:> - E) sclose
\end{code}
This is added to the signal |i| defined above to give the output current
% command, 
thus completing the definition of this experiment:
\begin{code}
i + ika *> DAC 0
\end{code}

Figure 3A and 3B shows the voltage response to a unitary synaptic
conductance and a train of synaptic inputs, respectively, with |gmaxk|
ranging from 0 to 100 nS. By varying the value of |rate|, we can
examine the input-output relationship of the neuron by measuring the
frequency of postsynaptic spikes. Spikes were detected from the first
derivative of the |v| signal with
\begin{code}
spike = tag () ((\v'->v'>vth') ?? D v)
\end{code}
and the spike frequency calculated with the |frequencyDuring| function.
This relationship between the postsynaptic spike frequency and the
simulated synaptic input |rate| is plotted in Figure 3C for four
different values of |gmaxk|. 
\input{discuss}

\bibliographystyle{apalike}
\bibliography{paper}

\pagebreak

\section*{Table 1}

\begin{tabular}{l  l}
\hline
  Quantity & Type \\ 
\hline
  Voltage across the cell membrane & |Signal Real| \\
  Ion concentration & |Signal Real| \\
  Animal location in 2D & |Signal (Real times Real)| \\
  Action potential & |Event ()| \\
  Action potential waveforms & |Event (Signal Real)| \\
  Spike detection threshold & |Duration Real| \\
  Spike interval & |Duration ()| \\
  Synaptic potential amplitude & |Event Real| \\
  Drug present & |Duration ()| \\
  Trial with parameter |alpha| & |Duration alpha| \\
  Visual stimulus & |Signal Shape| \\
  Lab notebook & |Event String| \\
\hline
\end{tabular}

\pagebreak

\section* {Figure and Table Legends}

\textbf{Figure 1}. Diagram of an experiment to record the looming
response from a locust DCMD neuron, showing the first five recorded
trials from one animal. Experiment design: \emph{blue lines},
simulated object size-to-approach speed ratio ($\frac{l}{||v||}$) for
given approach trial, \emph{red lines}, simulated object distance,
\emph{red triangles}, apparent collision time. Observed signal:
\emph{black lines}, recorded extracellular voltage. The largest
amplitude deflections are DCMD spikes. Analysis: \emph{green dots},
DCMD spikes, with randomly jittered vertical placement for display,
\emph{thin black line}, spike rate histogram with 50 ms bin size. The
inter-trial interval of four minutes is not shown.

\flushleft \textbf{Figure 2}. A, Spike rate histograms for approaches with
$\frac{l}{||v||}$ of 0.01, 0.02 and 0.04 s, with 50 ms bin size, with
collision time indicated by a black triangle. B, Scatter plot of
number of counted spikes against approach $\frac{l}{||v||}$ for
individual trials. C, Scatter plot of the maximum rate of spiking
against $\frac{l}{||v||}$ for individual trials. N=1 animal, 272
approaches.

\flushleft \textbf{Figure 3}. A, recorded intracellular voltage following
conductance injections of a unitary simulated synaptic conductance, in
the presence of A-type potassium conductances of increasing magnitude
(values given are for the maximal conductance $g_A$). B, as A, but
with a simulated presynaptic spike train with inter-spike intervals
drawn from a Poisson distribution (here a mean of $120 s^{-1}$; the spike
trains used to test the different levels of A-type conductance are
identical). C, the postsynaptic spike rate plotted against the rate of
simulated presynaptic inputs, with $g_A$ as in A.


%\includepdf[pages=-]{Figure1.pdf}
%\includepdf[pages=-]{FigureDyn.pdf}
%\includepdf[pages=-]{Figure4.pdf}

%\begin{comment}
%\pagebreak
%\vskip1ex 

\flushleft \textbf{Table 1}. Representation of physiological
observations and quantities in CoPE

%\includepdf[pages=-]{supplement.pdf}

\end{document}
 
