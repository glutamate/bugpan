\documentclass[11pt]{article}
%include lhs2TeX.fmt
%include lhs2tex-braincurry-preamble
\usepackage[a4paper, top=2.5cm, bottom=2.5cm, left=2.5cm, right=2.5cm]{geometry}
\usepackage{amsmath, amsthm, amssymb}
\usepackage{setspace} 
\usepackage{verbatim} 
\usepackage[final]{pdfpages}
\usepackage{natbib}
\usepackage{graphicx}
%\linenumbers
%\usepackage{epsfig}
\doublespacing \title{A formal mathematical framework for
  physiological observations, experiments and analyses} \author{Thomas
  A. Nielsen, Henrik Nilsson and Tom Matheson}
\begin{document}

\maketitle

\input{absintro}

\section*{The calculus of physiological evidence}

Examining the temporal evolution of observed quantities, such as
intrinsic rhythms or responses to external stimuli, is ubiquitous in
physiology. Often these observations are mediated by actions that are
said to happen at a certain time point - such as action potentials or
secretion events - but are themselves manifestations of continuous
changes in ion channel conductances or fusion pore dilations on a
different timescale. Time must play a multifaceted role in
physiological evidence.

Time also plays an essential role in some computer programs such as
animations, video games, robotics and physical simulations. Although
these time-dependent processes can be implemented in a conventional
programming language, they can also be composed in a referentially
transparent manner by combining elementary transformations of the
whole input, including responses from a user, into the entire output
from the program. Functional Reactive Programming
\citep[FRP;][]{Elliott1997, Nilsson2002} is one such computational
paradigm where these transformations can be defined by purely
mathematical functions. FRP introduces two types of values to place
information in a temporal context: signals, which represent
continuously varying quantities, and events, which represent distinct
occurrences. These types are flexible, in that they can carry
information of any other type. We show that these definitions of
signals and events permit the encoding of many kinds of physiological
evidence. Thus, definitions of experiments can be described concisely
in an FRP-like language based on the composition and calculuation of
signals and events.  We begin by describing the types that are central
to FRP, showing how physiological quantities can be captured in
them. We then describe how values of these types can be observed, and
how fuctions can be used to refine existing observations or generate
stimuli for experiments.

First, we introduce some terminology and basic concepts. We assume
that \emph{time} is global and is represented by a real number, as in
classical physics. An \emph{experiment} is an interaction between an
observer and a fixed number of organisms during a defined time
period. An experiment consist of one or more \emph{trials},
non-overlapping time periods during which the observer is running a
\emph{program} --- instructions for manipulating the environment and
for constructing mathematical objects, the \emph{observations}. The
\emph{analyses} are further programs to be run after the experiment
that construct other mathematical objects pertaining to this
experiment.

\subsubsection*{Type theory for physiological evidence}

What kinds of mathematical objects can count as physiological
evidence? We answer this question within simple type theory
\citep{Pierce2002}, which assigns to every object a \emph{type}. These
types include base types, such as integers |Integer|, real numbers
|Real|, text strings |String| and the boolean type |Bool| with the two
values |True| and |False|. In addition, types can be arbitrarily
combined in several ways, such that if |alpha| and |beta| are types,
the type |alpha times beta| is the pair formed by one element of
|alpha| and one of |beta|; |[alpha]| is a list of |alpha|s; and |alpha
-> beta| is the type of functions that calculate a value in the type
|beta| from a value in |alpha|. Here, we use the convention that greek
letters stand for type variables, which can be substituted by any
concrete type, such as a base type or a type combination. Types can be
defined with references to arbitrary types; for instance,
|withIntegers alpha = [Integer times alpha]| denotes for any type
|alpha| the list of pairs of integers and |alpha|s. The hole |alpha|
in the type definition can then be filled in to form a concrete type,
for instance |withIntergers String|. The ability to build flexible
type schemata in this manner and define generic functions over them
\citep[``parametric polymorphism'';][]{Pierce2002} is essential in our
calculus for representing a large range of physiological quantities
with a small number of concepts, as we show in this section.

What, then, are the types in which physiological evidence can be
values? We distinguish three type schemas that differ in the manner in
which measurements appear in a temporal context, but all derive their
flexibility from parametric polymorphism. \emph{Signals} capture the
notion of quantities that change in time. In physiology, observed
time-varying quantities often represent scalar quantities, such as
membrane voltages or muscle force, but there are also examples of
non-scalar signals such as the two- or three dimensional location of
an animal or of a body part. Here, we generalise this notion such that
for \emph{any} type |alpha|, a signal of |alpha| is defined as a
function from time to a value in |alpha|, written formally as:
\begin{code}
Signal alpha = Time -> alpha
\end{code}
Signals can thus take a new value for every different time point and
represent quantities that vary continuously, although these values may
be piecewise constant. For instance, the output of a differential
voltage amplifier might be captured in a |Signal Real|.

But not every physiological observation denotes continuous
change. Some measurements are derived from an instant in time --- such
as the peak amplitude of an electrical potential --- and others pertain
to an extended period of time. These qualitatively different classes
of observations are represented by \emph{events} and \emph{durations},
respectively. 

To model discrete occurences, FRP introduced events as a list of pairs
of time points and a value in a type |alpha|, called the ``tag'':
\begin{code}
Event alpha = [Time times alpha]
\end{code}
For example, an event could be constructed from a scalar signal such
that the time of the largest amplitude of a signal was associated with
the signal amplitude at that time-point. Events that do not have a
value of interest to associate with the time point at which it
occurred, can be tagged with the unit type |()| which has only one
element (that is, no information). Therefore, events can represent
both measurements where the principal information is \emph{when}
something happend, and measurements concerning \emph{what} happened.

A third kind of information describes the properties of whole time
periods. We define a duration of type |alpha| as a set of triples, of
which the first two components denote a start time and an end
time. The last component is again a value of any type |alpha|:
\begin{code}
Duration alpha = [Time times Time times alpha]
\end{code}
Durations are useful for information about a whole trial or about an
entire experiment, but could also be observations in their own right,
such as open times of individual ion channels, or periods in which
activity of a system exceeds a set threshold (e.g bursts). We have
used durations to hold information about an entire experiment, for
instance a session identifier or the animal strain. In that case, the
duration set contains a single element, with the start and end of the
experiement as start and end time, respectively. Lastly, durations
could be used for information that spans multiple trials but not an
entire experiment - for instance, the presence of a drug.

Since signals, events and durations can be instantiated for any type,
they form a simple but flexible framework for representing many
physiological quantities. We show a list of such examples primarily
drawn from neurophysiology in Table 1. These quantities are all
representable by signals, events or durations but with different
instantiations of the free type variable. A framework in a type
systems that does not support parametric polymophism would have to
represent these quantities fundamentally differently, thus removing the
possibility of re-using common analysis procedures.\vskip1ex
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
\hline
\end{tabular}
\vskip1ex 

Some of these quantities are directly observed from equipment such as
amplifiers or electronic detectors, but may need to be conditioned
before any conclusions can be drawn from them. Other quantities can
only be inferred from calculations on other observations. First we
show how to build programs that calculate with signals and events;
then we show how annotations allow these programs to interact with
external systems and measure their behaviour.

\subsubsection*{Calculating with signals and events}

From direct observations, one often needs to process events and
signals, create new events from signals, filter data and calculate
statistics. Here, we formulate these transformations in terms of the
lambda calculus \citep{Church1941}, a formal language for
referentially transparent computation based on evaluating purely
mathematical functions. The lambda calculus allows functions to be
used as first class entities: that is, they can be referenced by
variables and passed as arguments to other functions (which then
become higher-order functions). On the other hand, the lambda calculus
excludes variable or state mutation. These properties together mean
that the lambda calculus combines verifiable correctness with a high
level of abstraction, leading to programs that are in practise more
concise \citep{Hughes1989}. The lambda calculus or variants thereof
has been used as a foundation for mathematics \citep{Martin-Lof1985},
classical \citep{Sussman2001} and quantum \citep{Karczmarczuk2003}
mechanics, evolutionary biochemistry \citep{Fontana1994} and
programming languages \citep{McCarthy1960}.

In the simple lambda calculus, calculations are performed by function
abstraction and application. |\x->e| denotes the function with
argument |x| and body |e|, and |f y| the application of the function
|f| to the value |y|. For instance, the function |add2 = \x -> x+2|
adds two to its argument; hence |add2 3 = (\x->x+2) 3 = 3+2| by
substituting arguments in the function body. In addition we define a
number of constructs to improve the readability of the language,
although strictly they can be defined in terms of function application
and abstraction. The expression |if p then c else a| equals |c| if |p|
is |True| and |a| if |a| is |False|. Similarly, |let y = e in w|
defines a variable |y| with the value of the expression |e| (in which
|y| is in scope to allow recursive definitions) that can be used as a
value in the expression |w|.

Here, we present a concrete syntax for a new lambda calculus extended
with signals and events based on the lambda calculus. This language
borrows some concepts from the previous implementations of FRP, but it
emphasises signals and events as mathematical objects in themselves
rather than as control structures for creating reactive systems
\citep{Elliott1997, Nilsson2002}. The syntax and implementation
strategy is therefore very different from FRP.

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
transforms, for any two types |alpha| and |beta|, a signal of |alpha|
into a signal of |beta| by applying the function |f| of type |alpha
-> beta| to the value of the signal at every time point.

Further primitives are needed to form signals that depend on the
history of other signals. For instance, the differential operator |D|
differentiates a real-valued signal with respect to time, such that 
|D s| denote its first derivative and |D D s| the second derivative of the
signal |s|. Likewise, the differential operator can appear on the left
side of a definition, in which case it introduces a differential
equation by pattern matching (REF) on the derivative of a signal (see
example 2 below).

In addition, the expression |delay s| denotes the signal that is
delayed by a small amount of time. Other FRP implementations have
other primitives, in particular a |switch| statement that changes the
definition of a signal depending on the occurrence of specific
events. We have not needed such a construct in the experiments
described here.

Events and durations are defined as lists and can be manipulated and
constructed as such. Thus, a large number of transformations can be
defined with simple recursive equations including filters, folds and
scans familiar from functional programming languages
\citep{Hughes1989}.

In addition, we have added a special construct to detect events from
existing signals. For instance, a threshold detector generates an
occurrence in an event whenever the value of a signal exceeds a set
level (and then not again before the value of the signal has decreased
below that level and then reached it again.)  Here, we generalise the
threshold detector slightly by taking a predicate (i.e., a function of
type |alpha->Bool|) on the instantaneous values of the signal and
generate an event whenever the predicate becomes true using the |??|
operator. For instance,
\begin{code}
(\x->x>5) ?? s
\end{code}
denotes the event that occurs whenever x crosses the threshold level
|5| with a positive, but not negative, slope. 

This small number of special constructors, along with the lambda
calculus and the list semantics of events and durations, have allowed
us to construct a small ``standard library'' of analysis procedures
for physiology. Table S1 details the types and names of the functions
in this library.

\subsubsection*{Observing signals and events}

In the previous examples, signals, events and durations exist as
purely mathematical objects. In order to describe experiments, it must
also be possible to observe particular values from real-world systems, and
to create controlled stimuli to perturb these systems. For this
purpose, we introduce \emph{sources} and \emph{sinks} that bridge
variables in purely mathematical equations with the physical world.

The construct
\begin{code}
identifier <* source parameter
\end{code}
binds the signal yielded by a parametrised \emph{source} to
the variable \emph{identifier}. This variable will hold the
whole signal observed during the course of the experiment. The signal
source binding construct defines  a simple experiment:
\begin{code}
v <* ADC 0
\end{code}
which describes the observation of the voltage signal on channel 0 of
an analog-to-digital converter.

In addition to making appropriate observations, an experiment may also
involve a perturbation of the experimental preparation. To create a
stimulus for an external system, we first construct time-varying signals as
described above; for instance a sine wave. To build such a signal, we
start with a clock signal that counts the number of seconds since the
experiment started, which can be read from a clock source
\begin{code}
seconds <* clock ()
\end{code}
which is paramtrised by the unit type (i.e. no information). The sine
wave can then be defined with

\begin{code}
sineWave = smap sin seconds
\end{code}

Connecting this signal to the real world requires the opposite of a
signal source, namely a signal sink. To send the sineWave signal to a
channel of a digital-to-analog converter, we write

\begin{code}
sineWave *> DAC 0
\end{code}

In the context of a physiology experiment, these declarations can for
instance control the amount of current injected in a cell. Below,
non-numeric signals and signal sinks are used to generate visual
stimuli on a computer screen. Sinks and sources are thus be used to
link values, which have been or will be used in purely mathematical
expressions, to the real world. There are also operations in
experiments that are not related to real-world observation or to
purely functional computation --- for instance sampling from
probability distributions, which violates referentially transparancy
(if $rnd$ is a random number generator with an arbitratry distribution
parametrised by $\theta$, it is not in general the case that $ rnd
\theta + rnd \theta = 2*rnd \theta$). We have thus implemented sources
corresponding to common parametrised probability distributions. In
this more general view, sources and sinks bridge referentially
transparent and non-transparent computations.

\section*{Example 1}

Most animals can benefit from a mechanism for detecting and avoiding
obstacles and predators. In addition, movement in social animals might
be constrained by the need to avoid collisions with conspecifics. A
common component in such species is a visual detector for looming
objects. In locusts, a single neuron in each brain hemisphere, the
Lobular Giant Movement Detector (LGMD), responds preferentially to
looming stimuli \citep{Rind1992}. The response of the LGMD is
invariant to manipulations of many apsects of the looming
stimulus. For instance, a key property, the time of the peak firing
rate with respect to the retinal angle of the looming stimulus, is
insensitive to the colour, texture, size, velocity and azimuth of the
approaching object when averaged over several approaches
\citep{Gabbiani2001}.

We have constructed several experiments in the calculus of
physiological evidence to record the response of LGMD in locusts to
visual stimuli that simulate objects approaching with different
velocities. To generate these stimuli, we augmented the calculus of
physiological evidence with primitive three-dimensional geometric
shapes. Let the expression
\begin{code}
cube l
\end{code}
denote a cube located at the origin with the side length |l|,
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
                               (cube l)) sclose
\end{code}

|loomingSquare| differs from conventional protocols
\citep{Gabbiani2001} for stimulating the LGMD in that the object
appears to pass through the observer after collision. In order not to
evoke a large OFF response from the LGMD \citep{O'shea1976}
immediately after simulated collision, the object is frozen in space
as it reaches the plane of the surface onto which the animation is
projected \citep{Hatsopoulos1995}. To achieve this effect, we define a
new signal that has a lower bound of the distance from the eye to the
screen |zscreen|
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
loomingSquare' *> screen ()
\end{code}

The response to the looming stimulus in the LGMD neuron can be
recorded from the main longitudinal nerves (``connectives'') in the
ventral nerve cord. Although the LGMD does not make a long-range
projection, it reliably activates the descending contralateral
movement detector (DCMD) with a strong synaptic connection, such that
spikes in the DCMD follow LGMD spikes one to one
\citep{O'Shea1974}. Extracellular hook electrodes wrapped around one
connective can record activity in the contralateral DCMD, which produces the
largest-amplitude action potential in such recordings. These analogue
signals were amplified, filtered (see methods) and converted to a
digital signal:

\begin{code}
voltage <* ADC 0
\end{code}

|loomingSquare'| and |voltage| thus define a single approach and the
recording of the elicited response. This approach was repeated every 4
minutes, with different values of $\frac{l}{v}$. Figure 1 shows
$\frac{l}{v}$ as values with type |Duration Real|, together with the
|distance'| and |voltage| signals for the first five trials of one
experiment on a common time scale.

The simplest method for detecting spikes from a raw voltage trace is
to search for threshold crossings, which works well in practise for
calculating DCMD activity from recordings of the locust connectives
\citep{Gabbiani2001}. If the threshold voltage for spike detection is
|vth|, the event |spike| can be calculated with
\begin{code}
spike = (\v->v>vth) ?? voltage
\end{code}
Which yields a value of type |Event Real|, where the tag of each event
holds the voltage at which the threshold was crossed. The value of
this tag is likely to be close to the threshold |vth| and holds little
relevant information. Therefore, replacing each tag with the unit type
|()|, such that |spike| has type |Event ()| is a more meaningful
representation of the spike train. The function |tag| conveniently
replaces every tag in some events with a fixed value.
\begin{code}
spike = tag () ((\v->v>vth) ?? voltage)
\end{code}
so that |spike| has type |Event ()|. This event is displayed on the
common time scale in Figure 1. The top row displays the spike rate
histogram
\begin{code}
hspike  =
        sopen length (filter  (between <: delay seconds:> <: seconds:> . fst)
                              spikes) sclose
\end{code}
for each trial. This definition exploits the list semantics of events
by using the generic list-processing function |filter| which takes as
arguments predicate |p| and a list |xs|, and returns the list of
elements in |xs| for which the predicate holds. Here the predicate is
|fst| (which returns the first element of a pair, here the occurrence
time) composed (|.|) with the function |between = \x -> \y -> \z ->
z>x && z<=y|. 

We examined how the LGMD spike response varied with changes in
$\frac{l}{v}$. The average of |hspike| for three different values of
$\frac{l}{v}$ are shown in figure 2A, and 2B and 2C show the total
number of spikes (|length spike|) and largest value of |hspike|, for
each approach, plotted against the value of $\frac{l}{v}$. These plots
show that while the peak firing rate is a decreasing
function of $\frac{l}{v}$, the total number of spikes in the approach
is an increasing function. In addition, the time of the peak rate is
later with smaller values of $\frac{l}{v}$
\citep{Hatsopoulos1995}. 

This experiment indicates that the calculus of physiological evidence
can adequately and concisely describe the visual stimulus, spike
recording and relevant analysis for activation of the locust looming
detection circuit. To demonstrate the versatility of this framework,
we now show that it can be used to implement dynamic clamp in an
intracellular recording experiment.

\section*{Example 2}

The input-output relationship of individual neurons are fundamental to
the functioning of neuronal networks. Given a stimulus, e.g. pattern
of synaptic input or injected current waveform, what is the membrance
voltage trajectory and firing rate response of a neuron? In
particular, cell properties such as the dendritic morphology or ionic
conductances can profoundly influence this relationship. Such
influences can be examined with experiments or simulations; here we
show how the calculus of physiological evidence can be used to
formulate and execute dynamic-clamp experiments on synaptic
integration.

A dynamic clamp experiment requires electrical access to the
intracellular compartment, such that the cell membrane voltage can be
recorded and current injected into the cell. As opposed to a standard
current-clamp experiment, where the injected current waveform is known
in advance, in the dynamic clamp setup the injected current command is
calculated near-instantaneously from the membrance voltage. Unlike the
standard current clamp configuration, the dynamic clamp permits the
imposition of additional simulated ionic conductances on a real
neuron. For instance, it is possible to record the responce of a cell
to an added a synaptic conductance or an additional Hodgkin-Huxley
style voltage-sensitive membrane conductance. Here, we combine these
possibilities to investigate the effect of an A-type potassium
conductance \citep{Connor1971} on the response of a zebrafish spinal
motor neuron to synaptic excitation.

Dynamic clamp-experiments follow the same template: the current
command |i| is calculated at each time-step from the simulated conductance |g|
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
considering linear synaptic conductances \citep{Mitchell2003}. Here,
we consider the addition of a simulated fast excitatory synaptic
conductance to a real neuron. Simple models of synapses
approximate the conductance waveform with an alpha function.
\begin{code}
alpha = \tau -> sopen tau **2 * <: seconds :> *exp (- <: seconds :> *tau) sclose
\end{code}

To simulate a barrage of synaptic input to a cell, this waveform is
convolved with a presynaptic spike train. The spike train itself is
first bound from a source representing a random probability
distribution, in this case series of recurrent events of type |Event
()| for which the inter-occurrence interval is Poisson distributed.
Secondly, our standard library contains a function |convolveSE| which
convolves an impulse response signal with a numerically-tagged event,
such that the impulse response is multiplied by the tag before
convolution.
\begin{code}
preSpike <* poissonTrain rate
gsyn =  convolveSE (alpha amp tau) (tag 1 preSpike)
\end{code}
The signal |gsyn| could be used directly in a dynamic clamp experiment
using the above template. Here, we will examine other conductances
that modulate the response of the cell to synaptic excitation.

Both the subthreshold properties of a cell and its spiking rate can be
regulated by active ionic conductances. One way to examine this
regulation of synaptic integration is to impose an additional active
conductance on cells with dynamic clamp. In the Hodgkin-Huxley
formalism for ion channels, the conductance depends on one or more
state variables, for which the forward and backward rate constants
depend on the membrane voltage. Here we show the equations for the
activation gate of an A-type potassium current \citep{Connor1971},
following \citet[we use SI units and absolute voltages]{Traub1991}. The
equations for inactivation are analogous.

We write the forward and backward rates as functions of the membrane voltage
\begin{code}
alphaa = \v->  20*(-46.9-v*1000)/(exp ((-46.9-v*1000)/10) -1)
betaa = \v->   17.5*(v*1000+19.9)/(exp ((v*1000+19.9)/10) -1)
\end{code}

The time-varying state of the activation gate is given by a
differential equation. We use the notation |D x = sopen f
(x,<:seconds:>) sclose | to denote the ordinary differential equation
that is conventionally written $\frac{dx}{dt} = f(x,t) $ with starting
conditions explicitly assigned to the variable $x_0$.
\begin{code}
D a = sopen  alphaa <: vm :> * (1- <:a:> ) -
             betaa <: vm :> * <: a :> sclose
a_0 = 0
\end{code}
with the inactivation state signal |b| defined similarly.

The current signal from this channel is calculated from Ohm's law:
\begin{code}
ika = sopen gmaxk * <:a:> * <:b:> * (<:v:> - E) sclose
\end{code}
which is added to the signal |i| defined above that drives the current
command, completing the definition of this experiment. 

Figure 3A and 3B shows the voltage response to a unitary synaptic
conductance and a trains of synaptic inputs, respecitively, with
|gmaxk| ranging from 0 to 100 nS. A large A-type membrane conductance
decreases the amplitude of the EPSP, as expected, and decreases the
number of spikes in response to the injection of an identical synaptic
conductance waveform.

By varying the value of |rate|, we can examine the input-output
relationship of the neuron by measuring the frequency of postsynaptic
spikes. Firstly, spikes were detected from the first derivative of the
|v| signal with
\begin{code}
spike = tag () ((\v'->v'>vth') ?? D v)
\end{code}
and the spike frequency calculated with the |frequncyDuring| function.
This relationship between the postsynaptic spike frequency and the
simulated synaptic input |rate| is plotted in Figure 3C for four
different values of |gmaxk|. Large A-type conductances supress spikes
resulting from endogenous synaptic activity, which was not
pharmacologically blocked in this experiment, and increases the
threshold at which imposed simulated synaptic activity causes postsynaptic
spiking.

\input{discuss}

\includepdf[pages=-]{Figure1.pdf}
\includepdf[pages=-]{Figure2.pdf}
\includepdf[pages=-]{FigureDyn.pdf}
%\includepdf[pages=-]{Figure4.pdf}
\end{document}
 


% To see that this model is not sufficient to explain the variability on
% different trials even within one approach speed, we calculate the
% variance of spike time histograms for individual trials simulated with
% a fixed set of parameters tau, t0, baseline and rate, with histogram
% variances averaged across draws of parameters from the posterior
% interred from the spike trains recorded with $L/V=20 ms^{-1}$.  The
% expected variance from fixed parameter models is much smaller than
% that observed. To compensate, we draw a new rate for every trial from
% a distribution $N(mu_{rate}, sd_{rate})$, such that the parameters $mu_{rate}$
% and $sd_{rate}$ replace rate (and are fixed across trials). This model can
% better account for the trial-to-trial variability of the spike time
% histogram (Figure X).

%Implementation of this with [Duration [Int]] and list of priors.

% Finally, any of these parameters may vary across individuals in a
% population of locusts. We add an additional layer to the hierarchical
% model for the parameters for the population distribution from which
% individual-level parameters are drawn. The population parameters form
% the highest-level hyperprior and can be considered the only (free)
% parameters (of interest) in the model.

\begin{comment}
--rest of intro


effects and equational languages.


The adoption of a viable formal notation marks a turning point in the maturation of a scientific field. By reducing or removing ambiguity, such a language allows ideas to be formulated and communicated efficiently, experimental observations to be shared and replicated, and scientific inference to be scrutinized.

Examples: Rhodes? Matrix notation in electromagnetism. Dirac notation.

The is currently no proposed notation for neuroscience in general or neurophysiology in particular, despite calls for this [refs].


What would the requirements for a modern notation be:


-machine and human readable: any notation proposed must be readable by the primary producers and consumers of scientific knowledge who remain human. But computers are almost ubiquitous in experimentation and so may be able to help carrying out a defined experiment, and can also help individual researchers filtering through the vast scientific literature.

-models, experiments, many different fields, and new ones
a successful notation should cover experimentation, simulation and analysis across a large intersection of multidiciplinary fields

-concise: the language should be concise; important ideas should be expressible in a short amount of space. A concise, expressive language can form an integral part of the presentation of new ideas and findings; a description in verbose language must be relegated to an appendix or an on-line database. In particular, this means that the language should support abstraction: it must be possible to name any relevant combination of terms such that the name can later be used with no change in meaning (ref: Tennent?).

-linguistic framework/ontology?
 

-referentially transparent
In order to facilitate reasoning about experiments (by computers or by humans), terms should be referentially transparent - that is, it should be possible to substitute any term or combination of terms with their meaning without affecting the value of the whole expression or the outcome of the experiment. Side effects.


The essence of the problem lies in the last desideratum; how can something which is purely mathematical describe the real world? A similar problem arises in a different guise of programming language theory: how can a language based entirely on composition and evaluation of mathematical functions interact with the world outside the program, i.e. perform input and output. There are many elegant solutions  to this problem; here we highlight one because it serves as the main inspiration for our language for physiology. Functional Reactive Programming (FRP; REF) recognizes the essential role time plays in certain computations, including animations, robotics and physical simulations. FRP introduces two fundamental concepts to represent information in a temporal concepts: Signals, which represent contiuously varying quantities, and events representing discrete occurances. Both of these concepts are examples of higher-order types, that is they are type constructors that can be instantiated with any basic type. Thus, for any type |alpha|, a signal of |alpha| can be thought of as a function from time to a value in |alpha|, which we write in type theory as

Signal a = Time → a

example of signal.

Likewise, an event of a's is isomorphic to a list of pairs of time values and a values:

Event a = [Time x a]

In FRP implementations, Signals and events or signal transformers are first-class: they can be assigned to variables and consumed and created by functions, just as signal producers and consumers, or indeed any other function, are first-class.

To what extent can these concepts describe scientific experimentation? That almost certainly depends on the role time plays in the particular field. Here, we argue that in physiology, time indeed fulfills a crucial role and that the notions of signals and events capture many aspects of physiological evidence. Firstly, many directly observable quantities can be expressed as signals and events, for instance:
animal or body part location : Signal (Location, Location)
joint angles : Signal Degrees
spikes: event ()
membrane potention : Signal Voltage
similarly for membrane conductance, quantal release, multichannel recording.

Latent (non-observable) variables same. Ion concentration, spike

Functional nature gives analysis aspects.

Plan for paper.



--old results organization
The bugpan language

Bugpan is a purely functional strongly typed programming language designed to serve as a description language for experiments and simulations in physiology. Bugpan has a syntax and type system that places it within the ML family of languages and is in particular similar to the Haskell programming language. It shares the main features of all strongly typed functional languages: Bugpan has first-class functions; that is, functions can be passed as values to other functions or can be created by other functions and stored in variables.    

Example of function.

The types of all values and functions must be consistent and known at compile-time, but need not be specified by the user: types can be inferred automatically if they are used consistently. In addition, there is no global state and the value of variables cannot be changed; the principal mode of computation is through the composition and evaluation of recursive functions. The type system of Bugpan is the well-studied Hindley-Milner system, in which programming and theorem proving are essential the same thing. Parametric polymorphism.

To these characteristics we add constructs for creating and observing signals and events (see introduction). In our syntax, the construct sopen e sclose creates a signal with the value of the expression e at every time point, and <: s :> yields the value of the signal s in the temporal context created by the surrounding sopen ... sclose braces. For instance, the function smap defined as

smap f s = sopen f <: s :> sclose

transforms a signal of |alpha| s into a signal of \beta by apply the function f of type |alpha| → \beta to the value at every timepoint. Likewise, the constructs .. for events..

(?) ::  (a → Bool) → Signal a → Event a

(??) ::  (a → Bool) → Signal a → Duration ()

In addition to the operators for forming and reading signals, two constructs enable signals to have a memory. The operator delay introduces an infinitesimal time-delay in a signal. The switch operator allows the occurence of events to influence the time-course of a signal...

Signals and events can be defined in this language by mutual recursion, for instance a signal can switch on an event defined 

-pattern match on derivatives

-sinks, sources.

-durations

-How to describe experiment, simulation.

-session storage
-queries and analyses
-statistical models
 

We illustrate the capability of Bugpan to describe experiments and analyses with one computational and one experimental example. These examples are simple but by no means trivial and both build on active research within the last decade.

Gain control

-why important

-how to define it.

-how to achieve it

 

Locust experiments

 

\end{comment}


\begin{comment}
\pagebreak

\section*{Introduction, take 2}

Gottfried Leibniz was possibly the first person to suggest that
mechanized reasoning removes ambiguity and thus allows ideas to be
formulated and communicated efficiently and for inferences to be
scrutinized. He imagined that a formalism for reasoning rests on two
pillars: a language for describing entities and a set of rules for
calculating with them. Since then, formal languages have had a
profound impact on mathematical methods and the theoretical and
statistical analysis of the natural sciences. As examples we point to
Leibniz's own infinitesimals, vector notation in electromagnetism and
the proliferation of logical formalisms in the 20th century stating
with Frege's first-order logic and the \emph{Principia Mathematica}.

Unfortunately, this clarity does not extend fully to scientific
inference. ... 

An endevour to extend mechanical reasoning into this domain would have
to give precise answers to some fundamental questions:

\begin{itemize}
  \item What kinds of things are there? From antiquity to the semantic
    web, we have recognized the importance of a catalogue of
    existence. In mathematics or any calculational framework, a more
    fundamental question (the answer to which the feasability of an
    ontology presupposes) is the catalogue of types, who stand in a
    one-to-many relation with the objects themselves and which
    determine which transformations are valid. In developing a
    calculus of evidence in a scientific field, we must therefore ask:
    what are the relevant \emph{types} whose values are observed?

  \item How do we construct, observe, and transform values of those
    types?

  \item What does the observation of these typed values tell us about
    statistical models? How are statistical models typed?
\end{itemize}

It may be unlikely that one set of answers can address all of the
natural sciences. A more achievable aim is to account for part of a
single scientific field. Thus one way of defining a field is to ask
what its constituent types are.

There are many ways of writing down a definition of an experiment or
an analysis, some of which are easier to reason about than
others. Making judgements about experiments requires us to calculate
--- to re-arrange, isolate and substitute terms. These manipulations
are are much easier in a language in which a term can be replaced by
terms with identical meaning without changing the meaning of the
context. For instance, no matter what $w$ refers to or where it
appears, it is always true that $w+w$ can be substituted by $2w$. This
property, which is called referential transparency, is shared by all
'mathematical' notations, but \emph{not} by conventional (imperative)
programming languages.

Here, we present a language and a calculus for physiological
evidence. The language is based on an abstract notion of signals and
events, and the calculus consists of a referentially transparent
computational framework for relating these types and a flexible
statistical framework for specifying and evaluating models. This
frameworks does not describe the physical components of an organism;
there are no concepts of organ systems, cells, proteins. Instead it
describes the mathematical objects that play a role in
evidence. Therefore, it allows experiments, analyses, simulations and
models to be defined equationally and concisely.

Our framework is derived from the work in incorporating input and
output in referentially transparent programming languages. The most
well-studied of these is the lambda calculus, a general framework for
computation based entirely on evaluating functions in the purely
mathematical sense, i.e. as maps between sets. The lambda calculus
allows both recursive functions and the use of functions as first
class entities: that is, they can be referenced by variables and
passed as arguments to other functions (which then become higher-order
functions). These properties together mean that the lambda calculus
combines verifiable correctness with a high level of abstraction,
leading to programs that are in practice more concise (ref). The
lambda calculus or variants thereof has been used as a general
framework for the foundation of mathematics (Martin-lof), classical
(wisdom \& sussman) and quantum mechanics (Karczmarczuk 2003),
microprocessor design (Grundy), chemistry (fontana \& buss 1994).

But the pure lambda-calculus has
no constructs for interacting with the real world and therefore cannot
be used to describe experiments.

Fortunately, it is possible to interact with the real world in a
manner that maintains referentially transparency. 

Here we highlight
one approach to effects that seems to be well suited for
physiology. Functional Reactive Programming (FRP; ref) recognizes the
essential role time plays in certain computer programs, including
animations, robotics and physical simulations. FRP augments the lambda
calculus with two fundamental concepts to place information in a
temporal concepts: Signals, which represent continuously varying
quantities, and events representing discrete occurances. These
concepts are given specific definitions in FRP and in our calculus of
physiological evidence. Whether directly observed or calculated, they
form the basis for inference in hierarchical regression models.

We use the calculus of physiological evidence to perform a non-trivial
experiment in \emph{in vivo} insect neurophysiology. The desert locust
Schistocerca gregaria, like many other winged insects, has a
specialised circuit in the optic lobe for detecting approaching
objects. This system projects to descending ganglia via the DCMD
neuron which is accessible to recording. The DCMD response is
sensitive to a variety of parameters including the stimulus contrast,
approach speed and size (gabbiani). However, few studies have have addressed
whether the locust looming detection system is an efficient in that it
can discriminate objects that are on collision course from those that
are on a non-intercepting trajectory. We show that this is the case
and that the precision of the looming detector is influenced by the
approach velocity. The definitions of these experiments and the data
analysis procedure is contained within the main sections of this paper
in a handful of equations.
\end{comment}
