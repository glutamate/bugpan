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
  physiological observations, experiments and analyses} 

\author{Thomas A. Nielsen, Henrik Nilsson and Tom Matheson}

\begin{document}
\begin{titlepage}

\vspace{50 mm}
\begin{center}{\LARGE {\bf A formal mathematical framework for
  physiological observations, experiments and analyses}}
\end{center}
\vspace{50 mm}

\begin{center}{\large Thomas A. Nielsen$^{*1}$, Henrik Nilsson$^2$ and Tom Matheson$^1$}
\end{center}
\vspace{50 mm}

\begin{flushleft}
1. Department of Biology, University of Leicester, University Road, Leicester LE1 7RH

2. School of Computer Science, University of Nottingham, Jubilee Campus, Nottingham NG8 1BB
\vspace{50 mm}

$^*$ To whom correspondence should be sent (tan5@@le.ac.uk)

\end{flushleft}

\end{titlepage}
\input{absintro}

\section*{The calculus of physiological evidence}

First, we introduce some terminology and basic concepts. We assume
that \emph{time} is global and is represented by a real number, as in
classical physics. An \emph{experiment} is an interaction between an
observer and a fixed number of organisms during a defined time
period. An experiment consist of one or more \emph{trials},
non-overlapping time periods during which the observer is running a
\emph{program} --- instructions for manipulating the environment and
for constructing mathematical objects, the \emph{observations}. The
\emph{analyses} are further programs to be run after or during the
experiment that construct other mathematical objects pertaining to
this experiment.

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
\emph{function} from time to a value in |alpha|, written formally as:
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
activity of a system exceeds a set threshold (e.g during action
potential bursts). We have used durations to hold information about an
entire experiment, for instance a session identifier or the animal
strain. In that case, the duration set contains a single element, with
the start and end of the experiement as start and end time,
respectively. Lastly, durations could be used for information that
spans multiple trials but not an entire experiment --- for instance, the
presence of a drug.

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
  Lab notebook & |Event String| \\
\hline
\end{tabular}
\vskip1ex 

Some of these quantities are directly observed from equipment such as
amplifiers or electronic detectors, but may need to be conditioned
before any conclusions can be drawn from them. Other quantities can
only be inferred from calculations on other observations. 

These ideas may seem obvious --- indeed, we see the conceptual
simplicity as their principal advantage.  But they are \emph{not}
available as descriptions of observed data in biomedical ontologies
and are absent from many programming languages.

We now proceed to show how to build programs that calculate with
signals and events; then we show how annotations allow these programs
to interact with external systems and measure their behaviour.

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
This property \citep[referential transparency;][]{Whitehead1927} 
% HN 2010-11-10: "enables" is a bit too strong. For example, it is certainly
% *possible* to reason formally about imperative code.
% enables
facilitates
algebraic manipulation and reasoning about the programs
\citep{Bird1996}. The lambda calculus allows functions to be
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

In the lambda calculus, calculations are performed by function
abstraction and application. |\x->e| denotes the function with
argument |x| and body |e| (i.e., an expression |e| in which the
variable |x| is in scope that yields the value of the function), and
|f e| the application of the function |f| to the expression |e| (i.e.,
what more conventionally would be written $f(e)$, except that |f| here
in general is a function-valued \emph{expression}). For instance, the
function |add2 = \x -> x+2| adds two to its argument; hence |add2 3 =
(\x->x+2) 3 = 3+2| by substituting arguments in the function body. In
addition, we define a number of constructs to improve the readability
of the language. However, they are not \emph{necessary} as they can be
defined in terms of function application and abstraction (and,
depending on the exact version of the calculus, some additional
primitive functions). The expression |if p then e1 else e2| equals
|e_1| if |p| evaluates to |True| and |e_2| if it evaluates to |False|. 
The construct |let x = e1 in e2| defines a variable |x| bound to the value of
the expression |e_1| that scopes over |e_2| as well as |e_1| (thus
allowing recursive definitions).

We now present the concrete syntax for a new calculus, that we call
the \emph{calculus of physiological evidence} (CoPE), defined by
extending the lambda calculus with notions of signals and events,
along with the necessary constructs to define and manipulate such
entities. This calculus borrows some concepts from earlier versions of
FRP, but it emphasises signals and events as mathematical objects in
themselves rather than as control structures for creating reactive
systems \citep{Elliott1997, Nilsson2002}. Specifically, the new calculus
encompass a relaxed notion of \emph{causality}. The syntax and
implementation strategy is therefore very different from FRP.

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
signal |s|. Likewise, the differential operator can appear on the \emph{left}
side of a definition, in which case it introduces a differential
equation by pattern matching 
% HN 2010-11-10: This placeholder has been here a while. I don't think
% a formal reference is necessary: "pattern matching" should be sufficiently
% self-evident for the use we make of it here.
% (REF)
on the derivative of a signal (see example 2 below).

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
occurrence of an event whenever the value of a signal exceeds a set
level (and then not again before the value of the signal has decreased
below that level and then reached it again).  Here, we generalise the
threshold detector slightly by taking a predicate (i.e., a function of
type |alpha->Bool|) on the instantaneous values of the signal and
generate an event whenever the predicate \emph{becomes} true using the |??|
operator. For instance,
\begin{code}
(\x->x>5) ?? s
\end{code}
denotes the event that occurs whenever the value of the signal |s|
satisfies the predicate |\x->x>5|, i.e. is greater than 5, after having been
smaller than 5 for at least a small period of time, in practice the
timestep. The expression |(\x->x>5) ?? s| thus defines a threshold
detector restricted to threshold crossings with a positive slope.

This small number of special constructors, along with the lambda
calculus and the list semantics of events and durations, have allowed
us to construct a small ``standard library'' of analysis procedures
for physiology. Table 2 details the types and names of the functions
in this library.

% \subsubsection*{Observing signals and events}
\subsubsection*{Interacting with the physical world}

In the previous examples, signals, events and durations exist as purely
mathematical objects. However, in order to describe experiments, it must also
be possible to observe particular values from real-world systems, and to
create controlled stimuli to perturb these systems. For this purpose, we
introduce \emph{sources} and \emph{sinks} that act as a bridge between the
purely mathematical equations and the physical world.

A source is essentially an input port through which the value of some external
variable can be observed during the course of an experiment. Depending on the
nature of the variable, whether it can vary or will remain constant throughout
an experiment, the observation yields a signal or single value, respectively.
A typical example of a source is an analog-to-digital converter measuring some
experimental variable. Observing this over the duration of an experiment
yields a signal.

In more detail, the sources are parametrised, thus denoting a \emph{family}
of sources with the parameter(s) identifying a specific instance. The
construct
\begin{code}
identifier <* source parameter
\end{code}
binds the value or signal resulting by observing the the parametrised
\emph{source} during the course of an experiment to the variable
\emph{identifier}. Note that, in the case of a signal, the variable is bound
to the \emph{whole} signal. For a concrete example, the following code
defines a simple experiment:
\begin{code}
v <* ADC 0
\end{code}
This describes the observation of the voltage signal on channel 0 of an
analog-to-digital converter, binding it to the variable |v|.

What happens if the same source is observed more than once in a description of
an experiment? Often, for example if the source refers to a single, physical
input port such as a channel of an analog-to-digital converter, the result
will necessarily be the same as the same entity is being observed within a
single run of the experiment. Such sources are called \emph{idempotent}.
Idempotency ensures that separate experiments referring to a common external
variable can be composed easily with a predictable outcome. However, there are
other kinds of sources, notably random sources as discussed below, where
idempotency is \emph{not} desirable. Each occurrence of a non-idempotent
source is thus a separate, independent source, even if the name of the
source and the parameters happen to be the same.

In addition to making appropriate observations, an experiment may also involve
a perturbation of the experimental preparation. In the context of a physiology
experiment, the purpose could be to control the amount of current injected
into a cell. In one of the examples described below, non-numeric signals are
used to generate visual stimuli on a computer screen. 

This requires the opposite of a source, namely an output port connected to a
physical device capable of effectuating the desired perturbation. Such a port
is called a \emph{sink}. The value at the output at any point in time during
an experiment is defined by connecting the corresponding sink to a signal.
This is done through the the following construct, mirroring the source
construct discussed above:
\begin{code}
signal *> sink parameter
\end{code}{}

As a concrete example, suppose we wish to output a sinusoidal stimulus. We
first construct a time-varying signal describing the desired shape of the
stimulus. In this case, we start with a clock signal that counts the number of
seconds since the experiment started, which can be read from a clock source:
\begin{code}
seconds <* clock ()
\end{code}
(There is only one clock, meaning that the parameter is of the type unit |()|
that only has one element, also written |()|.) The sine wave can now be
defined as:
\begin{code}
sineWave = smap sin seconds
\end{code}
To send the |sineWave| signal to channel channel 0 of a digital-to-analog
converter, we then write
\begin{code}
sineWave *> DAC 0
\end{code}{}

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
is important there are no accidental correlations. Sharing of a single random
signal, when needed, can be described easily anyway: just bind that signal to a
variable as discussed above and use the variable to refer to the signal
instead of repeating the reference to the random source. In this more general
view, sources and sinks bridge referentially transparent and non-transparent
computations.

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

We have constructed several experiments in CoPE to record the response
of LGMD in locusts to visual stimuli that simulate objects approaching
with different velocities. To generate these stimuli, we augmented
CoPE with primitive three-dimensional geometric shapes. Let the
expression
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
minutes, with different values of $\frac{l}{||v||}$. Figure 1 shows
$\frac{l}{||v||}$ as values with type |Duration Real|, together with the
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
$\frac{l}{||v||}$. The average of |hspike| for three different values of
$\frac{l}{||v||}$ are shown in figure 2A, and 2B and 2C show the total
number of spikes (|length spike|) and largest value of |hspike|, for
each approach, plotted against the value of $\frac{l}{||v||}$. These plots
show that while the peak firing rate is a decreasing
function of $\frac{l}{||v||}$, the total number of spikes in the approach
is an increasing function. In addition, the time of the peak rate is
later with smaller values of $\frac{l}{||v||}$
\citep{Hatsopoulos1995}. 

This experiment indicates that the calculus of physiological evidence
can adequately and concisely describe the visual stimulus, spike
recording and relevant analysis for activation of the locust looming
detection circuit. To demonstrate the versatility of this framework,
we next show that it can be used to implement dynamic clamp in an
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

A dynamic clamp experiment \citep{Robinson1993, Sharp1993} requires
electrical access to the intracellular compartment, such that the cell
membrane voltage can be recorded and current injected into the
cell. As opposed to a standard current-clamp experiment, where the
injected current waveform is known in advance, in the dynamic clamp
setup the injected current command is calculated near-instantaneously
from the membrance voltage. Unlike the standard current clamp
configuration, the dynamic clamp permits the imposition of additional
simulated ionic conductances on a real neuron. For instance, it is
possible to record the responce of a cell to an added synaptic
conductance or an additional Hodgkin-Huxley style voltage-sensitive
membrane conductance. Here, we combine these possibilities to
investigate the effect of an A-type potassium conductance
\citep{Connor1971} on the response of a zebrafish spinal motor neuron
to synaptic excitation.

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
conditions explicitly assigned to the variable $x_0$. Here the
differential equation for the activation variable $a$ is
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

%\begin{comment}

\begin{tabular}{l  l  p{8cm}}
\hline
  Function & Type & Description\\ 
\hline
  |peak| & |Signal alpha -> Event alpha| & Peak value of each signal segment\\

  |freqDuring| & 
\parbox{4cm}{\begin{singlespace}
|Duration alpha -> Event beta| \\|-> Duration Real|
\end{singlespace}} 
& Count events in each occurence\\

  |around| &
\parbox{4cm}{\begin{singlespace}
|Event alpha -> Signal beta| \\|-> Signal beta|
\end{singlespace}} 
& Align signal around event occurrences\\

  |inout| &
\parbox{4cm}{\begin{singlespace}
|Event alpha -> Event beta| \\|-> Duration alpha times beta|
\end{singlespace}} 
& Create a durations from start and stop events\\

  |convolveSE| &
\parbox{4cm}{\begin{singlespace}
|Signal Real -> Event Real| \\|-> Signal Real|
\end{singlespace}} 
& Convolve a signal with an event\\

  |during| &
\parbox{4cm}{\begin{singlespace}
|Duration beta -> f alpha | \\ |-> f alpha|
\end{singlespace}} 
& Event/Duration/Signals that lie within occurences in a duration\\

  |burst| &
\parbox{4cm}{\begin{singlespace}
|Real -> Event alpha | \\|-> Duration ()|
\end{singlespace}} 
& Durations when successive inter-event occurrence intervals fall below a minimum\\

  |adjustDur| &
\parbox{4cm}{\begin{singlespace}
|Time times Time| \\ | -> Time times Time | \\ |-> Duration alpha | \\|-> Duration alpha|
\end{singlespace}} 
& Durations when successive inter-event occurrence intervals fall below a minimum\\




\hline


\end{tabular}
\vskip1ex 

%\end{comment}

Table 2. Some common operations for generic manipulation of signals, events and durations.

\end{document}
 
