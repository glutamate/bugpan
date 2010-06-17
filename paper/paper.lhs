\documentclass[11pt]{article}
%include polycode.fmt
%include lhs2tex-braincurry-preamble
\usepackage{amsmath, amsthm, amssymb}
\usepackage{setspace} 
\usepackage{verbatim} 
\usepackage[final]{pdfpages}
\usepackage{natbib}
\usepackage{graphicx}
%\usepackage{epsfig}
\onehalfspacing
\title{A calculus of physiological evidence}
\author{Thomas A. Nielsen et al}
\begin{document}

\maketitle

\section*{Introduction}

Mechanical reasoning removes ambiguity and thus allows ideas to be
formulated and communicated efficiently, and inferences to be
scrutinised. Consequently, formal languages and calculi have had a
profound impact on mathematics and the natural sciences. As
examples we point to Leibniz's infinitesimals, vector notation in
electromagnetism and Frege's first-order logic. Such languages are
useful because they allow us to calculate --- to re-arrange, isolate
and substitute terms --- and by doing so, to prove general
theorems. These symbolic manipulations are possible because terms can
be replaced by terms with identical meaning without changing the
meaning of the context. For instance, no matter what $w$ refers to or
where it appears, $w+w$ can always be substituted by $2w$. This
property, which is called referential transparency
\citep{Whitehead1927}, is shared by all ``mathematical'' notations but
not by conventional programming languages.

Although we can describe quantitative scientific models rigorously,
there are few formalisms to fully describe how evidence for or against
these models is obtained and evaluated. A mathematical approach
approach to experimentation itself could facilitate replication and
meta-analysis, permit a better understanding of apparent
inconsistencies between studies and a clearer formulation of what
constitutes sound scientific practice.  Here, we propose a calculus of
physiological evidence that can describe an experiment on a biological
organism such that it can be unambiguously replicated and be inspected
to certify whether analysis procedures are applicable. This framework
does not describe the physical components of biological organism; it
has no concept of networks, cells or proteins. Instead it describes the
observation and calculation of the mathematical objects that
constitute physiological evidence.

What is an experiment? Whether they are carried out by humans or by
automated equipment, many experiments can be seen as \emph{programs}
that manipulate and observe the real world. This definition suggests
that experiment descriptions must resemble programming languages, and
that a referentially transparent calculus of experiments could come
from programming language theory. Functional Reactive Programming
\citep[FRP;][]{Elliott1997, Nilsson2002} is an elegant approach for
purely mathematical transformations to interact with the physical
world. Here we show that the semantics of FRP provide a structure for
physiological evidence. We have implemented this calculus of evidence
in a concrete programming language and used it for non-trivial
neurophysiological experiments and data analysis. These
protocol are defined unambiguously in a handful of equations; we show
two such examples.

\section*{The calculus of physiological evidence}

Examining the temporal evolution of observed quantities, such as
intrinsic rhythms or responses to external stimuli, is ubiquitous in
physiology. Often these observations are mediated by actions that are
said to happen at a certain time point - such as action potentials or
secretion events - but are themselves manifestations of continuous
changes in ion channel conductances or fusion pore dilations on a
different timescale. Time must play a multifaceted role in
physiological evidence.

Time also plays an essential role in some computer programs, such as
such as animations, video games, robotics and physical
simulations. Although these can be implemented in a conventional
programming language, then can also be composed in a referentially
transparent manner by combining elementary transformations of the
whole input, including responses from the user, into the entire output
from the program. When these transformations can be defined by purely
mathematical functions, the resulting computational paradigm is known
as Functional Reactive Programming. FRP introduces
two types of values to place information in a temporal context:
Signals, which represent continuously varying quantities, and events
representing distinct occurrences. These types are flexible, in that
they can carry information of any other type. We show that there is
significant overlap between these definitions of signals and events,
and physiological evidence. Thus, definitions of experiments can be
described concisely in a language based on the composition and
calculuation of signals and events, such as one based on FRP.  We
begin by describing the types that are central to FRP, and how
physiological quantities can be captured in these types. We then
describe how values of of these types can be observed, and how
fuctions can be used to refine existing observations or generate
stimuli for experiments.

\subsubsection*{What is physiological evidence?}

What kinds of mathematical objects can count as physiological
evidence? We answer this question within simple type theory
\citep{Pierce2002}, which assigns to every object a \emph{type}. These
types include base types, such as integers |Integer|, real numbers
|Real|, text strings |String| and boolean type |Bool| with the two
values |True| and |False|. In addition, types can be arbitrarily
combined in several ways, such that if |alpha| and |beta| are types,
the type |alpha times beta| is the pair formed by an element of
|alpha| and one of |beta|; |[alpha]| is a list of |alpha|s; and |alpha
-> beta| is the type of functions that calculate a value in the type
|beta| from a value in |alpha|. Here, we use the convention that greek
letters stand for type variables, which can be substituted by any
concrete type, such as a base type or a compound type. Types can be
defined with references to arbitrary types; for instance,
|withIntegers alpha = [Integer times alpha]| denotes for any type |alpha|
the list of pairs of integers and |alpha|s. This hole in the type
definition can then be filled in to form a concrete type, for instance
|withIntergers String|. The ability to build flexible type schemata in
this manner and define generic function over them \citep[``parametric
  polymorphism'';][]{Pierce2002} is essential for representing
\emph{all} physiological quantities.

What, then, are the types in which physiological evidence can be
values? We distinguish three type schemas that differ in the
relationship between measurements and their temporal context, but all
derive their flexibility from parametric polymorphism. \emph{Signals}
capture the notion of quantities that change in time. In physiology,
observed time-varying quantities often represent scalar quantities,
such as membrane voltages or muscle force, but there are also examples
of non-scalar signals such as the two- or three dimensional location
of an animal or of a body part. Here, we generalise this notion such
that for \emph{any} type |alpha|, a signal of |alpha| is defined as a
function from time to a value in |alpha|, written formally as:
\begin{code}
Signal alpha = Time -> alpha
\end{code}
Signals can thus take a new value for every different time point and
represent quantities that vary continuously, although these values may
be piecewise constant. For instance, the output of a differential
voltage amplifier might be captured in a |Signal Real|.

But not every physiological observation denotes continuous
change. Some measurements are derived from an instant in time - such
as the peak amplitude of an electrical potential - and others pertain
to an extended period of time. These qualitatively different classes
of observations are represented by \emph{events} and \emph{durations},
respectively. 

To model discrete occurences, FRP introduced events as a set of pairs
of time points and a value in a type |alpha|, called the ``tag'':
\begin{code}
Event alpha = {Time times alpha}
\end{code}
For example, an event could be constructed from a scalar signal such
that the time of the largest amplitude of a signal was associated with
the signal amplitude at that time-point. Events that do not have a
value of interest to associate with the time point at which it
occurred, can be tagged with the unit type |()| which has only one
element (that is, no information). Therefore, events can represent
both measurements where the principal information is \emph{when}
something happend, and measurements concerning \emph{what} happened
where the time is mostly contextual.

A third kind of information describes the properties of whole periods
during which the system was exposed to a controlled stimulus. We
define a duration of type |alpha| as a set of triples, of which the
first two components denote a start time and an end time. The last
component is again a value of any type |alpha|:
\begin{code}
Duration alpha = {Time times Time times alpha}
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
drawn from neurophysiology here: \vskip1ex
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
external system and observe their responses.

\subsubsection*{Calculating with signals and events}

From the direct observations, one often needs to process events and
signals, create new events from signals, filter data and calculate
statistics. Here, we formulate these transformations in terms of the
lambda calculus \citep{Church1941}, a formal language for
referentially transparent computation based on evaluating purely
mathematical functions. The lambda calculus allows the use of
functions as first class entities: that is, they can be referenced by
variables and passed as arguments to other functions (which then
become higher-order functions). On the other hand, the lambda calculus
excludes variable or state mutation. These properties together mean
that the lambda calculus combines verifiable correctness with a high
level of abstraction, leading to programs that are in practise more
concise (ref). The lambda calculus or variants thereof has been used
as a foundation for mathematics \citep{Martin-Lof1985}, classical
\citep{Sussman2001} and quantum \citep{Karczmarczuk2003} mechanics,
evolutionary biochemistry \citep{Fontana1994} and programming
languages \citep{McCarthy1960}.

In the simple lambda calculus, calculations are performed by function
abstraction and application. |\x->e| denotes the function with
argument |x| and body |e|, and |f y| the application of the value |y|
to the function |f|. For instance, the function |add2 = \x -> x+2|
adds two to its argument; hence |add2 3 = (\x->x+2) 3 = 3+2| by
substituting arguments in the function body. In addition we define a
number of constructs to improve the readability of the language,
although strictly they can be defined in terms of function application
and abstraction. The expression |if p then c else a| evaluates to |c|
if |p| is |True| and |a| if |a| is |False|. Similarly, |let y = e in
w| defines a variable |y| with the value of the expression |e| that
can be used as a value in the expression |w|. 

Here, we present a concrete syntax for a formal language, based on the
lambda calculus and extended with first-class signals and events. Let
the construct |sopen e sclose| create a signal with the value of the
expression |e| at every time point, and |<: s :>| yield the current
value of the signal |s| in the temporal context created by the
surrounding |sopen  sclose| braces. For instance, 
\begin{code}
sopen 1 sclose
\end{code}
denotes the signal that always has the value 1; and the function smap
defined as
\begin{code}
smap = \f -> \s -> sopen f <: s :> sclose
\end{code}
transforms, for any two types |alpha| and |beta|, a signal of |alpha|
into a signal of |beta| by applying the function |f| of type |alpha
-> beta| to the value of the signal at every time point.

The differential operator |D| differentiates a real-valued signal with
respect to time, such |D s| denote its first derivative |D D s| the
second derivative of the signal |s|. Likewise, the differential
operator can appear on the left side of a definition, in which case it
introduces a differential equation by pattern matching (ref) on the
derivative of a signal (see example 2 below).

Further primitives are needed to form signals that depend on the
history of other signals. For any signal |s|, the expression |delay s|
denotes the signal that is delayed by a small amount of time (in
practise, one time step). Other FRP implementations have other
primitives, in particular a |switch| statement that changes the
definition of a signal depending on the occurrence of specific
events. We have not needed such a construct in the experiments
described here.

The simplest approach to constructing events, and that taken here, is
to detect events from existing signals. For instance, a threshold
detector generates an occurrence in an event every time point the value
of a signal exceeds a set level (and then not again before the value
of the signal has decreased below that level and then reached it
again.)  Here, we generalise the threshold detector slightly by taking
a predicate (i.e., a function of type |alpha->Bool|) on the
instantaneous values of the signal and generate an event whenever the
predicate becomes true using the |??| operator. For instance,
\begin{code}
(\x->x>5) ?? s
\end{code}
denotes the event that occurs whenever x crosses the threshold level
|5|. 

In addition to event detection, we suggest that events (and durations)
should be manipulated as if they were lists in a functional
programming language. Thus, a large number of transformations can be
defined with simple recursive equations including filters, folds and
scans (refs).

This small number of special constructors, along with the lambda
calculus and the list semantics of events and durations, have allowed
us to construct a small ``standard library'' of analysis procedures
for physiology. Table S1 details the types and names of the functions
in this library.

\subsubsection*{Observing signals and events}

In the previous examples, signals, events and durations exist as
purely mathematical objects. In order to describe experiments, it must
also be possible to observe these values from real-world systems, and
to create controlled stimuli to perturn these systems. For this
purpose, we introduce \emph{sources} and \emph{sinks} that bridge
variables in purely mathematical equations with the physical world.

Using the construct
\begin{code}
identifier <* source parameter
\end{code}
we bind the signal yielded by a parametrised \emph{source} to
the variable \emph{identifier}. This variable will hold the
whole signal observed during the course of the experiment. The signal
source binding construct defined a simple experiment:
\begin{code}
v <* ADC (0, 20000)
\end{code}
which describes the observation of the voltage signal on channel 0 of
an analog-to-digital converter at 20kHz sampling rate.

In addition to making appropriate observations, an experiment may also
involve a perturbation of the experimental preparation. To create a
stimulus for an external system, we first construct time-varying signals as
described above, for instance a sine wave. To build such a signal, we
start with a clock signal that counts the number of seconds since the
experiment started, which can be read from a clock source
\begin{code}
seconds <* clock ()
\end{code}
where |()| denotes that the clock source does not require any
parameters. The sine wave can then be defined with

\begin{code}
sineWave = smap sin seconds
\end{code}

Connecting this signal to the real world requires the opposite of a
signal source, namely a signal sink. To send the sineWave signal to a
digital-to-analog converter, we write

\begin{code}
sineWave *> DAC (0, 20000)
\end{code}

In the context of a physiology experiment, these declarations can for
instance control the amount of current injected in a cell. Below,
non-numeric signals and signal sinks are used to control visual
stimuli on a projection screen. In addition, sources and sinks
are not restricted to signals. Random number generators are also
difficult to describe as pure functions and expressions involving
random numbers can break referentially transparency. We have thus
implemented sources corresponding to common parametric probability
distributions.

\section*{Example 1}

Most animal species can benefit from a mechanism for detecting and
moving away from obstacles and predators. In addition, navigation in
social animals are further constrained by the need avoiding collisions
with conspecifics. A common component in such systems is a detector
for looming objects. In locusts, a single neuron in each brain
hemisphere, the Lobular Giant Movement Detector (LGMD), responds
preferentially to looming stimuli \citep{Rind1992}. The response of
the LGMD is known to be invariant to manipulations of many apsects of
the looming stimulus. For instance, a key property, the time of the
peak firing rate with respect to the retinal angle of the looming
stimulus, is insensitive to the colour, texture, size, velocity and
azimuth of the approaching object when averaged over several
approaches \citep{Gabbiani2001}. 

Here, we have constructed several experiments in the calculus of
physiological evidence to recorded the neural response in locusts to
visual stimuli representing identical objects approaching with
different velocities. To generate these stimuli, we augmented the
calculus of physiological evidence with primitive three-dimensional
geometric shapes. Let the expression
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
reported here. Since signals are entirely polymorphic containers, they
can carry not just numeric values but also shapes, and we represent
visual stimuli as values in |Signal Shape|. The looming stimulus
consists of a cube of side length l approaching the viewer with
constant velocity v. The time-varying distance from the observer to
the cube in real-world coordinates is a real-valued signal:
\begin{code}
distance = sopen v * (<: seconds :> - 5) sclose 
\end{code}

The |distance| signal is the basis of shape-valued signal for the
approaching square, |loomingSquare|:

\begin{code}
loomingSquare = 
     sopen colour  (0,0,0) 
                   (translate  (0,0, <: distance :> ) 
                               (cube l)) sclose
\end{code}

|loomingSquare| differs from conventional protocols for stimulating
the DCMD in that the object passes through the observer after
collision. In order not to evoke a large OFF response from the LGMD
\citep{O'shea1976} immediately after collision, the object is frozen
in space as it reaches the plane of the surface onto which the
animation is projected \citep{Hatsopoulos1995}. To achieve this
effect, we define a new signal that has a lower bound of the distance
from the eye to the screen |zscreen|
\begin{code}
distance' = sopen max (zscreen, <: distance :> ) sclose
\end{code}
|loomingSquare'| is identical to |loomingSquare| except for the use of
|distance'|.

Finally, |loomingSquare'| is connected to a screen signal sink that
represents an abstract visual display unit capable of projecting
three-dimensional shapes onto a two-dimensional surface.

\begin{code}
loomingSquare' *> screen ()
\end{code}

The response to the looming stimulus in the LGMD can be recorded from
the locust connectives. Although the LGMD is not thought to make a
long-range projection, it reliably activate the descending
contralateral movement detector (DCMD) with a strong synaptic
connection, such that spikes in the DCMD follow LGMD spikes one to one
\citep{O'Shea1974}. Extracellular hook electrodes wrapped around the
connectives record activity in the DCMD as the strongest unit. These
electrodes were amplified, filtered (see methods) and converted to a
digital signal:

\begin{code}
voltage <* ADC (0, 20000)
\end{code}

|loomingSquare'| and |voltage| define a single approach and the
recording of the response thereto. This approach was repeated every 4
minutes, with different values of $\frac{l}{v}$. Figure 1 shows
$\frac{l}{v}$ as values with type |Duration Real|, together with the
|distance'| and |voltage| signals for the first five trials on a
common time scale.

The simplest method for detecting spikes from a raw voltage trace is
to search for threshold crossings, which works well in practise for
calculating DCMD activity from recordings of the locust connectives
(ref). If the threshold voltage for spike detection is |vth|, the
event |spike| can be calculated with
\begin{code}
spike = (\v->v>vth) ?? voltage
\end{code}
Here, we throw away the tag
\begin{code}
spike = tag () ((\v->v>vth) ?? voltage)
\end{code}
so that |spike| has type |Event ()|. This event, with a jittered tag
for display purposes (ref), is displayed on the common time scale in
Figure 1. The top row displays the spike count histogram |hspike =
sopen length (filter (between <: delay seconds:> <: seconds:> . fst)
spikes) sclose| for each trial.

We examined how the DCMD spike response varied with changes in
$\frac{l}{v}$. The average of |hist| for three different values of
$\frac{l}{v}$ are shown in figure 2B, and 2C and 2D show the total
number of spikes (|length spike|) and largest tag of |hspike|, for
each approach, plotted against the value of $\frac{l}{v}$. These plots
show that while the peak rate of the spike histogram is an decreasing
function of $\frac{l}{v}$, the total number of spikes in the approach
is n increasing function. In addition, the time of the peak rate is
later with smaller values of $\frac{l}{v}$
\citep{Hatsopoulos1995}. 

\section*{Example 2}

An important property studied in computational neuroscience is the
input-output relationships of neurons. Given a stimulus, e.g. injected
current waveform or pattern of synaptic input, what is the membrance
voltage trajectory and firing rate response of a particular neuron? In
particular, the cell properties such as the dendritic morphology or
ionic conductances can profoundly influence this relationship. Such
influences can be examined with experiments or simulations; here we
show how the calculus of physiological evidence can be used to
formulate and execute (simulated, at least for now!) dynamic-clamp
experiments on synaptic integration.

A dynamic clamp experiment requires electrical access to the
intracellular compartment, such that the cell membrane voltage can be
recorded and current injected into the cell. As opposed to a standard
current-clamp experiment, where the injected current waveform is known
in advance, in the dynamic clamp setup the injected current is
calculated near-instantaneously from the membrance voltage and a known
conductance waveform by ohm's law, or by simulated
Hodgkin-Huxley-style voltage-sensitive conductances.

Dynamic clamp-experiments thus follow the same template --- the voltage
is read and the (output) current calculated from the imposed
conductance:
\begin{code}
v <* ADC (0,20000)

i = sopen (<: v :> - E)* <: g :> sclose

i *> DAC (0,20000)
\end{code}
The experiment is thus characterised by the signal $g$.

In the simplest case, $g$ is independent of $v$; for instance, when
considering linear synaptic conductances (Ref angus and simon). Here,
we consider the addition of a simulated fast excitatory synaptic
conductance to a real neuron. Simple neural models of often simulate
synaptic excitation using an alpha function, which provides a
relatively good fit to many synaptic processes.
\begin{code}
alpha amp tau = sopen amp*tau **2 * <: seconds :> *exp (- <: seconds :> *tau) sclose
\end{code}
Fig 3A shows the membrane voltage recorded from a XXX neuron in a
dynamic clamp experiment where |g = sopen alpha Y Z <:seconds:>
sclose|.
 
A more realistic simulation of the input to a cell is the convolution
of this signal with a presynaptic spike train. The spike train itself
is first read from a source representing a random probability
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
The new signal gsyn was used in a new dynamic clamp experiment, and
the recorded membrane voltage (Fig 3B) showed considerable
subthreshold fluctuations and some spikes. By changing |rate| and
measuring the frequency of postsynaptic spikes (|frequencyDuring
running ((\v->v>0) ?? v)|), we can plot the
input-output relationship of the neuron (Fig 3C).

Both the subthreshold properties of a cell and its spiking rate can be
regulated by active ionic conductances. One way to examine this
regulation of synaptic integration is to impose an additional active
conductance on cells with dynamic clamp. In the Hodgkin-Huxley
formalism for ion channels, the conductance depends on two state
variables, for which the forward and backward rate constants depend on
the membrane voltage. Here we show the equations for the activation
gate of the A-current, following (Traub ref). The equations for
inactivation are analogous.

We write the forward and backward rates as functions of the membrane voltage
\begin{code}
alphaa v = (20*(13.1-v))/((exp ((13.1 -v)/10)) -1)
betaa v =  (17.5*(v-40.1))/((exp ((v-40.1)/10))-1)
\end{code}

The time-varying state of the activation gate is given by a
differential equation. We use the notation |D x = | $x_0$ | fby sopen f
  (x,<:seconds:>) sclose | to denote the ordinary differential equation
that is conventionally written $\frac{dx}{dt} = f(x,t) $ with starting
conditions $x=x_0$.
\begin{code}
D a = 0 fby sopen  alphaa (<: vm :> *1000+60) * (1- <:a:> ) -
                   betaa (<: vm :> *1000+60) * <: a :> sclose
\end{code}
with the inactivation state signal |b| defined similarly.

The current signal from this channel is calculated from Ohm's law:
\begin{code}
ika = {: gmaxk * <:a:> * <:b:>*(<:vm:> - E) :}
\end{code}
which is added to the signal |i| defined above. Figure 3A and 3B shows
the voltage response to a unitary synaptic conductance and a trains of
synaptic inputs, respecitively, with |gmaxk = 10 nS|.

By varying the value of |rate|, we can examine the input-output
relationship of the model neuron. To plot this relationship
quantitatively, we need a function to calculate the frequency of
events in durations. |frequencyDuring| does this by exploiting the
list semantics of both durations and events, using 
functions |map|, |filter| and |length| familiar from list processing:

\begin{code}
frequencyDuring ds es = map f ds
   where f ((t1,t2),_) =
           let count = length (filter (between t1 t2 . fst) es)  
           in ((t1,t2), count / t2-t1)
\end{code}

|frequencyDuring| retains the temporal context of the firing rate
by returning a new duration, with the rate of the event as the tag. 

Figure 3C shows a scatterplot of the postsynaptic against the
presynaptic spike rate for two different values of |gmaxk|, the
maximal conductance of the A-current. Here, an increase in the
a-current appears to raise the threshold for activation of the
postsynaptic cell without changing the gain.

\section*{Discussion}

We propose that three types, signals, events and durations, are
sufficient to represent physiological evidence when they can be
parametrised by any other type. We show how observations and
calculations of these types can be described in a mathematical
framework based on the lambda calculus. Two examples from
neurophysiology illustrate this approach: the \emph{in vivo} spike
train response to a visual looming stimulus in locusts; and a
simulation of synaptic integration in a simple model neuron. 

We present an entirely new approach to performing and communicating
science. Here, both stimuli and observations are defined concisely and
unambiguously by mathematical \emph{equations}. This makes it possible
to not only repeat, manipulate and reson about experiments in a formal
framework. We can quibble about whether these definitions are simpler
or clearer than a definition written in plain English or are easier to
produce than clicking buttons in a graphical user interface. But they
are certainly less ambiguous and more powerful than either of those
alternatives.

To our knowledge this is the first explicit use of type theory to
classify evidence in a experimental scientific field. We find that
parametric polymorphism is critical in modelling a wide repertoire of
evidence. We also surprisingly found that the there is broad overlap
between the types that hold stimuli, observations and calculated
values. This may be not be the case in other fields. For instance, in
anatomy, the direct observations may be mediated by images and
inferences mainly made about graph structures. (We don't know how to
represent the stimuli.) A wider question is which type theories can
support evidence from many different fields. For physiology, we have
found that simple type theory \citep{Pierce2002} is sufficient. Richer
type theories including impredicativity and dependent types
\citep{Pierce2002} may help capture aspects of evidence in other fields.

What exactly is the thing that is described in this paper? First of
all, it is a very practical tool: a collection of computer programs
for executing experiments and analyses and organising
data. Controlling every aspect of the scientific process with programs
that share data formats is potentially efficient and eliminates many
sources of human error. Secondly, beyond any particular
implementation, we have described a framework for reasoning about and
manipulating scientific experiments. Obtaining such an experiment
definition is a necessary step in automating the experiment.  It also
makes the conditions of the recording very explicit, and can serve as
a unambiguous communication medium. Finally, the types we have
presented form a linguistic framework and an ontology for
physiology. This ontology can form the basis for the interchange of
physiological data without imposing unnecessary constraints on what
can be shared.

The principal claim of this paper, that signals, events and durations
can represent all physiological evidence, is presented without a
concrete proof. In one regard, the claim is trivially true: if a
piece of evidence can be represented in a type |alpha|, then a value
of type |Duration alpha| can be constructed by this function:
\begin{code}
dur x = [((-l, l),x)]
\end{code}
where l is a sufficiently large number. A stronger claim is this: all
references to time are signals, events or durations. Thus, our theory
will be refuted by a single piece of evidence in a physiological
experiment that has temporal information or context but is neither a
signal, event or a duration, or is an event or a duration that has
temporal information in the tag. We can already think of one such
example: power-spectra can be thought of as values analogous to
signals, but indexed by frequency rather than time. The most
appropriate manner of extending our framework to include
frequency-indexed and even spatial information must remain a topic for
further research. On the other hand, the lack of linear algebra,
higher order statistics, information theoretic analysis et cetera, et
cetera in our examples is \emph{not} to be taken as a limitation of
our theory; representing these analyses in a mathematical framework is
a solved problem and is not the topic of this paper.

\subsection*{Statistics}

We have used the word ``evidence'' to mean direct observations and
calculated values from experiments. In another sense of the word,
evidence carries information that is relevant for a particular
statistical model. Here we discuss some ideas as to how values with
signal, event or duration types can be combined with statistical
models. First, a conservative approach is to take measurements on
signals and events - for instance, the amplitude of signal
deflections, event frequencies etc - and store these in durations. The
language of event detectors and list semantics for durations and
events may be sufficiently rich to describe these measurements, as we
have demonstrated for the limited examples in this paper. It is then
straightforward to use the tags of durations representing measurements
in classical null-hypothesis significance tests such as the General
Linear Model. This approach differs little from the way data is
analysed today in physiology, but a lightweight and executable
formalism for describing these analyses would make it easier to handle
large amounts of data and reduce the risk of human error.

A more intriguing possibility is to build statistical models for the
directly observed data, and to use durations to describe a
hierarchical organisation of conditional dependencies amongst
parameters in such a model. Although probabilistic models for direct
observations can be built in physics \citep{Daniell1991}, the flexible
construction of such models for biological organisms may depend on the
development of new programming languages.

\subsection*{Relation to existing technologies}

It should be no surprise that some notion of signals and events can
play a central role in physiology. Existing software packages that are
popular in the acquisition and analysis of physiological data have
signal-like (Igor Pro, Spike 2), and event-like (Spike 2) data
structures at the core of their operations. Although these package
have some flexibility in the information that can be carried by
signals and events, they do not exploit full parametric polymorphism;
the range of observations and stimuli that can be described with this
more limited definition is smaller. For instance, the signal of shapes
that describe a visual stimulus in Example 1, or the two- or three
dimensional location of a moving animal \emph{cannot} be represented
by a signal in these systems. Thus functions written over generic
signals cannot be used in cases where such data are represented in a
different data structure.

Our account of physiological evidence solves or immediately proposes
solutions to many problems in neuroinformatics. One such problem is
the difficulty of sharing primary data from neuroscientific
experiments (REF). A repository of such data must answer the question
``what kinds of things should be shared?'' We show here that fully
polymorphic signals, events and durations can represent data across
many aspects of neurophysiology, and can be the foundation for a very
general database. Previous work on scientific knowledge representation
has also suggested a possible role for ``meta-data'' to represent the
context of an experiment (REFs). Here, we make no distinction between
data and meta-data. All relevant information which can be represented
by a value in some type can exist as signals, events and
durations. There are important reasons to believe that there cannot
exist a strict distinction between data and meta-data; is, for
instance, the temperature at which an experiment is conducted, data or
meta-data? That surely depends on the intention of the experimenter
and whether it is manipulated, but also of the person who is analysing
or meta-analysing the data. By not assigning data to different bins,
but merely saying, ``there exists information about this experiment''
we are not imposing any limits on the nature of this information. That
does not imply that individual pieces of information have no
context. Because every value has a temporal context, the entire
context of that value can be retrieved by gathering other values with
a similar or enclosing temporal context.

There has been some work on formalising ``workflows'' and
creating ``workflow engines'' for perform data analysis. In the
contrasting approach here, all information about the experiment is
defined by mathematical equations and exist as values of well-defined
types. We are not excluding the possibility that an elaborate user
interface can help formulate these equations, or construct new values
in ad-hoc circumstances (we have used a simple interface to enter the
thresholds for spike detections in Example 1). But such interfaces may
be inessential to scientific inference, even in the large scale. This
is especially likely to be the case when we can write generic
functions over types with very flexible definitions.

Lastly, there has been much work on semantic web ontologies that
describe results from biological experiments. While our approach is at
variance with some of this work, in other respects it is 
complementary. There is much relevant information about the
experiments performed here that we have not represented; for instance,
the exact position of the locust with respect to the screen, the anatomical
location of the recording electrodes etc. Such information should be
represented as a duration, but the question is what type the tag of
the duration should have. On solution is to allow tags to take values
that correspond to subjects or relations in a semantic web ontology.

\subsection*{Towards verified scientific inference}

Whether the natural sciences are socially constructed remain an open
question in the philosophy of science. If we seriously entertain the
notion that science is instead based on logic \citep{Jaynes2003}, it
must be possible in principle to mechanically verify scientific
inference, just as mathematical proofs can be verified by a proof
checker (ref). It is of course not possible to verify particular
hypotheses about the physical world, or a biological organism. What
can be verified are statements about experiments, for instance that
particular variables were randomly controlled and not observed;
outcomes have not gained correlation due to the analysis procedure;
missing data is accounted for by the statistical model
\citep{Gelman2003}; correct propagation of errors \citep{Taylor1997}
and consistent units of measure \citep{Kennedy1997}; the absence of
``double dipping'' \citep{Kriegeskorte2009}; and ultimately, whether
the gathered data support the conclusion. Statistics addresses some of
these issues in relating atomic observations to parameter estimation
and hypothesis testing, but not how those observations are
obtained. Experiment description languages, and the reification of
experimental observations into values of concrete types (which may not
always be the type of real numbers), can play an important role in
such inference. But the statistical framework within which such
inferences take place impact the amount of information that must be
analysed; for instance, if we accept the likelihood principle
\citep{Jaynes2003}, we can safely ignore the intention of the
experimenter. 

There has been substantial progress in \emph{automation} in the
experimental sciences, which has substatially accelerated the
acquisition of knowledge. In contrast, there has been almost no work
in verification, a seperate but overlapping application of calculating
machines to science. Nevertheless, if such verification is possible it
may lead to a much more radical change in the way scientific research
is conducted.

\section*{Methods}

\subsection*{Language implementation} 

Although Bugpan is intended to present a single language for
experimentation, simulation and analysis, we have used two different
implementation strategies for reasons of rapid development and
execution efficiency. For purposes of experimentation and simulation,
we have implemented a prototype compiler that can execute some Bugpan
programs that contain signals and events that are defined by mutual
recursion, as is necessary for many of the simulations and experiments
in this paper. For post-acquisition/simulation analysis, where one
often merely wishes to calculate a new value from existing
observations, we have implemented Bugpan as a domain-specific
language embedded in the purely functional programming language
Haskell.  OpenGL and Comedi.

\subsection*{Locust experiments}

Recordings from the locust DCMD neurons were performed as previously
described (ref). Briefly, locusts were fixed in plasticine with the
ventral side upwards. The head was fixed with wax at an 90 degree angle and
the connectives were exposed. A pair of hook electrodes were placed
underneath the connectives and the electrodes and connectives enclosed
in petroleum jelly. The electrode signal was amplified 1000x and
bandpass filtered 50-5000 Hz, before analog-to-digital conversion at
18 bits and 20 kHz with a NI-6xxx board. The locust was placed in
front of a 22'' CRT monitor running with a vertical refresh rate of
160 Hz. All aspects of the visual stimulus and analog-to-digital
conversion were controlled by Bugpan programs running on a single
computer.

\bibliographystyle{apalike}
\bibliography{paper}

\includepdf[pages=-]{Figure1.pdf}
\includepdf[pages=-]{Figure2.pdf}
\includepdf[pages=-]{FigureIF.pdf}
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
