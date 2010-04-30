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

\subsection*{Alternative titles}
\begin{description}
\item Functional reactivity as a calculus of physiological evidence
\item A functional calculus of physiological evidence
\item Structure and interpretation of physiological evidence
\end{description}
\section*{Introduction}

Mechanical reasoning removes ambiguity and thus allows ideas to be
formulated and communicated efficiently and for inferences to be
scrutinised. Consequently, formal languages and calculi have had a
profound impact on mathematics and the natural sciences. As
examples we point to Leibniz's infinitesimals, vector notation in
electromagnetism and Frege's first-order logic. These languages are
useful because they allow us to calculate --- to re-arrange, isolate
and substitute terms --- and by doing so, to prove general
theorems. These symbolic calculations are possible because terms can
be replaced by terms with identical meaning without changing the
meaning of the context. For instance, no matter what $w$ refers to or
where it appears, $w+w$ can always be substituted by $2w$. This
property, which is called referential transparency
\citep{Whitehead1927}, is shared by all ``mathematical'' notations but
not by conventional programming languages.

Although we can describe quantitative scientific models rigorously,
there are few formalisms to describe how evidence for or against these
models is obtained and evaluated. Today, most experiments are executed
by \emph{ad hoc} computer code, and communicated in natural
language. A more explicit approach to experimentation could facilitate
replication and meta-analysis, permit a better understanding of
apparent inconsistencies between studies and a clearer formulation of
what constitutes sound scientific practice.  Here, we propose a
calculus of physiological evidence that can describe an experiment
such that it can be unambiguously replicated and be inspected to
certify whether analysis procedures are applicable. This framework
does not describe the physical components of an animal; there are no
concepts of organ systems, cells or proteins. Instead it describes
observation and calculation of the mathematical objects that
constitute physiological evidence.

What is an experiment? Whether they are carried out by humans or by
automated equipment, experiments can be seen as \emph{programs} that
manipulate and observe the real world. This definition suggests that
experiment descriptions must resemble programming languages, and that
a referentially transparent calculus of experiments could come from
programming language theory. Functional Reactive Programming
\citep[FRP;][]{Elliott1997, Nilsson2002} is an elegant approach for
purely mathematical transformations to interact with the physical world;
here we show that the semantics of FRP provide a structure for
physiological evidence. We have implemented this calculus of evidence
in a concrete programming language and used it for neurophysiological
experiments, simulations and data analysis. Observations defined in
such a language have a simple interpretation in hierarchical Bayesian
models, a powerful framework for biological inference rarely used in
physiological data analysis \citep[but see][]{Gelman1996}.

We use a calculus of physiological evidence based on FRP to perform a
several non-trivial experiments in neurophysiology. The definitions of
these experiments and the data analysis procedure are contained within
the main sections of this paper in a handful of equations.

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
mathematical functions, the resulting style is known as Functional
Reactive Programming (Ref and inelegant). Here, we introduce a new
formulation of FRP designed to describe scientific experiments,
simulations and analyses. We begin by describing the types that are
central to FRP, and how physiological quantities can be captured in
these types. We then describe how values of of these types can be
observed, and how fuctions can be used to refine existing observations
or generate stimuli for experiments.

\subsubsection*{Types}

What kinds of mathematical objects can count as physiological
evidence? We answer this question within simple type theory (Ref),
which assigns to every object a \emph{type}. The types include base
types, such as integers, real numbers and text strings. In addition,
types can be arbitrarily combined in several ways, such that if
|alpha| and |beta| are types, the type |alpha times beta| is the pair
formed by an element of |alpha| and one of |beta|; |[alpha]| is a list
of |alpha|s; and |alpha -> beta| is a function from |alpha| to
|beta|. Types can be defined with to reference arbitrary types; for
instance, |withIntegers alpha = [(Z, alpha)]| denotes for any type
|alpha| the list of pairs of integers and |alpha|s. The ability to
build flexible type schemata in this manner and write generic function
over them (``parametric polymorphism'') is essential for the ability
to represent \emph{all} physiological quantities. 

FRP introduces two types of values to place information in a temporal
context: Signals, which represent continuously varying quantities, and
events representing distinct occurrences. These types are flexible, in
that they can be instantiated with any other type. Here, we add a
third type for information pertaining to temporal extents.

In physiology, observed time-varying quantities often represent scalar
quantities, such as membrane voltages or muscle force, but there are
also examples of non-scalar signals such as the two- or three
dimensional location of an animal or of a body part. Here, we
generalise this notion such that for \emph{any} type |alpha|, a signal
of |alpha| is defined as a function from time to a value in |alpha|,
written formally as:
\begin{code}
Signal alpha = Time -> alpha
\end{code}
Signals can thus take a new value for every different time point and
represent quantities that vary continuously, although these values may
be piecewise constant. For instance, the output of a differential
voltage amplifier might be captured in a |Signal Real|. Signals may be
stored in many different ways --- some signals as a sampled array,
others as an elementary function of time --- but to satisfy this
definition, there must exist an operation that, for any signal and for
any time point, can yield a value.

Unlike signals such as an extracellular potential or a membrane
conductance, some observed quantities such as action potentials are
discrete occurrences and are not associated with a new value at every
time point. To represent this qualitatively different class of
observations, we introduce events, defined as a set of pairs of
time points and a value in a type |alpha|, called the ``tag'': 
\begin{code}
Event alpha = {Time times alpha}
\end{code}
For example, an event could be constructed from a
scalar signal such that the time of the largest amplitude of a signal
was associated with the signal amplitude at that time-point. Some
events may not have a value of interest to associate with the
time point at which it occurred, in which case we can use the unit type
|()| which has only one element (that is, no information).

A third kind of information describes the properties of whole
``trials'', i.e. periods during which the system was exposed to a
controlled stimulus. Signals (which change from one time point to
another) and events (pertaining only to an instant) are unsuitable for
representing this information. What is needed is a type for
representing values associated with temporal extents. We define a
duration of type |alpha| as a set of triples, of which the first two
components denote  a start time and an end time. The last component is
again a value of any type |alpha|:
\begin{code}
Duration alpha = {Time times Time times alpha}
\end{code}
Durations are useful for information about whole trial or about an
entire animal, but could also be observations in their own right, such
as open times of individual ion channels, periods in which activity of
a system exceeds a set threshold (e.g bursts). We have used durations
to hold information about an entire experiment, for instance a session
identifier or the animal strain. Lastly, durations could be used for
information that spans multiple trials but not an entire experiment -
for instance, the presence of a drug.

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

Some of these values are directly observed from equipment such as
amplifiers or electronic detectors, but may need to be conditioned
before any conclusions can be drawn from them. Values in other types
can only be inferred from calculations on other observations. First we
show how to build programs that calculate with signals and events;
then we show how annotations allow these programs to stimulate
external system and observe their responses.

\subsubsection*{Calculating with signals and events}

In addition to specifying the types of physiological observations, FRP
provides the transformation facilities needed for data analysis. Thus
one can process events and signals, create new events from signals,
filter data and calculate statistics as necessary. These
transformations are formulated in terms of the lambda calculus
\citep{Church1941}, a formal language for referentially transparent
computation based on evaluating purely mathematical functions. The
lambda calculus allows the use of functions as first class entities:
that is, they can be referenced by variables and passed as arguments
to other functions (which then become higher-order functions). On the
other hand, the lambda calculus excludes variable or state mutation,
such that the meaning of variables are solely as references to
values. These properties together mean that the lambda calculus
combines verifiable correctness with a high level of abstraction,
leading to programs that are in practise more concise (ref). The
lambda calculus or variants thereof has been used as foundation for
mathematics \citep{Martin-Lof1985}, classical \citep{Sussman2001} and
quantum \citep{Karczmarczuk2003} mechanics, evolutionary biochemistry
\citep{Fontana1994} and programming languages \citep{McCarthy1960}. 

In the simple lambda calculus, calculations are performed by function
abstraction and application. |\x->e| denotes the function with
argument |x| and body |e|, and |f y| the application of
the value |y| to the function |f|. For instance, the function |add2 =
\x -> x+2| adds two to its argument; hence |add2 3 = (\x->x+2) 3 =
3+2| by substituting arguments in the function body.

Here, we present a concrete syntax for a formal language, based on the
lambda calculus and extended with first-class signals and events. Let
the construct |sopen e sclose| create a signal with the value of the
expression e at every time point, and |<: s :>| yield the current
value of the signal s in the temporal context created by the
surrounding |sopen ... sclose| braces. For instance, 
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

Further primitives are needed to form signals that depend on the
history of other signals. For any signal |s|, the expression |delay s|
denotes the signal that is delayed by a small amount of time (in
practise, one time step). 

The differential operator |D| differentiates a real-valued signal with
respect to time, such |D s| denote its first derivative |D D s| the
second derivative of the signal |s|. Likewise, the differential
operator can appear on the left side of a definition, in which case it
introduces a differential equation. This is illustrated in example 2
(see below)

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

Thus, signals describe continuous changes and events discrete
occurrences. Sometimes, one requires a continuous behaviour to change
abruptly into a different mode. FRP provides a clever way of doing
this: a special construct, |switch| changes the definition of signals
depending of the occurrence of specified events. 

...

This small number of special constructors, along with the lambda
calculus and the list semantics of events and durations, have allowed
us to construct a small ``standard library'' of analysis procedures
for physiology. Table S1 details the types and names of the functions
in this library.

\subsubsection*{Observing signals and events}

In the previous examples, signals, events and durations exist as
purely mathematical objects. In order to describe experiments, it must
also be possible to observe these values from real-world systems, and
to create controlled stimuli. For this purpose, we introduce
\emph{sources} and \emph{sinks} that bridge variables in purely
mathematical equations with the physical world.

Using the construct
\begin{code}
identifier <* source parameter
\end{code}
we bind the signal yielded by a \emph{paramet}rised \emph{source} to
the variable \emph{identifier}. This variable will hold the
whole signal observed during the course of the experiment. The signal
source binding construct defined a simple experiment:
\begin{code}
v <* ADC (0, 20000)
\end{code}
which describes the observation of the voltage signal on channel 0 on
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

Although every animal species can benefit from a mechanism for
detecting and moving away from obstacles and predators, the need for a
collision avoidance system is particularly acute in social animals. A
common component in such systems is a detector for looming objects. In
locusts, a single neuron in each brain hemisphere, the Lobular Giant
Movement Detector (LGMD), responds preferentially to looming stimuli
\citep{Rind1992}. The response of the LGMD is known to be invariant to
manipulations of the looming stimulus; for instance, a key property,
the time of the peak firing rate with respect to the retinal angle of
the looming stimulus, is insensitive to the colour, texture, size,
velocity and azimuth of the approaching object when averaged over
several approaches \citep{Gabbiani2001}. However, the reliability of
the looming detector is not well understood. For instance, the amount
and origin of variability in the response to repeated approaches of
identical objects is important for the animal behaviour (ref?), but
has not been quantified for this detector. In addition, a looming
detector must be able to discriminate objects that are on collision
course, but the efficiency of the LGMD in doing so has not been
quantified.

We constructed several experiments in the calculus of physiological
evidence to address these questions. Initially, we recorded the
response to visual stimuli representing identical objects approaching
with different velocities; the stimuli were later modified to
distinguish looming from nearly-looming objects. To generate these
stimuli, we augmented the calculus of physiological evidence with
primitive three-dimensional geometric shapes. Let the expression
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
visual stimuli as values in |Signal Shape|. The looming stimulus for
locusts consists of a cube of side length l approaching the viewer
with constant velocity v. The time-varying distance from the observer
to the cube in real-world coordinates is a real-valued signal:
\begin{code}
distance = sopen v * (5 - <: seconds :>) sclose 
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
effect, first we define an event for the collision of the object and
the screen

\begin{code}
hit = (\z->z<zscreen) ?? distance
\end{code}
and |switch| a new distance signal, |distance'|, based on the
occurrence of |hit|.
\begin{code}
distance' = switch {  _    ~>  distance, 
                      hit  ~>  \ (thit, zhit) -> sopen zhit sclose } 
\end{code}
This statement creates a new signal |distance'| which is identical to
the |distance| signal until an occurrence of a |hit| event, at which
point the distance is a constant, |zhit| (which equals $z_{screen}$.)
|loomingSquare'| is identical to |loomingSquare| except for the use of
|distance'|, and is connected to the |screen| sink.

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
voltage trajectory and firing rate response of a particular neuron?
This question can be addressed with experiements or simulations; here
we show how the calculus of physiological evidence can be used to
construct simple models of spiking neurons concisely. 

First, we consider the integrate-and-fire neuron. In this model, a
cell changes between two modes (integration and refraction)
\begin{code}
v_m =  switch {  _, refrac_end  ~>  let D v = cellOde v in v
                 spike          ~>  sopen v_rest sclose }
\end{code}
where
\begin{code} 
spike       = (\v->v>0.05) ?? v_m
refrac_end  = later 0.002 spike
\end{code}
In this example, the temporal evolution of |v_m| will alternate
between two modes: solving the differential equation given by
|cellOde| and a constant value of |v_rest|. The first mode will be
active initially (as indicated by |_|), and whenever the |refrac_end|
event occurs (the end of the refractory period). When the |spike|
event occurs, |v_m| will follow the constant signal |sopen v_rest
sclose| until another event occurs.



\section*{Discussion}

summary

No histograms, point estimators, data/meta-data distinction, work-flow
engines.

\subsection*{What is this thing?}

\begin{itemize}
\item practical tool
\item description is a necessary step in automation
\item framework for reasoning about experimentation
\item ontology, linguistic framework
\item calculus of physiological evidence.
\end{itemize}

frequency domain
no fancy analysis; not the point.
 

\subsection*{relation to semantic web ontologies}

completely orthogonal

related work

\subsection*{Representing scientific knowledge}
 How shall we represent scientific
knowledge? There has recently been much progress in representing
scientific information in a manner that facilitates machine inference
(semantic web refs). But if we are to represent scientific knowledge -
in the classical sense of true justified belief - we must include a
description of how we came about this information. In particular, this
must include a description of (i) the exact procedure carried out
during the experiment (ii) the context in which the experiment was
carried out (iii) the raw data collected during the experiment (iv)
the analyses and statistical tests carried out after the
experiment. Of these, (i) and (iv) are essentially executable programs
and (iii) are values in an appropriate datatype, here signals, events
or durations. (ii) is the most difficult to represent; here we make
extensive use of durations, which can be instantiated with any data
type; thus we can represent the sex of an experimental subject as a
duration (lasting the entirety of the experiment) of a (Male/Female)
type or, for a less typed approach, a string.

\subsection*{Towards verified scientific inference}

experiment description languages, and the reification of experimental
observations into values of concrete types, form basis for inference.

much easier with principle of likelihood: no need to represent the
intention of the experimenter. 

It will also permit the verification of statements about experiments,
for instance that particular variables were randomly controlled and
not observed; outcomes have not gained correlation due to the analysis
procedure; missing data is accounted for by the statistical model
\citep{Gelman2003}; correct propagation of errors \citep{Taylor1997}
and consistent units of measure \citep{Kennedy1997}; the absence of
``double dipping'' \citep{Kriegeskorte2009}. Statistics addresses some
of these issues in relating atomic observations to parameter
estimation and hypothesis testing, but not how those observations are
obtained.

relationship to automation.

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
observations, we have implemented Bugpan as an domain-specific
language embedded in the purely functional programming language
Haskell.  OpenGL and Comedi.

\subsection*{Locust experiments}

Recordings from the locust DCMD neurons were performed as previously
described (ref). Briefly, locusts were fixed in plasticine with the
abdomen upwards. The head was fixed with wax at an 90 degree angle and
the connectives were exposed. A pair of hook electrodes were placed
underneath the connectives and the electrodes and connectives enclosed
in petroleum jelly. The electrode signal was amplified 1000x and
bandpass filtered 50-5000 Hz, before analog-to-digital conversion at
18 bits and 20 kHz with a NI-6xxx board. The locust was placed in
front of a 22'' CRT monitor running with a vertical refresh rate of
160 Hz. All aspects of the visual stimulus and analog-to-digital
conversion were controlled by Bugpan programs running on a single
computer.

\subsection*{Statistical analysis}

metropolis-within-Gibbs sampler, proposal: Gaussian tuned to 15-40\% acceptance
rate. Uniform priors for mean and variance components. 

\bibliographystyle{apalike}
\bibliography{paper}

\includepdf[pages=-]{Figure1.pdf}
\includepdf[pages=-]{Figure2.pdf}
\includepdf[pages=-]{Figure3.pdf}
\includepdf[pages=-]{Figure4.pdf}
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
