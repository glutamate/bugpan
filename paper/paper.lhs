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
\item Structure and Interpretation of Neural Physiology
\item Functional reactivity as a calculus of physiological evidence
\item A functional calculus of physiological evidence
\item Structure and interpretation of physiological evidence
\end{description}
\section*{Introduction}

Mechanical reasoning removes ambiguity and thus allows ideas to be
formulated and communicated efficiently and for inferences to be
scrutinised. Consequently, formal languages and calculi have had a
profound impact both on mathematics and the natural sciences. As
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

Although we can describe quantitative scientific models, there are few
formalisms to describe how evidence for or against these models is
obtained and evaluated. A more explicit approach to experimentation
could facilitate replication and meta-analysis, permit a better
understanding of apparent inconsistencies between studies and a
clearer formulation of what constitutes sound scientific practice.
Here, we propose a calculus of physiological evidence that can
describe an experiment such that it can be unambiuously replicated
and be inspected to certify whether analysis procedures are
applicable. This framework does not describe the physical components
of an animal; there are no concepts of organ systems, cells or
proteins. Instead it describes observation and calculation of the
mathematical objects that play a role in physiological evidence.

What is an experiment? Whether they are carried out by humans or by
automated equipment, experiments can be seen as \emph{programs} that
manipulate and observe the real world. This definition suggests that
experiment definitions must resemble programming languages, and that a
referentially transparent calculus of experiments could come from
programming language theory. An elegant and declarative (\emph{what}, not
\emph{how}) approch to effectful programing is realised in Functional
Reactive Programming \citep[FRP;][]{Elliott1997, Nilsson2002}; we
combine observations defined in such a language with hierarchical
bayesian models, a powerful framework for biological inference rarely
used in physiology.

We use a calculus of physiological evidence based on FRP to perform a
non-trivial experiment in \emph{in vivo} insect neurophysiology. The
desert locust \emph{Schistocerca gregaria}, like many other winged
insects, has a specialised circuit in the optic lobe for detecting
approaching objects \citep{Rowell1976, Hatsopoulos1995}. This system
projects to descending ganglia via the descending contralateral
movement detector (DCMD) neuron which is accessible to recording. The
DCMD response is sensitive to a variety of parameters including the
stimulus contrast, approach speed and size
\citep{Gabbiani2001}. However, few studies have addressed whether the
locust looming detection system is efficient in discriminating objects
that are on collision course from those that are on a non-intercepting
trajectory. We show that this is the case and that the precision of
the looming detector is influenced by the approach velocity. The
definitions of these experiments and the data analysis procedure are
contained within the main sections of this paper in a handful of
equations.

\section*{The calculus of physiological evidence}

Time plays a crucial role in physiology. Plots of the temporal
evolution of observed quantities, such as instrinsic rhythms or responses
to external stimuli, are ubiquitous in publications. Often these
observations are mediated by actions that are said to happen at a
certain time point - such as action potentials or secretion events -
but are themselves composed of continuous changes in ion channel
conductances or fusion pore dilations. Time must have a priviliged
role in accounting for physiological evidence, although it is used to
index many different things.

Functional Reactive Programming is a family of programming languages
built for computer programs where time also plays an essential role,
such as animations, robotics and physical simulations. FRP introduces
two abstract concepts to place information in a temporal context:
Signals, which represent continuously varying quantities, and events
representing distinct occurances. Programs writen in a such a language
.. takes whole time as an input (Henrik had something that sounds good
here.)... Here, we show that a language based on FRP can capture many
aspects of experimentation and analysis in physiology. The types
introduced by FRP - signals, events - present a flexible scheme for
representing concrete physiological evidence. The ``functional''
aspect of FRP define analysis procedures such as signal transformers
and event detectors. Finally, we show how this framework ties in with
inference in hierarchical probabilistic models.

\subsection*{Types}

The observation, analysis and simulation of signals are essential to
physiology, and also forms the basis of the bugpan formalism. In
physiology, observed signals are usually time-varying scalar
quantities such as membrane voltages or muscle force, but there are
also examples of non-scalar signals such as the two- or three
dimensional location of an animal or of a body part. Here, we
generalise this notion such that for \emph{any} type |alpha|, a signal
of |alpha| is defined as a function from time to a value in |alpha|:
|Signal alpha = Time -> alpha|. For instance, the output of a
differential voltage amplifier might be captured in a |Signal
Real|. In addition to the types introduced here, we assume that the
universe of types are inhabited by base types such as the integers,
the real numbers, and means of combining simple types such as pairs,
lists and functions. Indeed, signals and events are synonyms for such
combinations.

Unlike signals such as an extracellular potential or a membrane
conductance, some observed quantities such as action potentials are
discrete occurrences and are not associated with a new value at every
timepoint. To represent this qualitatively different class of
observations, we introduce events as the type |Event alpha = [Time
  times alpha]|. Like signals, events are a higher-order type that can
be instantiated for any type a, such that a event of a is a list of
occurrences at specific timepoints, each with an associated value. For
example, an event could be constructed from a scalar signal such that
the time of the largest amplitude of a signal was associated with the
signal amplitude at that time-point. Some events may not have a value
of interest to associate with the timepoint at which it occured, in
which case we can use the unit type |()| which has only one element
(that is, no information).

To construct events from signals, we take a predicate on
the instantaneous values of the signal and generate an event whenever
the predicate becomes true using the |??| operator (|?? ::
(alpha->Bool) -> Signal alpha -> Event alpha|).

.... How shall we represent such a summary
statistic? It holds one value that pertains to a period of time, in
this case from the beginning of the trials until the collision
time. Signals (which change from one timepoint to another) and events
(pertaining only to an instant) are unsuitable for representing this
information. What is needed is a type for representing values
associated with temporal extents. We define a duration of type |alpha|
as a list of pairs, of which the second component is a value of
|alpha| and the first component is itself a pair of a start time and
an end time:
\begin{code}
Duration alpha = [Time times Time times alpha]
\end{code}

...  These simple, yet flexible, types suffice to represent many
physiological quantities: \vskip1ex
\begin{tabular}{l  l}
\hline
  Quantity & Type \\ 
\hline
  Voltage across the cell mebrane & |Signal Real| \\
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

some directly observed, some inferred. How to infer them?

\subsection*{Calculating with signals and events}

Physiological inference necessarily involves the calculation of
infered quantities from direct observations. In addition to specifying
the types of these quantities, FRP provides the transformation
facilities needed for data analysis. Thus one can process events and
signals, create new events from signals, filter data and calculate
statistics as necessary. A combination of pure functions that have
no side effects and a construct to form signals based on an expression
for their instantaneous value allows us to build signal transformers.


The lambda calculus \citep{Church1941} is the best understood
framework for referentially transparent computation, based entirely on
evaluating functions in the purely mathematical sense, i.e. as maps
between sets. The lambda calculus allows the use of functions as first
class entities: that is, they can be referenced by variables and
passed as arguments to other functions (which then become higher-order
functions). These properties together mean that the lambda calculus
combines verifiable correctness with a high level of abstraction,
leading to programs that are in practice more concise (ref). The
lambda calculus or variants thereof has been used as foundation for
mathematics \citep{Martin-Lof1985}, classical \citep{Sussman2001} and
quantum mechanics \citep{Karczmarczuk2003}, evolutionary biochemistry
\citep{Fontana1994} and functional programming languages
(ref:mccarthy?). In the \emph{typed} lambda calculus, values inhabit
base types such as integers, real numbers, or compound types such as
vectors, lists, functions or pairs.

Here, we present a ....  Let the construct |sopen e sclose| create a
signal with the value of the expression e at every time point, and |<:
s :>| yield the current value of the signal s in the temporal context
created by the surrounding |sopen ... sclose| braces. As in the lambda
calculus, functions are first class and can be passed as arguments to
other functions; |\x->e| denotes the function with argument |x| and
body |e|. For instance, the function smap defined as
\begin{code}
smap = \f -> \s -> sopen f <: s :> sclose
\end{code}
transforms a signal of |alpha| s into a signal of |beta| by apply the
function |f| of type |alpha → beta| to the value at every
timepoint. 

delay, D, 

event constructor 
emap

durations, events: lists

more examples

\subsection*{Observing signals and events}

In the previous examples, signals, events and durations exist as
purely mathematical objects.

we introduce the notion of signal sources,
which represent the unprocessed/raw observations available in an
experiment. 

Using the construct
\begin{code}
identifier <* signal source
\end{code}
we bind the signal yielded by the signal source to the variable
denoted by \emph{identifier}. This variable will hold the whole signal
observed during the course of the experiment. The signal source
binding construct allows us to formulate a simple experiment:
\begin{code}
voltage <* ADC (0, 20000)
\end{code}
which describes the (unperturbed) observation of the voltage signal on
channel 0 on an analog-to-digital converter at 20kHz sampling rate.

In addition to making appropriate observations, an experiment may also
involve a pertubation of the experimental preparation. To make a
dynamic pertubation, we first need to be able to construct
time-varying signals, for instance a sine wave. To build such a
signal, we start with a clock signal that counts the number of seconds
since the experiment started, which can be read from a clock source
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
digital-to-analog converter, we might write

\begin{code}
sineWave *> DAC (0, 20000)
\end{code}

Non-real signals: screen

sources not restricted to signals. Events, random number generation.

\subsection*{Probabilistic inference}

But we also find that new analysis methods become feasible;
for instance, having functions as first class entities makes it much
simpler to directly represent probability distributions. We show how
hierarchical Bayesian modelling \citep{Gelman2003} can incorporate the
reactive entities here introduced, such that model parameter
estimation and hypothesis testing takes into account all the
information observed.

\section*{Results}

Although every mobile species can benefit from a mechanism for
detecting and moving away from obstacles and predators, the need for a
collision avoidance system is particularly acute in social animals. A
common component in such systems is a detector for looming objects. In
locusts, a single neuron in each brain hemisphere, the Lobular Giant
Movement Detector, responds preferentially to looming stimuli
(REFS). The response of is known to be invariant to manipulations of
the looming stimulus; for instance, a key property, the time of the
peak firing rate with respect to the retinal angle of the looming
stimulus, is insensitive to the colour, texture, size, velocity and
azimuth of the approaching object when averaged over several
approaches (gabbinai). However, the reliability of the looming
detector is not well understood. For instance, the amount and origin
of variability in the response to repeated approaches of identical
objects is important for the animal behaviour (ref?). In addition, a
looming detector must be able to discriminate objects that are on
collision course, but the efficiency of the LGMD in doing so has not
been quantified. 

We constructed several experiments in the calculus of physiological
evidence to address these questions. Initially, we recorded the
response to identical objects approaching with different velcities;
the visual stimuli were later modified to distinguish looming from
nearly-looming objects. These experiments all involved visual stimuli
consisting of animations of primitive three-dimensional geometric
shapes. Since signals are entirely polymorphic containers, they can
carry not just numeric values but also shapes if we have a suitable
data representation for them. (in fact, FRP was invented to compose
reactive animations). Say that the function
\begin{code}
cube l
\end{code}
constructs a value of the shape type representing a cube located at
the origin with the side length |l|,
\begin{code}
translate (x,y,z) shp 
\end{code}
denotes the shape that results from translating the shape shp by the
vector |(x,y,z)| and
\begin{code}
colour (r,g,b) shp
\end{code}
the shape identical to shp except with the colour intensity red r,
green g and blue b. Additional constructors can be introduced for more
complex stimuli, but these are sufficient for the experiments reported
here. We aim to show a cube of side length l approaching the viewer
with constant velocity v to produce the characteristic looming
response in the locust. We will construct the signal of
shapes representing this animation and then connect it to a suitable
signal sink. First, we calculate the time-varying distance from the
observer to the cube in real-world coordinates:
\begin{code}
distance = sopen v * <: seconds :> - 5 sclose 
\end{code}

Secondly, we calculate the shape signal

\begin{code}
loomingSquare = 
     sopen colour  (0,0,0) 
                   (translate  (0,0, <: distance :> ) 
                               (cube l)) sclose
\end{code}

|loomingSquare| is a faithful animation of an object approaching the
origin, but it differs from conventional protocols for stimulating the
DCMD in that the object passes through the observer after
collision. For reasons currently beyond the understanding of the first
author of this (somewhat incomplete) manuscript, it is common to
freeze the object as it reaches the plane of the surface onto which
the animation is projected (Ref). To achieve this effect, first we
define an event for the collision of the object and the screen

\begin{code}
hit = (\z->z<zscreen) ?? distance
\end{code}
We would like to create a new signal that changes its behaviour when
the hit event occurs. Although in this case such an effect can be
achieved by a conditional expression, the switch expression from FRP
is a more general and powerful solution to this problem. Switch
creates a new signal by selecting from a list of signals depending on
which of several events occurred last.

\begin{code}
distance' = switch {hit ~> \ (thit, zhit) -> sopen zhit sclose } distance 
\end{code}
This statement creates a new signal |distance'| which is identical to
the |distance| signal until an occurrence of a |hit| event, at which
point a new signal can be calculated as a function of the time and the
tag of the event occurrence. In this case, a signal with the constant
value $z_{hit}$ (which equals $z_{screen}$) is the new form of distance'.

Finally, loomingSquare is connected to a screen signal sink that
represents an abstract visual display unit capable of projecting
three-dimensional shapes onto a two-dimensional surface.

\begin{code}
loomingSquare *> screen ()
\end{code}
Here, the screen sink is not parametrized, but for a more flexible
description language it could be configured with the background colour
and the viewing angle.

explain runnign this multiple times.

refer to figure 1 here.

In addition to producing the visual animation depicting a looming
stimulus, we must also record its neural response. The LGMD reliably
activates the descending contralateral movement detector (DCMD) that
can be recorded extracellularly from the connectives. ...
\begin{code}
voltage <* ADC (0, 20000)
\end{code}
The simplest method for detecting spikes from a raw
voltage trace is to search for threshold crossings, which works well
in practice for calculating DCMD activity from recordings of the
locust connectives. If the threshold voltage for spike detection is
|vth|, we can define the event |spike| by
\begin{code}
spike = (\v->v>vth) ?? voltage
\end{code}

fig 1: spikes and ecvoltage

spike time histograms

In order to create a duration tagged with the number of spikes during
each trial, for every element in the |running| duration, we filter the
spike events to include only those that occur within that temporal
extent, take the length of the resulting list, and pair it with the
start and end point of the duration.
\begin{code}
nspikes = countDuring running spikes

countDuring ds evs = map count ds
   where count = \  ((t1,t2),_) -> 
                    ((t1,t2),length (filter  (between t1 t2 . fst) 
                                             evs))
\end{code}
which requires a single non-standard utility function

\begin{code}
between = \t1 -> \t2 -> \t -> t>t1 && t<t2 
\end{code}
and the standard list-processing functions |map|, |filter| and |length|.


The spike count histogram, ie the average of |sopen length (filter
(between <: delay seconds:> <: seconds:> . fst) spikes) sclose| for a
locust DCMD recording, is shown in fig X along with a plot of nspikes
against the L/V value. These plots show that while the peak rate of
the spike histogram is an decreasing function of L/V, the total numbe
of spikes in the approach is n increasing function. In addition, the
time of the peak rate is later with smaller values of L/V (ref). These
preliminary observations suggest a complex relationship between the
firing rate and the stimulus parameters. (And we have no idea what
aspect of the spike train are most important in influencing behaviour)

Rather than drawing relationships between specific point estimators
(peak rate, number of spikes, time of peak firing), we built a
hierarchical probabilistic model of the full spike train on every
trial and use bayesian inference to estimate its parameters.  explain
what this is. (In this way, we do not lose any information in the
process of data analysis.)

First, we assume that on a particular trial i with a given approach
speed, the DCMD firing rate can be described by an inhomogenous
Poisson process with rate $r(t)$. That is, the probability of seeing a
spike between time $t$ and $t+dt$ is $r(t)dt$ for small dt. Although we do
not know what the form of $r(t)$ is a priori, in practice we can look
for a parametric function that can be fit to the spike time histogram
under a variety of conditions in a series of preliminary
experiments. One such function is the reversed and time-shifted alpha
function with a fixed baseline:
\begin{equation*}
r_{\alpha}(t) = 
\begin{cases} rate \cdot \frac{-(t-t_0)}{\tau}\cdot e^{1+\frac{-(t-t_0)}{\tau}}+baseline & \text{if $t \le t_0$,}
\\
baseline &\text{if $t > t_0$.}
\end{cases}
%r_{\alpha}(t) = %    | t < t0 = ((-(t-t0)/tau)*exp(1+(t-t0)/tau))*(rate-baseline)+baseline
%    | otherwise = baseline
\end{equation*}

To see that this model is not sufficient to explain the variability on
different trials even within one approach speed, we calculate the
variance of spike time histograms for individual trials simulated with
a fixed set of parameters tau, t0, baseline and rate, with histogram
variances averaged across draws of parameters from the posterior
interred from the spike trains recorded with $L/V=20 ms^{-1}$.  The
expected variance from fixed parameter models is much smaller than
that observed. To compensate, we draw a new rate for every trial from
a distribution $N(mu_{rate}, sd_{rate})$, such that the parameters $mu_{rate}$
and $sd_{rate}$ replace rate (and are fixed across trials). This model can
better account for the trial-to-trial variability of the spike time
histogram (Figure X).

Implementation of this with [Duration [Int]] and list of priors.

The model should also attempt to explain the influence of the approach
speed on the spike train. We estimate parameters separately to sets of
spike trains recorded with approach $L/V$ of $10$, $20$ and $40
ms^{-1}$. From these fits, it appears that the x is related... y is
inversely related etc. We thus replace $mu_{rate}$ by a constant of
proportionality  $alpha_{rate}$ etc.

Finally, any of these parameters may vary across individuals in a
population of locusts. We add an additional layer to the hierarchical
model for the parameters for the population distribution from which
individual-level parameters are drawn. The population parameters form
the highest-level hyperprior and can be considered the only (free)
parameters (of interest) in the model.

The final model, estimated parameters. 

which correlate? 

show effect on time of peak, amplitude, mean weighted rise.

is the peak before or after collision?

variability wihtin and between animals

Motivate displaced loom experiment. 

show responses, nspikes etc

Do responses still follow function?
Which parameters does the displacement affect? 
Interaction displacement and speed.

say something about Accuracy/speed tradeoff.

bias

\section*{Discussion}

No histograms, point estimators, data/meta-data distinction, workflow
engines.

\subsection*{What is bugpan}

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
and consistent units of measure \citep{Kennedy1997}; the absense of
``double dipping'' \citep{Kriegeskorte2009}. Statistics adresses some
of these issues in relating atomic observations to parameter
estimation and hypothesis testing, but not how those observations are
obtained.

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
observations, we have implemtented Bugpan as an domain-specific
language embedded in the purely functional programming language
Haskell.  OpenGL and Comedi.

\subsection*{Locust experiments}

Recordings from the locust DCMD neurons were performed as previously
described (ref). Briefly, locusts were fixed in plasticine with the
abdomin upwards. The head was fixed with wax at an 90 degree angle and
the connectives were exposed. A pair of hook electrodes were placed
underneath the connectives and the electrodes and connectives enclosed
in petrolium jelly. The electrode signal was amplified 1000x and
bandpass filtered 50-5000 hz, before analog-to-digital conversion at
18 bits and 20 khz with a NI-6xxx board. The locust was placed in
front of a 22'' CRT monitor running with a vertical refresh rate of
160 hz. All aspects of the visual stimulus and analog-to-digital
conversion were controlled by Bugpan programs running on a single
computer.

\subsection*{Statistical analysis}

metropolis-within-gibbs sampler, proposal: gaussian tuned to 15-40\% acceptance
rate. Uniform priors for mean and variance components. 

\bibliographystyle{apalike}
\bibliography{paper}

\includepdf[pages=-]{Figure1.pdf}
\includepdf[pages=-]{Figure2.pdf}
\includepdf[pages=-]{Figure3.pdf}
\end{document}
 



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
