## stogen: a tool to generate maps of stochastic processes

- with various space and time correlation structures,

- with various marginal probability distributions (at a given time and location).

### Space correlation

Spatial correlation can be obtained by any of the following algorithms:

- the recursive application of a diffusion operator on a white noise
(more efficient for short-range correlation scales), in module stodiff,

- the convolution of a white noise with a filtering kernel,
with a Quasi Monte Carlo approximation of the convolution integral
(more efficient for long-range correlation scales), in module stokernel.

Other algorithms can easily be introduced in the code as a new possible optipn
(in routine `sto_new_2d` in module stopar).

### Time correlation

Time correlation is obtained by autoregressive processes (in module stopar):

- a new map of spatially correlated noise is used to feed
the autoregressive equation at each time step;

- different time correlation spectra can be produced by using
autoregressive processes of various orders
(the higher the order, the smoother the process);

- for long correlation time scales (as compared to time step)
the process can be only updated every n time steps,
and then interpolated in time inbetween.
This can save a substantial computation time 
if the generation of the spatially correlated random noise is expensive.

In the current version, only a linear time interpolation is available,
but the code allows for storing an arbitrary number of time slices in memory
to allow for higher order interpolation schemes.

### Marginal distribution

By default, the marginal distribution of the stochastic fields
is a normal distribution (with sero mean and unit standard deviation).
Other distributions can be obtained by applying a transformation
to the reference normal processes (in module stomarginal), including:

- a bounded normal distribution: all values outside the bound are reset to the bound;

- a wrapped normal distribution, which is useful for cyclic variables (like angles);

- a lognormal distribution, which is useful where positive random numbers are requested;

- a bounded distribution in a given interval, which is here obtained by a transformation by the arctangent function.

New options can easily be included.

### Random number generator

The code includes three random number generators (shr3, kiss32, and kiss64)
and two algorithms to transform the integer pseudo-random sequence
into Gaussian numbers (the polar method and the ziggurat method),
all includes in the modules `storng_kiss` and `storng_ziggurat`.
The default option is kiss32 with ziggurat,
but the user is free to use any combination of them.
Other random number generators can also easily be included.
A tool to compare their computational performance is provide in module `storng_check`.

The result I obtain from this comparison is:

```
We test 3 random number generators:
   -kiss64 (period ~ 2^250, with four 64-bit seeds),
   -kiss32 (period ~ 2^123, with four 32-bit seeds),
   -shr3   (period ~ 2^64,  with one  32-bit seed)
together with 2 methods to generate normal numbers:
   -the polar method (Marsaglia, 1964),
   -the ziggurat method (Marsaglia, 2000).

Typical results obtained (in seconds), to generate 10^8 numbers
polar    (Marsaglia, 1964) + kiss64 (Marsaglia, 2009)   1.89253000000000
polar    (Marsaglia, 1964) + kiss32 (Marsaglia, 1992)   1.50936100000000
polar    (Marsaglia, 1964) + shr3 (simple & fast rng)   1.14710500000000
ziggurat (Marsaglia, 2000) + kiss64 (Marsaglia, 2009)   1.26209300000000
ziggurat (Marsaglia, 2000) + kiss32 (Marsaglia, 1992)  0.693772999999999
ziggurat (Marsaglia, 2000) + shr3 (simple & fast rng)  0.523565000000000
(the last 4 are obtained with real compuations in single precision)

In view of these results, the default choice used in the code is:
ziggurat (Marsaglia, 2000) + kiss32 (Marsaglia, 1992)
which combines a long enough period with good efficiency.
Options are included in the namelist to modify the default.
```

### For more details

- on the code, see directory src

- on the installation of the code, see directory tools

- on the use of the code, see directory examples
