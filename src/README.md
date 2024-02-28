## Descritpion of the modules

There are two kinds of modules:

- the external code, which mimics the time iteration of a geohysical model (like NEMO),
and illustrates how to make requests to the stochastic modules;

- the stochastic modules, which receive requests from the external code
and produce the stochastic processes with the requested properties.

### External code

- **_stogen_** :
    Main program with empty model illustrating the use of the stochastic modules, including:
    - initialization of model parameters (grid, number of time steps, restart options),
    - initialization of stochastic code (call to `sto_mod_init`),
    - a loop on time steps to update the stochastic fields (call to `sto_mod`)
      and store them in files (call to `sto_write`).

- **_stomod_** :
    Main stochastic module (model dependent),
    embedding all dynamical stochastic parameterizations:
    - initialization phase (routine `sto_mod_init`):
      - initialization of every dynamical stochastic parameterizations (here only `sto_template_init`),
      - initialization of the structure of the stochastic arrays (call to `sto_array_init`),
      - initialization of the time iteration of the stochastic arrays (call to `sto_par_init`);
    - time update (routine `sto_mod`):
      - update stochastic fields (call to `sto_par`),
      - apply dynamical stochastic parameterization (call to `sto_template`).

    The routines may need to be organized differently depending on
    where the stochastic parameterization code must be used in the geohysical model.
    
- **_stotemplate_** :
    Template for including a new dynamical stochastic parameterization in the geohysical model.
    This illustrates how to make requests for stochastic fields with user-defined fetaures
    and how to use the resulting stochastic fields in the model.
    - initialization phase (routine `sto_template_init`):
      - request index for a new stochastic field (call to `sto_array_request_new`),
      - define the features of the stochastic field with the corresponding index
        (by filling parameters like `stofields(index)%type_xy` specifying
	the requested type of xy correlation strcuture),
    - time update (routine `sto_template`):
      - make use of the stochastic field in the model (`stofields(index)%sto2d`,
        `stofields(index)%sto3d`, or `stofields(index)%sto0d`, depending on the requested
	dimension of the stochastic field, stored in `stofields(index)%dim`).

- **_stowrite_** :
    Write the resulting stochastic fields in a NetCDF file.

- **_stoexternal_** :
    This module is used by the stochastic modules below to get all information
    they need from the geohysical model: type of variables, description of the model grid,
    ensemble parameters, lateral boundary conditions (or connection between subdomains).
    This is the only place where model data go to the stochastic modules,
    so that this can be easily identified and possibly upgraded. This is model dependent.

### Stochastic modules

- **_stoarray_** :
    This is the data module, where all stochastic fields are defined and stored:
    - it receives the requests from the users
      (with routines `sto_array_request_size` and `sto_array_request_new`),
    - it allocates the required arrays according to requests,
      and check the consistency of the requested features
      (with routine `sto_array_init`).

- **_stopar_** :
    This is the time evolution module, where all stochastic fields are updated at each time step:
    - initialization phase (routine `sto_par_init`):
      - seed random number generator (according to subdomain and member indices),,
      - initialize methods to generate spatially correlated random fields
        (calls to `sto_diff_init`, `sto_kernel_init`,...),
      - initialize transformations to requested marginal distributions
        (call to `sto_marginal_init`),
      - initialize parameters of autoregressive processes,
      - initialize random fields (from restart or from the requested method
        to generate spatially correlated random fields: `sto_diff`, `sto_kernel`,...);
    - time update (routine `sto_par`):
      - forward the autoregressive process in time  (or
        interpolate between a past and future state of the autoregressive process),
      - perform the transformation to the requested marginal distribution.

- **_stowhite_** :
    Generate a map of Gaussian white noise, with zero mean and unit standard deviation.

- **_stodiff_** :
    Generate a map of spatiallye correlated noise, with zero mean and unit standard deviation,
    using the recursive application of a diffusion operator on a white noise.

- **_stokernel_** :
    Generate a map of spatiallye correlated noise, with zero mean and unit standard deviation,
    using the convolution of a white noise with a filtering kernel.

- **_stosobolseq_** :

- **_stomarginal_** :
    Transform the Gaussian process to the requested marginal distribution.

- **_storng_kiss_** :
    Random number generator. This includes the kiss32 and kiss64 random number generators
    and the polar method to transform the integer sequence into Gaussian numbers.

- **_storng_ziggurat_** :
    Random number generator. This includes the shr3 random number generator
    and the ziggurat method to transform the integer sequence into Gaussian numbers.

- **_storng_check_** :
    Check relative performance of random number generators.

- **_storst_** :
    Read and write from restart file (not yet implemented).
