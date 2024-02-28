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
    - initialization of stochastic code (call to _sto_mod_init_),
    - a loop on time steps to update the stochastic fields (call to sto_mod)
      and store them in files (call to sto_write).

- **_stomod_** :
    Main stochastic module (model dependent),
    embedding all dynamical stochastic parameterizations:
    - initialization phase (routine sto_mod_init):
      - initialization of every dynamical stochastic parameterizations (here only sto_template_init),
      - initialization of the structure of the stochastic arrays (call to sto_array_init),
      - initialization of the time iteration of the stochastic arrays (call to sto_par_init);
    - time update (routine sto_mod):
      - update stochastic fields (call to sto_par),
      - apply dynamical stochastic parameterization (call to sto_template).
    

- **_stotemplate_** :

- **_stowrite_** :

- **_stoexternal_** :

### Stochastic modules

- **_stoarray_** :

- **_stopar_** :

- **_stowhite_** :

- **_stodiff_** :

- **_stokernel_** :

- **_stosobolseq_** :

- **_stomarginal_** :

- **_storng_kiss_** :

- **_storng_ziggurat_** :

- **_storng_check_** :

- **_storst_** :
