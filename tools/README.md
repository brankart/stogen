## stogen installation directory

To compile the library and the examples :

- create a 'make.macro' file corresponding to your compiler in the 'macro' directory.
  This is the Makefile configurable part, which specifies
  your compiler options and where to find the NetCDF library.

```bash
ln -sf macro/make.(MY_MACHINE) Makefile.macro
```

- install the code (as in 'install.bash'):

```bash
target="$HOME/bin/stogen"

./mkmf -t Makefile.macro -p $target ../src/*.[Ffh]90

make
```

- recompile with:

```bash
make
```

- update the Makefile (if the source are modified) with :

```bash
target="$HOME/bin/stogen"
./mkmf -t Makefile.macro -p $target ../src/*.[Ffh]90
```
