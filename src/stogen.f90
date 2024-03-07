PROGRAM stogen
   !!======================================================================
   !!                     ***  PROGRAM stogen  ***
   !!
   !! ** Purpose : encapsulate stomod so that it can be called
   !!              independently by other codes
   !!======================================================================
   !! History :
   !!   2024-01  (J.-M. Brankart)
   !!----------------------------------------------------------------------
   USE stoexternal, only : numnam_ref, numnam_cfg, numout, lwm, initialize_grid, initialize_mask
   USE stomod   ! stochastic module
   USE stowrite
   !
   NAMELIST/namrun/ nn_it000, nn_itend

   ! Open namelist files
   numnam_ref = 10 ; numnam_cfg = 11 ; lwm = .FALSE.
   OPEN(UNIT=numnam_ref,FILE='namelist_ref',STATUS='OLD',FORM='FORMATTED',ACCESS='SEQUENTIAL')
   OPEN(UNIT=numnam_cfg,FILE='namelist_cfg',STATUS='OLD',FORM='FORMATTED',ACCESS='SEQUENTIAL')

   ! Open output file
   !OPEN(UNIT=numout,FILE='stogen.output',STATUS='REPLACE',FORM='FORMATTED',ACCESS='SEQUENTIAL')

   ! Read namrun namelist : parameters of the run
   READ  ( numnam_ref, namrun, IOSTAT = ios)
   IF( ios /= 0 ) STOP 'Unable to read namelist_ref in stogen'
   READ  ( numnam_cfg, namrun, IOSTAT = ios)
   IF( ios /= 0 ) STOP 'Unable to read namelist_cfg in stogen'

   ! Initialize model grid and mask
   CALL initialize_grid
   CALL initialize_mask

   ! initialize stochastic code
   CALL sto_mod_init

   ! initialize the output NetCDF file
   CALL sto_write_init

   DO kt=1,nn_itend   ! Iterate on time
      ! apply stochastic parameterizations
      CALL sto_mod(kt)
      ! write stochastic fields in file
      CALL sto_write(kt)
   ENDDO
   !
   ! finalize the output NetCDF file
   CALL sto_write_final

   !!======================================================================
END PROGRAM stogen
