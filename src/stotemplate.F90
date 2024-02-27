MODULE stotemplate
   !!======================================================================
   !!                       ***  MODULE stotemplate  ***
   !!
   !! Purpose : Template module for a new stochastic scheme
   !!           Illustrating how to use the stochastic modules
   !!======================================================================
   USE stoarray

   IMPLICIT NONE
   PRIVATE

   INTEGER :: jstotemplate1   ! index of stochastic field used in this scheme
   INTEGER :: jstotemplate2   ! a possible second index
   INTEGER :: jstotemplate3   ! a third, it could be an array...

   PUBLIC sto_template, sto_template_init

CONTAINS

   SUBROUTINE sto_template(kt)
      !!----------------------------------------------------------------------
      !!
      !!                     ***  ROUTINE sto_template  ***
      !!
      !! This routine is called at every time step
      !! to make appropriate use of the stochastic field
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt   ! ocean time-step index

      ! Here we just write some statistics of the current stochastic field
      ! print *, 'min',MINVAL(stofields(jstotemplate1)%sto2d)
      ! print *, 'max',MAXVAL(stofields(jstotemplate1)%sto2d)

   END SUBROUTINE sto_template


   SUBROUTINE sto_template_init
      !!----------------------------------------------------------------------
      !!
      !!                     ***  ROUTINE sto_template_init  ***
      !!
      !! This routine is calle at initialization time
      !! to request stochastic field with appropriate features
      !!
      !!----------------------------------------------------------------------

      ! Read namelist block corresponding to this stochastic scheme
      ! -> get parameters

      ! Request index for an new stochastic array
      CALL sto_array_request_new(jstotemplate1)

      ! Set features of the requested stochastic field
      stofields(jstotemplate1)%type_t='arn'
      stofields(jstotemplate1)%corr_t=10.0
      stofields(jstotemplate1)%nar_order=1

      ! Request index for an new stochastic array
      CALL sto_array_request_new(jstotemplate2)

      ! Set features of the requested stochastic field
      stofields(jstotemplate2)%type_t='arn'
      stofields(jstotemplate2)%type_xy='kernel'
      stofields(jstotemplate2)%type_xy='diffusive'
      stofields(jstotemplate2)%corr_t=5.0
      stofields(jstotemplate2)%corr_xy=50.0
      stofields(jstotemplate2)%nar_order=2
      stofields(jstotemplate2)%nar_update=5
      stofields(jstotemplate2)%ker_type=0
      stofields(jstotemplate2)%ker_coord=0
      stofields(jstotemplate2)%diff_passes=50
      stofields(jstotemplate2)%type_variate='lognormal'
      stofields(jstotemplate2)%ave=1.0
      stofields(jstotemplate2)%std=0.5
      stofields(jstotemplate2)%type_variate='bounded_atan'
      stofields(jstotemplate2)%ave=0.7
      stofields(jstotemplate2)%std=0.1
      stofields(jstotemplate2)%min=0.
      stofields(jstotemplate2)%max=1.
      stofields(jstotemplate2)%type_variate='wrapped_normal'
      stofields(jstotemplate2)%ave=0.5
      stofields(jstotemplate2)%std=1.0

   END SUBROUTINE sto_template_init

   !!======================================================================
END MODULE stotemplate
