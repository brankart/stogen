MODULE stoexternal
   !!======================================================================
   !!                       ***  MODULE  stoexternal  ***
   !! Purpose        : external resources provide by the model (user supplied)
   !!=====================================================================
   !!   lbc_lnk      : generic interface for lbc_lnk_3d and lbc_lnk_2d
   !!   ctl_nam      : generate error message if failed to read namelist
   !!----------------------------------------------------------------------

   ! Type of variables
   INTEGER, PUBLIC, PARAMETER ::   sp = SELECTED_REAL_KIND( 6, 37)   !: single precision (real 4)
   INTEGER, PUBLIC, PARAMETER ::   dp = SELECTED_REAL_KIND(12,307)   !: double precision (real 8)
   INTEGER, PUBLIC, PARAMETER ::   wp = dp                              !: working precision
   INTEGER, PUBLIC, PARAMETER ::   i4 = SELECTED_INT_KIND( 9)        !: single precision (integer 4)
   INTEGER, PUBLIC, PARAMETER ::   i8 = SELECTED_INT_KIND(14)        !: double precision (integer 8)
   INTEGER, PUBLIC, PARAMETER ::   lc = 256                          !: Length of Character strings

   ! Problem dimension
   INTEGER, PUBLIC, PARAMETER ::   jpi = 361      !: size dimension 1
   INTEGER, PUBLIC, PARAMETER ::   jpj = 181      !: size dimension 2
   INTEGER, PUBLIC, PARAMETER ::   jpk = 1        !: size dimension 3
   INTEGER, PUBLIC ::   narea = 1      !: index of local domain
   INTEGER, PUBLIC ::   mppsize = 1    !: number of processes
   INTEGER, PUBLIC ::   jpiglo = jpi   !: size of global domain (dim 1)
   INTEGER, PUBLIC ::   jpjglo = jpj   !: size of global domain (dim 2)

   ! Description of the grid
   INTEGER, PUBLIC, SAVE, DIMENSION(jpi)      :: mig            ! index of grid point in global grid
   INTEGER, PUBLIC, SAVE, DIMENSION(jpj)      :: mjg            ! index of grid point in global grid
   REAL(wp), PUBLIC, SAVE, DIMENSION(jpi,jpj) :: glamt, gphit   ! longitude and latitude
   REAL(wp), PUBLIC, SAVE, DIMENSION(jpi,jpj) :: glamtglo       ! global longitude
   REAL(wp), PUBLIC, SAVE, DIMENSION(jpi,jpj) :: gphitglo       ! global latitude

   ! Description of the mask (real model arrays)
   REAL(wp), PUBLIC, SAVE, DIMENSION(jpi,jpj,jpk) :: mask_t = 1  ! land/ocean mask at T-points
   REAL(wp), PUBLIC, SAVE, DIMENSION(jpi,jpj,jpk) :: mask_u = 1  ! land/ocean mask at U-points
   REAL(wp), PUBLIC, SAVE, DIMENSION(jpi,jpj,jpk) :: mask_v = 1  ! land/ocean mask at V-points

   ! Description of the mask (pointers used in stochastic codes)
   LOGICAL, PUBLIC, SAVE  :: use_mask3d = .TRUE.
   INTEGER, PUBLIC, SAVE  :: grid_type = 1
   REAL(wp), PUBLIC, SAVE, DIMENSION(:,:),   POINTER :: rmask2d ! land/ocean mask at T-points
   REAL(wp), PUBLIC, SAVE, DIMENSION(:,:),   POINTER :: umask2d ! land/ocean mask at U-points
   REAL(wp), PUBLIC, SAVE, DIMENSION(:,:),   POINTER :: vmask2d ! land/ocean mask at V-points
   REAL(wp), PUBLIC, SAVE, DIMENSION(:,:,:), POINTER :: rmask3d ! land/ocean mask at T-points
   REAL(wp), PUBLIC, SAVE, DIMENSION(:,:,:), POINTER :: umask3d ! land/ocean mask at U-points
   REAL(wp), PUBLIC, SAVE, DIMENSION(:,:,:), POINTER :: vmask3d ! land/ocean mask at V-points

   ! I/O parameters
   INTEGER, PUBLIC ::   numout      =    6      !: logical unit for output print; set to stdout; do not change
   LOGICAL, PUBLIC ::   lwm         = .TRUE.    !: true on the 1st processor only (always)
   LOGICAL, PUBLIC ::   lwp         = .TRUE.    !: true on the 1st processor only .OR. ln_ctl
   INTEGER, PUBLIC ::   numnam_ref  =   -1      !: logical unit for reference namelist
   INTEGER, PUBLIC ::   numnam_cfg  =   -1      !: logical unit for configuration specific namelist
   INTEGER, PUBLIC ::   numond      =   -1      !: logical unit for Output Namelist Dynamics

   ! Ensemble parameters
   CHARACTER(len=3), PUBLIC :: cn_mem='001'            !: charcater string with ensemble member index
   LOGICAL, PUBLIC          :: ln_ensemble = .FALSE.   !: control of ensemble simulations
   LOGICAL, PUBLIC          :: ln_ens_rst_in = .FALSE. !: use ensemble (T) or single (F) input restart file
   INTEGER, PUBLIC          :: nn_ens_size = 1         !: ensemble size
   INTEGER, PUBLIC          :: nn_ens_start = 1        !: index of the first ensemble member
   INTEGER, PUBLIC          :: nmember = 1             !: index of current ensemble member

   ! Public routines
   INTERFACE lbc_lnk
      MODULE PROCEDURE lbc_lnk_2d, lbc_lnk_3d
   END INTERFACE

   PUBLIC ctl_nam, initialize_grid, initialize_mask, broadcast_array

CONTAINS

   SUBROUTINE lbc_lnk_2d( pt2d, cd_type, psgn )
      !!---------------------------------------------------------------------
      !!                 ***  ROUTINE lbc_lnk_2d  ***
      !!
      !! ** Purpose :   set lateral boundary conditions on a 2D array (non mpp case)
      !!
      !! ** Method  :   psign = -1 :    change the sign across the north fold
      !!                      =  1 : no change of the sign across the north fold
      !!                      =  0 : no change of the sign across the north fold and
      !!                             strict positivity preserved: use inner row/column
      !!                             for closed boundaries.
      !!----------------------------------------------------------------------
      CHARACTER(len=1)            , INTENT(in   )           ::   cd_type   ! nature of pt3d grid-points
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout)           ::   pt2d      ! 2D array on which the lbc is applied
      REAL(wp)                    , INTENT(in   )           ::   psgn      ! control of the sign

      ! periodic condition in longitude
      pt2d(1,:)   = pt2d(jpi-1,:)
      pt2d(jpi,:) = pt2d(2,:)

      ! set condition at poles
      pt2d(:,1)   = SUM(pt2d(1:jpi-2,2))/(jpi-2)
      pt2d(:,jpj) = SUM(pt2d(1:jpi-2,jpj-1))/(jpi-2)

   END SUBROUTINE lbc_lnk_2d


   SUBROUTINE lbc_lnk_3d( pt3d, cd_type, psgn )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE lbc_lnk_3d  ***
      !!
      !! ** Purpose :   set lateral boundary conditions on a 3D array (non mpp case)
      !!
      !! ** Method  :   psign = -1 :    change the sign across the north fold
      !!                      =  1 : no change of the sign across the north fold
      !!                      =  0 : no change of the sign across the north fold and
      !!                             strict positivity preserved: use inner row/column
      !!                             for closed boundaries.
      !!----------------------------------------------------------------------
      CHARACTER(len=1)                , INTENT(in   )           ::   cd_type   ! nature of pt3d grid-points
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout)           ::   pt3d      ! 3D array on which the lbc is applied
      REAL(wp)                        , INTENT(in   )           ::   psgn      ! control of the sign

      INTEGER :: jk

      ! periodic condition in longitude
      pt3d(1,:,:)   = pt3d(jpi-1,:,:)
      pt3d(jpi,:,:) = pt3d(2,:,:)

      ! set condition at poles
      DO jk = 1, jpk
        pt3d(:,  1,jk) = SUM(pt3d(1:jpi-2,    2,jk))/(jpi-2)
        pt3d(:,jpj,jk) = SUM(pt3d(1:jpi-2,jpj-1,jk))/(jpi-2)
      ENDDO

   END SUBROUTINE lbc_lnk_3d


   SUBROUTINE ctl_nam ( kios, cdnam, ldwp )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE ctl_nam  ***
      !!
      !! ** Purpose :   Informations when error while reading a namelist
      !!----------------------------------------------------------------------
      INTEGER          , INTENT(inout) ::   kios      ! IO status after reading the namelist
      CHARACTER(len=*) , INTENT(in   ) ::   cdnam     ! group name of namelist for which error occurs
      CHARACTER(len=5)                 ::   clios     ! string to convert iostat in character for print
      LOGICAL          , INTENT(in   ) ::   ldwp      ! boolean term for print
      !!----------------------------------------------------------------------
      WRITE (clios, '(I5.0)') kios
      IF( kios < 0 ) THEN
         print *, 'W A R N I N G:  end of record or file while reading namelist ' &
 &           // TRIM(cdnam) // ' iostat = ' // TRIM(clios)
      ENDIF

      IF( kios > 0 ) THEN
         print *, 'E R R O R :   misspelled variable in namelist ' &
 &           // TRIM(cdnam) // ' iostat = ' // TRIM(clios) 
      ENDIF
      kios = 0
      RETURN
   END SUBROUTINE ctl_nam


   SUBROUTINE initialize_grid
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE initialize_grid  ***
      !!
      !! ** Purpose :   initialization of grid features
      !!----------------------------------------------------------------------
      INTEGER :: ji, jj

      DO ji= 1, jpi
        mig(ji) = ji
      ENDDO

      DO jj= 1, jpj
        mjg(jj) = jj
      ENDDO

      DO jj= 1, jpj
      DO ji= 1, jpi
        glamt(ji,jj) = 360._wp * REAL(ji-1,wp) / REAL(jpi-2,wp)
        gphit(ji,jj) = 180._wp * REAL(jj-1,wp) / REAL(jpj,wp) - 90._wp
        glamtglo(ji,jj) = glamt(ji,jj)
        gphitglo(ji,jj) = gphit(ji,jj)
      ENDDO
      ENDDO

   END SUBROUTINE initialize_grid


   SUBROUTINE initialize_mask
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE initialize_mask  ***
      !!
      !! ** Purpose :   initialization of mask features
      !!----------------------------------------------------------------------

      rmask3d => mask_t
      umask3d => mask_u
      vmask3d => mask_V

      rmask3d(jpi/2,jpj/4:3*jpj/4,:) = 0.
      umask3d(jpi/2,jpj/4:3*jpj/4,:) = 0.
      umask3d(jpi/2-1,jpj/4:3*jpj/4,:) = 0.
      vmask3d(jpi/2,jpj/4-1:3*jpj/4,:) = 0.

      tmask3d(jpi/4:3*jpi/4,jpj/2,:) = 0.
      umask3d(jpi/4:3*jpi/4,jpj/2,:) = 0.
      umask3d(jpi/4:3*jpi/4,jpj/2-1,:) = 0.
      umask3d(jpi/4-1:3*jpi/4,jpj/2,:) = 0.

   END SUBROUTINE initialize_mask


   SUBROUTINE broadcast_array( ptab )
      REAL(wp), DIMENSION(:), INTENT(in) :: ptab   ! array to broadcast

      ! Insert MPI broadcast code here

   END SUBROUTINE broadcast_array

END MODULE stoexternal
