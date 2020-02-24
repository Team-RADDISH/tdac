module m_pdaf

  use mpi
  use m_params
  
  implicit none
  
contains

  subroutine init_pdaf(dim_state, dim_ensemble, outflag)

    integer, intent(in) :: dim_state
    integer, intent(in) :: dim_ensemble
    integer, intent(out) :: outflag

    integer :: err
    
    call MPI_Init(err)

    param_int(1) = dim_state
    param_int(2) = dim_ensemble
    param_int(3) = resampling_type
    param_int(4) = perturbation_type

    param_real(1) = perturbation_level
    
    call PDAF_init(filter_type, filter_subtype, stepnull, param_int, dim_pint, &
         param_real, dim_preal, MPI_COMM_WORLD, MPI_COMM_WORLD, MPI_COMM_WORLD, &
         task_id, n_modeltasks, in_filterpe, init_ensemble, in_screen, &
         outflag)
    
  end subroutine init_pdaf

  subroutine init_ensemble(filtertype, dim_p, dim_ens, state_p, Uinv, ens_p, flag)

    integer, intent(in) :: filtertype
    integer, intent(in) :: dim_p
    integer, intent(in) :: dim_ens
    real, dimension(:), intent(inout) :: state_p
    real, dimension(:,:), intent(inout) :: Uinv
    real, dimension(dim_p, dim_ens), intent(inout) :: ens_p
    integer, intent(inout) :: flag

    ens_p = 0.0
    flag = 0
    
  end subroutine init_ensemble
  
  subroutine finalize_pdaf()

    integer :: err
    
    call MPI_Finalize(err)
    
  end subroutine finalize_pdaf

end module m_pdaf
