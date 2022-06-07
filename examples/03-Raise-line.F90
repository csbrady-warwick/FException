module demo

  use fexception_mod
  implicit none
  type(fexception_handle) :: error

  contains

  subroutine demo_sub
    type(fexception) :: exception
    logical, parameter :: is_error = .true.

    if (is_error) &
        call exception%raise(error,message="There was an error", &
            file = __FILE__, line = __LINE__)
    if (exception%test()) RETURN
  end subroutine demo_sub

end module demo

program demo_prog
  use demo
  implicit none

  error = register_error("general error", exit_code = -1)
  call demo_sub
  print *,'code completed without error'

end program demo_prog
