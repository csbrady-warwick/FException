module demo

  use fexception_mod
  implicit none
  type(fexception_handle) :: lt0, gt10

  contains

  subroutine demo_sub(value)
    integer, intent(in) :: value
    type(fexception) :: exception

    call core(value, exception)
    if (exception%catch(lt0)) then
      print *, "Exception handled for value ", value
    end if

    if (exception%test()) return

  end subroutine demo_sub

  subroutine core(value, exception)
    integer, intent(in) :: value
    type(fexception), intent(inout) :: exception

    if (value < 0) then
      call exception%raise(lt0,message="This error can be handled", &
          file = __FILE__, line = __LINE__)
      return
    end if

    if (value > 10) then
      call exception%raise(gt10,message="This error cannot be handled", &
          file = __FILE__, line = __LINE__)
      return
    end if

    print *, 'Handling value ', value

  end subroutine core

end module demo

program demo_prog
  use demo
  implicit none

  lt0 = register_error("Less than zero", exit_code = -1)
  gt10 = register_error("Greater than zero", exit_code = -2)

  print *,'This version of the code will only output error information to &
      &a file called fort.10'

  print *,'Note that error information caused by your Fortran runtime will &
      &still output to stderr'

  call set_log_unit(10)

  call demo_sub(5)
  print *,'5 works fine'

  call demo_sub(-1)
  print *,'-1 is an error but a handleable one'

  call demo_sub(15)
  print *,'You will never see this line because 15 is not handleable'

end program demo_prog
