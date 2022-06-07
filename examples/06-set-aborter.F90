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

  subroutine aborter(exception, error_code)
    type(fexception), intent(in) :: exception
    integer, intent(in) :: error_code

    print *, "Aborting with error code ", error_code
    stop
  end subroutine aborter

end module demo

program demo_prog
  use demo
  implicit none

  lt0 = register_error("Less than zero", exit_code = -1)
  gt10 = register_error("Greater than zero", exit_code = -2)

  print *, 'This version of the code uses a custom abort handler'
  print *, 'In this case, it prints a custom error message and then '
  print *, 'stops by calling stop rather than error stop so there is '
  print *, 'no backtrace'

  call set_abort(aborter)

  call demo_sub(5)
  print *,'5 works fine'

  call demo_sub(-1)
  print *,'-1 is an error but a handleable one'

  call demo_sub(15)
  print *,'You will never see this line because 15 is not handleable'

end program demo_prog
