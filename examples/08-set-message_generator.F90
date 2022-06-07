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

  subroutine generator(info, string)
    type(fexception_info), intent(in) :: info
    character(len=:), allocatable, intent(out) :: string
    character(len=10) :: lstr

    string = "Custom error output" // new_line('a')
    !File will not be allocated if not specified by the calling code so test for it
    if (allocated(info%file)) then
      string = string // "File : " // info%file // new_line('a')
      write(lstr,'(I0)') info%line
      string = string // "Line : " // trim(lstr) // new_line('a')
    end if
    string = string // "Error : " // info%name // new_line('a')
    string = string // "Message : " // info%message // new_line('a')
  end subroutine generator

end module demo

program demo_prog
  use demo
  implicit none

  lt0 = register_error("Less than zero", exit_code = -1)
  gt10 = register_error("Greater than zero", exit_code = -2)

  print *,'This version of the code uses a custom error printer'
  print *,'to add an extra header to the error information that'
  print *,'is printed'

  call set_error_message_generator(generator)

  call demo_sub(5)
  print *,'5 works fine'

  call demo_sub(-1)
  print *,'-1 is an error but a handleable one'

  call demo_sub(15)
  print *,'You will never see this line because 15 is not handleable'

end program demo_prog
