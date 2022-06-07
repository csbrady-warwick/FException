  program demo
    use fexception_mod
    implicit none
    type(fexception_handle) :: range_error, key_error
    range_error = register_error("Index out of range", exit_code = -1)
    key_error = register_error("Invalid key", exit_code = -2)
  end program demo

