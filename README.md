# FException - An approach to error handling in Fortran

## Aims

The aim of FException is to provide an easy but flexible mechanism for handling errors in Fortran. Fortran doesn't have true exceptions and cannot implement them directly without fundamental changes to the language. Despite its name FException doesn't try to implement C++/Python style exceptions but has its own approach that is a mixture of classical exceptions with propagating error codes

## Definitions

* Host Code - The program that is using the FException library

* Problem - A well defined failure *mode* of all or part of the host code. This term is introduced to separate the concept of a *problem* that has occurred in the host code from an *error* which is part of the FException library and represents the library dealing with a *problem* that has encountered in the host code 

* Error - The concept in FException of a *problem* in the *host code* that has been registered as something that can be reported through FException

* Error handle - A program object in the *host code* that defines an *error*

* Error state - A flag recording whether a given *error* is *active* or *inactive*

* Active - Of an *error state* - the matching *problem* in the *host code* has occurred and has not yet been *caught*

* Inactive - Of an *error state*  - the matching *problem* in the *host code* either has not occurred or has been *caught* after it has occurred

* Catch/Caught - The process of testing for one or more *active* *error states*, dealing with the *problem* that they represent and then *inactivating* the *error state*

* Raise - Flag a given *error state* as being *active*

* Exception - A program object in the *host code* that contains *error states* and provides access to their states using *error handles*

## Concepts

FException is an error handling library that has an approach that is somewhere between classic bit mask error codes and C++ and Python exceptions. The features of FException are

* Pass exception objects through your code where at any point you can raise an error to flag that a problem has occurred, test for a problem having occurred or catch an error state which both tests for occurrence and also inactivates the error state.

* Errors both have generic names (created when the error state is created) and specific messages that are specified when the error is raised and both are reported if the error is not caught

* Semi-automatic handling of uncaught errors. When the exception object goes out of scope then any uncaught errors automatically terminate your code

* Error states created by composition rather than inheritance. Rather than deriving an exception object from a base class, you can flag multiple error states as active. For example this would allow an error caused by being unable to open a database file as being both a database error and a filesystem error

* Easy custom termination code. Library such as MPI often require non standard methods of terminating your code to achieve clean shutdown (i.e. MPI_Abort). FException makes it easy to write your code to call these non standard termination functions

* Easy output format variation. FException has a built in format for reporting exceptions but this can easily be overridden in the host code

* Output to multiple LUNs. By default FException reports error states to error_unit (stderr) but this can easily be overridden by the host code to allow writing to multiple LUNs, allowing reporting of errors to log files as well as to screen.

* Complete control of final output. If you are using a logging library or you need to send you messages over a communication channel rather than reporting them to a file you can get the error output from FException as a string before the code terminates allowing you more control over how to use the error results.


## Getting started

FException is a single module that follows the Fortran 2018 standard with a fallback mode of Fortran 2008 (for compilers that support preprocessors, define the preprocessor macro "F2008" (without quotes) to engage the fallback mode). You should ship FException with your code (specifically the file fexcept.F90) and compile it as part of the host code compilation. FException has no dependecies except on intrinsic modules. The FException module is called "fexception_mod" and must be used when FException objects or subroutines are wanted. It is defined in the single file src/exception.F90. FException is basically a Fortran version of a C++ header only library. The idea is that you compile the source code into your code rather than linking against a centrally installed version of the library. Usually you would want to ship FException with your code. You do not need to ship this documentation with FException when it is included in your code, and you may change FException as you wish but please do not remove the copyright information at the top of the file

### Getting version information

You can get version information about FException using the "get_version" function. This function takes three optional parameters : version, major_revision and minor_revision. Changes to minor revision numbers are changes that do not alter any existing part of the interface to the code - bug fixes, internal modifications to the code etc. Changes to major revisions do alter existing parts of the interface but do not break any existing code. This would include adding additional information to error states, adding that information to the fexception_info object, adding new features to the library etc. Changes to the version number *may* add breaking changes to the interface.

Since FException is intended to be shipped with a code rather than linked against a centrally installed version, version checking shouldn't be needed in general but is included so that it can be used if needed

### Registering exceptions

The first thing that needs to be done to use FException is to register the problems that can occur in the host code. This is done using the function "register_error". This function takes one required parameter and two optional parameters and returns an object of type(fexception_handle). The required parameter is a string describing the general class of the error. This string should be useful and valid to describe any possible instance of the error being used. For example if you intend to use an error handle to refer to an attempt to access an array with an out of bounds index then "Array out of bounds" would be a good description. The two optional parameters are "exit_code" which is an integer describing which error code the program should return if it exits due to this error (this only happens if compiled with Fortran 2018 support. In Fortran 2008 mode it prints the error code but always returns the code -1). The final parameter is "is_fatal" which can be set to .FALSE. to flag that the error should not cause the program to terminate even if it is not handled when the exception is resolved. Non fatal errors print error messages the same as fatal errors but do not cause the program to terminate. Setting "is_fatal" to .TRUE. or not passing the parameter means that the error is fatal. An example of registering exceptions would be

```fortran

  program demo
    use fexception_mod
    implicit none
    type(fexception_handle) :: range_error, key_error
    range_error = register_error("Index out of range", exit_code = -1)
    key_error = register_error("Invalid key", exit_code = -2)
  end program demo

```

### Creating an exception object

The core of the FException system is the "type(fexception)" object and you have to create an instance of it to actually report errors. The fexception object doesn't need to be manually set up except in some very specific circumstances which will be mentioned in the advanced section. Simply create it and use it.

### Raising an exception

Raising an exception is done by calling the raise method of the fexception object. You raise an exception using the handle that you registered. Raising an error flags that an error has occurred and the error must be handled. Because there is no direct language support for exceptions in Fortran raising an exception doesn't automatically exit the subroutine. You can test for an exception having been raised using the "test" method of the fexception object. If it returns true then an error has been raised. It is important to note that having an fexception object with flagged errors doesn't do anything until the fexception object goes out of scope. When this happens the code will error stop if there are still errors flagged in the fexception object. 

A simple example shows how to raise an exception and then to test whether the exception has been raised

```Fortran
module demo

  use fexception_mod
  implicit none
  type(fexception_handle) :: error

  contains

  subroutine demo_sub
    type(fexception) :: exception
    logical, parameter :: is_error = .true.

    if (is_error) &
        call exception%raise(error,message="There was an error")
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
```

The output of this is

```
****************************************
Exception 1 triggered - general error
Message - There was an error
----------------------------------------
Stopping on first fatal error. All errors have been reported
but the error code reported is from the first error in the list
****************************************
ERROR STOP -1

Error termination. Backtrace:
#0  0x1006b9113 in ???
#1  0x1006b9dbf in ???
#2  0x1006bb113 in ???
#3  0x1002ef5fb in __fexception_mod_MOD_fail
	at src/exception.F90:627
#4  0x1002ec39f in __fexception_mod_MOD_destructor
	at src/exception.F90:639
#5  0x1002eba5b in __fexception_mod_MOD___final_fexception_mod_Fexception
	at src/exception.F90:643
#6  0x1002f2e9b in __demo_MOD_demo_sub
	at examples/02-Raise.F90:16
#7  0x1002f2eeb in demo_prog
	at examples/02-Raise.F90:25
#8  0x1002f2f77 in main
	at examples/02-Raise.F90:21
```

The first part of it (contained within the asterisks) is the output from the FException library itself. It contains the general class description of the error that was provided when the error was registered and also the specific message that was created when the error was raised. After that the code calls "ERROR STOP" with the numerical error code that was provided when the error was registered. Depending on your compiler you may or may not see the backtrace that we see in the above error from gfortran. If you do get a backtrace then you have to remember that it reports information about the internal elements of FException as well as your actual code. All of the elements that mention __fexception_mod (the exact syntax will depend on which compiler you used but you would usually expect fexception_mod to be a part of the name) are inside the fexception mod and should be ignored. The final line inside the original code is at stack frame element #6 __demo_MOD_demo_sub line 16. This is the line where the FException object went out of scope and was not handled. Unfortunately this is generally the last line of the function that the FException object was created in, even when you returned early from the function. In general you want to know the line where the exception was raised anyway, not the line where the code finally decided that it was unable to handle it.

When you raise an exception you can optionally specify "file" and "line" parameters to the raise call to specify the filename and line number that raised the exception. In most modern Fortran compilers there are C style macros defined that contain that information (if your Fortran file is preprocessed at all), __FILE__ and __LINE__, so the following code will cause FException to tell you exactly where the error occurred.

```Fortran

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
```

This changes the output to include the filename and line number that raised the exception

```
****************************************
Exception 1 triggered - general error
Found in file examples/03-Raise-line.F90 at line 15
Message - There was an error
----------------------------------------
Stopping on first fatal error. All errors have been reported
but the error code reported is from the first error in the list
****************************************
ERROR STOP -1

Error termination. Backtrace:
#0  0x101381113 in ???
#1  0x101381dbf in ???
#2  0x101383113 in ???
#3  0x100ebb04b in __fexception_mod_MOD_fail
	at src/exception.F90:636
#4  0x100eb7def in __fexception_mod_MOD_destructor
	at src/exception.F90:648
#5  0x100eb74ab in __fexception_mod_MOD___final_fexception_mod_Fexception
	at src/exception.F90:652
#6  0x100ebee4b in __demo_MOD_demo_sub
	at examples/03-Raise-line.F90:17
#7  0x100ebee9b in demo_prog
	at examples/03-Raise-line.F90:26
#8  0x100ebef27 in main
	at examples/03-Raise-line.F90:22
```

### Catching an error

The previous section describes how to create errors, how to raise the errors that you have created and shown how the errors cause the program to terminate when the FException object goes out of scope. The next question is how do you trap errors that you can handle and strip them out of the  FException object so that they don't cause termination. This is done using the "catch" method. There are several versions of "catch" but the most commonly used one takes a single exception handle as a parameter.

```Fortran
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
      call exception%raise(lt0,message="There was an error", &
          file = __FILE__, line = __LINE__)
      return
    end if

    if (value > 10) then
      call exception%raise(gt10,message="There was an error", &
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

  call demo_sub(5)
  print *,'5 works fine'

  call demo_sub(-1)
  print *,'-1 is an error but a handleable one'

  call demo_sub(15)
  print *,'You will never see this line because 15 is not handleable'

end program demo_prog
```

```
 Handling value            5
 5 works fine
 Exception handled for value           -1
 -1 is an error but a handleable one
****************************************
Exception 1 triggered - Greater than zero
Found in file examples/04-Catch.F90 at line 34
Message - This error cannot be handled
----------------------------------------
Stopping on first fatal error. All errors have been reported
but the error code reported is from the first error in the list
****************************************
ERROR STOP -2

Error termination. Backtrace:
#0  0x1011e1113 in ???
#1  0x1011e1dbf in ???
#2  0x1011e3113 in ???
#3  0x100ced1ff in __fexception_mod_MOD_fail
	at src/exception.F90:636
#4  0x100cedb6f in __fexception_mod_MOD_destructor
	at src/exception.F90:648
#5  0x100cedf73 in __fexception_mod_MOD___final_fexception_mod_Fexception
	at src/exception.F90:652
#6  0x100cee85b in __demo_MOD_demo_sub
	at examples/04-Catch.F90:20
#7  0x100cee9e3 in demo_prog
	at examples/04-Catch.F90:57
#8  0x100ceea4f in main
	at examples/04-Catch.F90:45
```

The catch method has two effects. The first one is that it unsets the error state that was being tested for and the second is that it returns a logical value determining whether or not the error state being tested for was set. So you would generally use it in the way shown in the above example. Call "catch" inside an "if" statement, and then inside the body of the "if" perform whatever actions are needed for handling the exception. As shown in the above example one would normally use the "test" method immediately after you have caught all of the exceptions that you intend to handle and return if there are any remaining active error states.

The second most common form of "catch" takes no parameters but still returns a logical flag. It tests for any error states being set, resets all error states that are set and returns whether any error states were set. It is used in the same way as the version that takes a single parameter but catches all errors.

The final form takes an array of exception handles and returns a logical flag. This version of the function resets any of the error states that you include in the array of handles and returns whether or not any of those states were initially set. Optionally you may supply a logical variable after the array of handles. If this logical variable is set to ```.true.``` then the behaviour of this form changes from returning true if *any* of the handles in the array are set to only returning true if *all* of the handles in the array are set. Similarly with this parameter set to true the error handles are only unset if *all* of the handles tested for are set.

In all of the cases the general approach is similar. You catch the exception(s) that you know that you can handle by testing for the return value from one of the calls to the catch method. Inside the test you put the code that deals with the exception. After the test block you then use the "test" method of the FException object to see if there are any unhandled exceptions and return if there are any.

It is worth mentioning that the "test" method also has the same varieties as the "catch" method but the most common use of "test" is the variety with no parameters.

### Difference between "catch" and "test"

The difference between "test"-ing for an error and "catch"-ing one is whether the error state is unset. A call to "catch" will unset the error state(s) that were being caught so that that error state is no longer active. A call to "test" will simply check whether any of the error state(s) tested for were true without unsetting them. The logical return value from both functions will be the same if they are called with the same parameters.

It is worth noting that the most common uses of the two functions are different. One would usually use "catch" with one or more exception handles to test for specific exceptions. This is because generally you should only be catching exceptions that you know how to deal with the problem that the raised exception indicates. 

### Manually exiting on error

Normally FException objects will cause termination of the code if there are uncaught error states when the object goes out of scope but there are some reasons why you might want an error to cause termination immediately if there are unhandled exceptions. The easiest way of doing this is to call the "fail" method of an FException object. This method takes no parameters and returns no values. If there are any error states set then the code will immediately terminate, printing the errors immediately. If no error states are set then the code will continue and the call to "fail" will do nothing.

Normally you would not want to call fail manually since it removes the main benefit of exception type error handling - allowing errors to "bubble up" to the level that can handle them. There are two main reasons why you might want to call fail manually.

The first reason is that you have written your code with your exception objects either defined in module scope or in the main program block. Under the Fortran standard finalizers are not called when the code terminates. This means that FException objects that only go out of scope when the code terminates will *not* correctly report errors automatically. If you want to have FException objects defined either in module scope or in the main program block then you will want to call the "fail" method before the code exits.

The second reason is after a function where failure of any kind is unhandleable and you want to be clear about that fact.

## Intermediate uses

### Outputting to a log file

By default FException prints output to error_unit as defined in iso_fortran_env (stdout in C terminology). If you want to output to another LUN then you do it using the "set_log_unit" function or the "set_log_unit" method of an FException object. Calling the "set_log_unit" function sets the default output unit(s) for all FException objects unless you have called the "set_log_unit" method of the FException object in which case the units that are specified there take precedence.

Both the global function and the method on the FException objects have the same three signatures. They are all subroutines and return no values and you can either pass them no parameters, a single integer LUN or an array of integer LUNs. If you pass no parameters then the behaviour is returned to default (outputting to error_unit). If you pass a single LUN then all output will be directed to that LUN, while if you provide an array of LUNs then output will be to all LUNs in the list. If you want to keep the output to error_unit then you should explicitly pass this value to the array of LUNs.

### Custom abort function

When FException objects terminate a code they do it by calling the "error stop" function and normally there is no reason to not abort your code like this. Generally unhandled exceptions mean that the code has gone wrong and no usable data can be recovered, but there are still cases where you might want to have more control over how the code terminates. For example you might have stored earlier known good states that should be written to disk before the code terminates. Alternatively you might need to call some kind of custom termination condition by a library that you are using (notably MPI codes would normally be terminated by a call to MPI_Abort). You can manually control what happens when termination occurs by setting an abort function using "set_abort"

As with setting the log outputs there are both subroutine and FException object methods of "set_abort" with the subroutine version setting the default behaviour and the method setting the abort behaviour for a specific FException object. Both take a single parameter, a function that will be called when the code should abort. The function takes two intent in parameters, one the FException object that is triggering the termination and the second the integer error code that is associated with the specific error state that is terminating the code. It is essential that the aborter function terminates the code in some fashion, and there is no guarantee that an FException object will still be valid after the aborter function has been called. 

```fortran
  subroutine aborter(exception, error_code)
    type(fexception), intent(in) :: exception
    integer, intent(in) :: error_code

    print *, "Aborting with error code ", error_code
    stop
  end subroutine aborter
```

If you call "set_abort" with no parameters as a method to an FException object then that FException object will return to using the default aborter. If you call the "set_abort" subroutine with no parameters then all FException objects using the default abort function will return to using the built in approach of calling error stop.

## Advanced Uses

### Custom printing

While you can easily tell FException to output to custom LUNs, sometimes you want to output to other mechanisms than standard Fortran LUNs. For example if you are using a logging library then you would normally call a function from that library to output error information rather than writing to a Fortran LUN. In this case you would create a custom error printer. An error printer is a function that is handed the string that would normally be printed to a LUN which can then be passed to a logging library. It is important to note that the code will be terminated almost immediately after the error printer is called so if your library has any form of buffering you should call the relevant function to flush the library's buffers before leaving the error printer function. 

You set a custom error printer using "set_error_printer" as either a subroutine or a method of an FException object. Both the function and the method take a single parameter of the function to be called when the error message is to be printed. The function takes a single assumed length intent in string as the parameter.

```fortran
  subroutine print_error(message)
    character(len=*), intent(in) :: message

    print *, repeat('=',50)
    print *, 'This is the error message'
    print *, repeat('=',50)
    print *, message
  end subroutine print_error
```
As with previous functions calling "set_error_printer" without any function parameter restores the default behaviour. Once you have set an error printer then default printing to LUNs is suppressed entirely until you restore the default behaviour by calling "set_error_printer" with no parameters.

### Custom message generator

The built in error messages always have the same form, but sometimes you might want a different form of error message. You can do this by calling the "set_error_message_generator" either as a subroutine or as a method of an FException object. Both the method and the subroutine take a single parameter which is a function to be called to generate the error message. The function is called once for each error state set in the exception object. The function takes a single type(fexception_info) intent in parameter and a deferred length allocatable character intent out parameter. 

```fortran
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
```

All of the parameters that are available to the core FException code are available through the FException_info object. With future versions of FException it is guaranteed that no information will be removed from the FException_info object, but new information may be added. In the version of FException that comes with this readme FException_info contains the following fields

```fortran
  type fexception_info
    character(len=:), allocatable :: name !< Name of exception (created at registration)
    character(len=:), allocatable :: message !< Message associated with exception (created at raise)
    character(len=:), allocatable :: file !< Filename where error state was created (optionally created at raise)
    integer :: line=-1 !< Line number where error state was created (usually created at raise if file was specified)
    integer :: exception_index !< Integer index of error state (used to allow printing error 1, error 2 etc.)
  end type fexception_info
```

All of the fields will be populated by the FException library before calling your error_message_generator except for "file" which is only allocated if a filename was specified when the exception was raised.
