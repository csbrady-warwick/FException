!FException Copyright 2022 C.S.Brady
!Software is provided "as is" with no guarantees or warantees of any kind
!Please use at your own risk

MODULE fexception_mod
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : error_unit, INT64
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: fexception, fexception_handle, register_error, set_log_unit, &
      fexception_info, set_error_message_generator, set_error_printer, &
      set_abort, get_version

  INTEGER, PARAMETER :: l_version = 1
  INTEGER, PARAMETER :: l_major_revision = 0
  INTEGER, PARAMETER :: l_minor_revision = 0

!> Type defining all of the information for a single exception
!! Used to provide information about an exception for custom
!! error message generators
  TYPE fexception_info
    CHARACTER(LEN=:), ALLOCATABLE :: name !< Name of exception (created at registration)
    CHARACTER(LEN=:), ALLOCATABLE :: message !< Message associated with exception (created at raise)
    CHARACTER(LEN=:), ALLOCATABLE :: file !< Filename where error state was created (optionally created at raise)
    INTEGER :: line=-1 !< Line number where error state was created (usually created at raise if file was specified)
    INTEGER :: exception_index !< Integer index of error state (used to allow printing error 1, error 2 etc.)
  END TYPE fexception_info

!> \internal
!> Internal type representing the properties of an exception class
!! only used internally
  TYPE fexcept_info
    CHARACTER(LEN=:), ALLOCATABLE :: name
    LOGICAL :: is_fatal = .TRUE.
    INTEGER :: fatal_code = 0
  END TYPE fexcept_info

!> \internal
!> Type allowing for arrays of allocatable strings
  TYPE fexcept_string
    CHARACTER(LEN=:), ALLOCATABLE :: content
  END TYPE fexcept_string

!> Opaque handle for each exception that is registered with the system
  TYPE fexception_handle
    PRIVATE
    INTEGER(INT64) :: handle
  END TYPE fexception_handle

!> @class fexception
!> Main exception type
  TYPE fexception
    PRIVATE
    LOGICAL, DIMENSION(:), ALLOCATABLE :: exceptions
    TYPE(fexcept_string), DIMENSION(:), ALLOCATABLE :: exception_messages, exception_files
    INTEGER, DIMENSION(:), ALLOCATABLE :: exception_lines
    INTEGER, DIMENSION(:), ALLOCATABLE :: log_units
    PROCEDURE(error_printer_fn), POINTER, NOPASS :: error_printer => NULL()
    PROCEDURE(error_message_generator_fn), POINTER, NOPASS :: error_message_generator => NULL()
    PROCEDURE(abort_fn), POINTER, NOPASS :: abort => NULL()
    CONTAINS
!> Function to catch any errors in the exception object
!! made available through generic "catch" interface.
!! Unsets all errors when called
!! @result catch .TRUE. if any error state is set
    PROCEDURE :: catch_all
!> Function to catch a list of errors in the exception object
!! \memberof fexception
!! made available through the generic "catch" interface.
!! Unsets all errors in the provided array when called
!! @param exceptions Array of exception handles. Empty array will catch no errors
!! @result catch .TRUE. if any of the listed error states are set
    PROCEDURE :: catch_array
    PROCEDURE :: catch_single
    PROCEDURE :: test_all, test_array, test_single
    PROCEDURE :: clear_all, clear_array, clear_single
    PROCEDURE :: raise_array, raise_single
    PROCEDURE :: set_l_log_unit_array, set_l_log_unit_single
    PROCEDURE, PUBLIC :: allocate
    GENERIC, PUBLIC :: catch => catch_all, catch_array, catch_single
    GENERIC, PUBLIC :: clear => clear_all, clear_array, clear_single
    GENERIC, PUBLIC :: raise => raise_array, raise_single
    GENERIC, PUBLIC :: test => test_all, test_array, test_single
    GENERIC, PUBLIC :: set_log_unit => set_l_log_unit_array, set_l_log_unit_single
    PROCEDURE, PUBLIC :: set_message
    PROCEDURE, PUBLIC :: set_abort => set_l_abort
    PROCEDURE, PUBLIC :: set_error_printer => set_l_error_printer
    PROCEDURE, PUBLIC :: set_error_message_generator => set_l_error_message_generator
    PROCEDURE, PUBLIC :: fail
    FINAL :: destructor
  END TYPE fexception

  ABSTRACT INTERFACE
    SUBROUTINE abort_fn(exception, error_code)
      IMPORT fexception
      TYPE(fexception), INTENT(IN) :: exception
      INTEGER, INTENT(IN) :: error_code
    END SUBROUTINE abort_fn
    SUBROUTINE error_message_generator_fn(info, error)
      IMPORT fexception_info
      TYPE(fexception_info), INTENT(IN) :: info
      CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: error
    END SUBROUTINE error_message_generator_fn
    SUBROUTINE error_printer_fn(message)
      CHARACTER(LEN=*), INTENT(IN) :: message
    END SUBROUTINE error_printer_fn
  END INTERFACE

  INTERFACE set_log_unit
    MODULE PROCEDURE :: set_log_unit_array
    MODULE PROCEDURE :: set_log_unit_single
  END INTERFACE

  !> Global variables storing all registered exceptions and default values
  !! for some parameters
  INTEGER, SAVE :: exception_count = 0
  TYPE(fexcept_info), DIMENSION(:), ALLOCATABLE :: info
  INTEGER, DIMENSION(:), ALLOCATABLE :: default_log_units
  PROCEDURE(error_printer_fn), POINTER :: default_error_printer => NULL()
  PROCEDURE(error_message_generator_fn), POINTER :: default_error_message_generator => NULL()
  PROCEDURE(abort_fn), POINTER :: default_abort => NULL()

  CONTAINS

  SUBROUTINE get_version(version, major_revision, minor_revision)
    INTEGER, INTENT(OUT), OPTIONAL :: version, major_revision, minor_revision

    IF (PRESENT(version)) version = l_version
    IF (PRESENT(major_revision)) major_revision = l_major_revision
    IF (PRESENT(minor_revision)) minor_revision = l_minor_revision
  END SUBROUTINE get_version

  !> Internal function to allocate a string if an optional
  !! \internal
  !! source argument is present
  !! @param co Character string to allocate with source if source is present
  !! if source is absent co is deallocated (INTENT(OUT))
  !! @param cn Optional string that will allocate source with if present
  SUBROUTINE replace_string(co, cn)
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: co
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: cn
    IF (PRESENT(cn)) ALLOCATE(co, SOURCE=cn)
  END SUBROUTINE replace_string 

!> Public function to allow a user to register a class of error
!! @param name - Name of error type. Printed when error occurs
!! @param is_fatal - Logical determines whether error causes code to
!! fail. Optional, default .TRUE.
!! @param exit_code - Integer error code for code to return if
!! it exits with this error 
  FUNCTION register_error(name, exit_code, is_fatal)
    CHARACTER(LEN=*), INTENT(IN) :: name
    LOGICAL, INTENT(IN), OPTIONAL :: is_fatal
    INTEGER, INTENT(IN), OPTIONAL :: exit_code

    TYPE(fexception_handle) :: register_error
    TYPE(fexcept_info) :: temp

    IF (PRESENT(is_fatal)) temp%is_fatal = is_fatal
    IF (PRESENT(exit_code)) temp%fatal_code = exit_code
    ALLOCATE(temp%name, SOURCE=name)

    IF (ALLOCATED(info)) THEN
      info = [info, temp]
    ELSE
      info = [temp]
    END IF
    exception_count = exception_count + 1
    register_error%handle = exception_count
  END FUNCTION register_error

!> Function to allocate internal elements
!! normally called automatically but can be called by user
!! to force exception to take specific abort or error printer
!! functions
  PURE SUBROUTINE allocate(this)
    CLASS(fexception), INTENT(INOUT) :: this
    IF (ALLOCATED(this%exceptions)) RETURN

    ALLOCATE(this%exceptions(exception_count), SOURCE = .FALSE.)
    ALLOCATE(this%exception_messages(exception_count))
    ALLOCATE(this%exception_files(exception_count))
    ALLOCATE(this%exception_lines(exception_count), SOURCE = -1)
    this%abort => default_abort
    this%error_printer => default_error_printer
    this%error_message_generator => default_error_message_generator

  END SUBROUTINE allocate

!> Function to catch any errors in the exception object
!! made available through generic "catch" interface.
!! Unsets all errors when called
!! @result catch .TRUE. if any error state is set
  FUNCTION catch_all(this) RESULT(catch)
    CLASS(fexception), INTENT(INOUT) :: this
    LOGICAL :: catch
    IF (.NOT. ALLOCATED(this%exceptions)) THEN
      catch = .FALSE.
      RETURN
    END IF

    catch = ANY(this%exceptions)
    this%exceptions = .FALSE.
  END FUNCTION catch_all

!> Function to catch a list of errors in the exception object
!! \memberof fexception
!! made available through the generic "catch" interface.
!! Unsets all errors in the provided array when called
!! @param exceptions Array of exception handles. Empty array will catch no errors
!! @param only_all Optional parameter. If .TRUE. then return is only true
!! if *all* of the exceptions in the array are set rather than *any* of them
!! @result catch .TRUE. if any of the listed error states are set
  FUNCTION catch_array(this, exceptions, only_all) RESULT(catch)
    CLASS(fexception), INTENT(INOUT) :: this
    TYPE(fexception_handle), DIMENSION(:), INTENT(IN) :: exceptions
    LOGICAL, INTENT(IN), OPTIONAL :: only_all
    LOGICAL :: catch, l_only_all
    INTEGER :: iexcept

    IF (.NOT. ALLOCATED(this%exceptions)) THEN
      catch = .FALSE.
      RETURN
    END IF

    l_only_all = .FALSE.
    IF (PRESENT(only_all)) l_only_all = only_all

    IF (.NOT. l_only_all) THEN
      catch = .FALSE.
      DO iexcept = 1, SIZE(exceptions)
        catch = catch .OR. this%exceptions(exceptions(iexcept)%handle)
        this%exceptions(exceptions(iexcept)%handle) = .FALSE.
      END DO
    ELSE IF (SIZE(exceptions) > 0) THEN
      catch = .TRUE.
      DO iexcept = 1, SIZE(exceptions)
        catch = catch .AND. this%exceptions(exceptions(iexcept)%handle)
      END DO
      IF (catch) THEN
        DO iexcept = 1, SIZE(exceptions)
          this%exceptions(exceptions(iexcept)%handle) = .FALSE.
        END DO
      END IF
    ELSE
      catch = .FALSE.
    END IF
  END FUNCTION catch_array

!> Function to catch a single error in the exception object
!! \memberof fexception
!! made available through the generic "catch" interface.
!! Unsets the specified error when called
!! @param exceptions Exception handle to test
!! @result catch .TRUE. if specified error is set
  FUNCTION catch_single(this, exception, message) RESULT(catch)
    CLASS(fexception), INTENT(INOUT) :: this
    TYPE(fexception_handle), INTENT(IN) :: exception
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT), OPTIONAL :: message
    LOGICAL :: catch

    IF (.NOT. ALLOCATED(this%exceptions)) THEN
      catch = .FALSE.
      RETURN
    END IF

    IF (PRESENT(message)) THEN
      message = this%exception_messages(exception%handle)%content
    END IF

    catch = this%exceptions(exception%handle)
    this%exceptions(exception%handle) = .FALSE.
  END FUNCTION catch_single

  !> Function to clear all errors in an exception object
  !> Subroutine wrapper around catch_all function
  SUBROUTINE clear_all(this)
    CLASS(fexception), INTENT(INOUT) :: this
    LOGICAL :: dummy

    dummy = this%catch()
  END SUBROUTINE clear_all

  !> Function to clear an array of errors in an exception object
  !> Subroutine wrapper around catch_array function
  SUBROUTINE clear_array(this, exceptions)
    CLASS(fexception), INTENT(INOUT) :: this
    TYPE(fexception_handle), DIMENSION(:), INTENT(IN) :: exceptions
    LOGICAL :: dummy

    dummy = this%catch(exceptions)
  END SUBROUTINE clear_array

  !> Function to clear an error in an exception object
  !> Subroutine wrapper around catch_single function
  SUBROUTINE clear_single(this, exception)
    CLASS(fexception), INTENT(INOUT) :: this
    TYPE(fexception_handle), INTENT(IN) :: exception
    LOGICAL :: dummy

    dummy = this%catch(exception)
  END SUBROUTINE clear_single

!> Function to test if any errors are set in the exception object.
!! \memberof fexception
!! Made available through the generic "test" interface.
!! Does *not* unset any error states 
!! @result test .TRUE. if any error states are set
  FUNCTION test_all(this) RESULT(test)
    CLASS(fexception), INTENT(INOUT) :: this
    LOGICAL :: test
    IF (.NOT. ALLOCATED(this%exceptions)) THEN
      test = .FALSE.
      RETURN
    END IF

    test = ANY(this%exceptions)
  END FUNCTION test_all

!> Function to test a list of errors in the exception object
!! \memberof fexception
!! made available through the generic "test" interface.
!! Does *not* unset any error states
!! @param exceptions Array of exception handles. Empty array will catch no errors
!! @result test .TRUE. if any of the listed error states are set
  FUNCTION test_array(this, exceptions, only_all) RESULT(test)
    CLASS(fexception), INTENT(IN) :: this
    TYPE(fexception_handle), DIMENSION(:), INTENT(IN) :: exceptions
    LOGICAL, INTENT(IN), OPTIONAL :: only_all
    LOGICAL :: test, l_only_all
    INTEGER :: iexcept

    IF (.NOT. ALLOCATED(this%exceptions)) THEN
      test = .FALSE.
      RETURN
    END IF

    l_only_all = .FALSE.
    IF (PRESENT(only_all)) l_only_all = only_all

    IF (.NOT. l_only_all) THEN
      test = .FALSE.
      DO iexcept = 1, SIZE(exceptions)
        test = test .OR. this%exceptions(exceptions(iexcept)%handle)
        IF (test) RETURN
      END DO
    ELSE IF (SIZE(exceptions) > 0) THEN
      test = .TRUE.
      DO iexcept = 1, SIZE(exceptions)
        test = test .AND. this%exceptions(exceptions(iexcept)%handle)
      END DO
    ELSE
      test = .FALSE.
    END IF
  END FUNCTION test_array

!> Function to test a single error in the exception object
!! \memberof fexception
!! made available through the generic "test" interface.
!! Does *not* unset any error states
!! @param exceptions Array of exception handles. Empty array will catch no errors
!! @result test .TRUE. if the listed error state is set
  FUNCTION test_single(this, exception, message) RESULT(test)
    CLASS(fexception), INTENT(IN) :: this
    TYPE(fexception_handle), INTENT(IN) :: exception
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT), OPTIONAL :: message
    LOGICAL :: test

    IF (.NOT. ALLOCATED(this%exceptions)) THEN
      test = .FALSE.
      RETURN
    END IF
    IF (PRESENT(message)) THEN
      message = this%exception_messages(exception%handle)%content
    END IF

    test = this%exceptions(exception%handle)
  END FUNCTION test_single

!> Function to set an array of error states to active. Made available through
!! \memberof fexception
!! the generic "raise" interface
!! @param exceptions Array of exceptions to activate
!! @param file Optional string to specify the file that has raised
!! the exception. In compilers that support it this should be set to
!! __FILE__ to automatically determine this
!! @param line Optional integer specifying the line in the file that
!! raised the exception. In compilers that support it set this to
!! __LINE__ to automatically determine this
  SUBROUTINE raise_array(this, exceptions, file, line)
    CLASS(fexception), INTENT(INOUT) :: this
    TYPE(fexception_handle), DIMENSION(:), INTENT(IN) :: exceptions
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: file
    INTEGER, INTENT(IN), OPTIONAL :: line
    INTEGER :: iexcept

    CALL this%allocate()

    DO iexcept = 1, SIZE(exceptions)
      CALL replace_string(this%exception_files(exceptions(iexcept)%handle)%content, &
          file)
      IF (PRESENT(line)) THEN
        this%exception_lines(exceptions(iexcept)%handle) = line
      ELSE
        this%exception_lines(exceptions(iexcept)%handle) = -1
      END IF
      this%exceptions(exceptions(iexcept)%handle) = .TRUE.
    END DO
  END SUBROUTINE raise_array

!> Function to set a single error state to active. Made available through
!! \memberof fexception
!! the generic "raise" interface
!! @param exceptions Array of exceptions to activate
!! @param message Specify the message to go with this particular raising
!! of an error
!! @param file Optional string to specify the file that has raised
!! the exception. In compilers that support it this should be set to
!! __FILE__ to automatically determine this
!! @param line Optional integer specifying the line in the file that
!! raised the exception. In compilers that support it set this to
!! __LINE__ to automatically determine this
  SUBROUTINE raise_single(this, exception, message, file, line)
    CLASS(fexception), INTENT(INOUT) :: this
    TYPE(fexception_handle), INTENT(IN) :: exception
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: message
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: file
    INTEGER, INTENT(IN), OPTIONAL :: line

    CALL this%allocate()

    this%exceptions(exception%handle) = .TRUE.
    CALL replace_string(this%exception_messages(exception%handle)%content, message)
    CALL replace_string(this%exception_files(exception%handle)%content, file)
    IF (PRESENT(line)) THEN
      this%exception_lines(exception%handle) = line
    ELSE
      this%exception_lines(exception%handle) = -1
    END IF
    
  END SUBROUTINE raise_single

!> Function to set the message associated with a given exception
!! \memberof fexception
!! Does nothing if the exception is not set. Intended to be used either to
!! specify a message for an error state raised using an array of states
!! @param exception Exception to set the message for
!! @param message Message to set. Optional, if not present no message
  SUBROUTINE set_message(this, exception, message)
    CLASS(fexception), INTENT(INOUT) :: this
    TYPE(fexception_handle), INTENT(IN) :: exception
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: message

    IF (.NOT. this%exceptions(exception%handle)) RETURN
    CALL replace_string(this%exception_messages(exception%handle)%content, message)
  END SUBROUTINE set_message

  !> Set an array of LUNs that error output will be provided to
  !! \memberof fexception
  !! for a particular exception object
  !! Made available through the generic "set_log_unit" method
  !! @param log_units Array of LUNs to take output
  SUBROUTINE set_l_log_unit_array(this, log_units)
    CLASS(fexception), INTENT(INOUT) :: this
    INTEGER, DIMENSION(:), INTENT(IN) :: log_units
    IF (SIZE(log_units) == 0) THEN
      IF (ALLOCATED(this%log_units)) DEALLOCATE(this%log_units)
    ELSE
      this%log_units = log_units
    END IF
  END SUBROUTINE set_l_log_unit_array

  !> Set a LUN that error output will be provided to
  !! \memberof fexception
  !! for a particular exception object
  !! Made available through the generic "set_log_unit" method
  !! @param log_unit LUN to take output
  SUBROUTINE set_l_log_unit_single(this, log_unit)
    CLASS(fexception), INTENT(INOUT) :: this
    INTEGER, INTENT(IN), OPTIONAL :: log_unit

    IF (PRESENT(log_unit)) THEN
      CALL this%set_log_unit([log_unit])
    ELSE
      CALL this%set_log_unit([integer::])
    END IF
  END SUBROUTINE set_l_log_unit_single

  !> Set an array of LUNs that error output will be provided to
  !! \memberof fexception
  !! for all exception objects that do not have log units specified manually
  !! Made available through the generic "set_log_unit" function
  !! @param log_units Array of LUNs to take output
  SUBROUTINE set_log_unit_array(log_units)
    INTEGER, DIMENSION(:), INTENT(IN) :: log_units
    IF (SIZE(log_units) == 0) THEN
      IF (ALLOCATED(default_log_units)) DEALLOCATE(default_log_units)
    ELSE
      default_log_units = log_units
    END IF
  END SUBROUTINE set_log_unit_array

  !> Sets a LUN that error output will be provided to
  !! \memberof fexception
  !! for all exception objects that do not have log units specified manually
  !! Made available through the generic "set_log_unit" function
  !! @param log_unit LUN to take output
  SUBROUTINE set_log_unit_single(log_unit)
    INTEGER, INTENT(IN), OPTIONAL :: log_unit

    IF (PRESENT(log_unit)) THEN
      CALL set_log_unit([log_unit])
    ELSE
      CALL set_log_unit([integer::])
    END IF
  END SUBROUTINE set_log_unit_single

!> Function to set the aborter function for a given exception object
!! \memberof fexception
!! The aborter function is called instead of ERROR STOP when fexception
!! wants to stop a code. Intended for uses in cases such as MPI codes where
!! MPI_ABORT should be called instead of ERROR STOP.
!! @param abort Aborter function. If not specified then revert to default behaviour
  SUBROUTINE set_l_abort(this, abort)
    CLASS(fexception), INTENT(INOUT) :: this
    PROCEDURE(abort_fn), OPTIONAL :: abort
    IF (PRESENT(abort)) THEN
      this%abort => abort
    ELSE
      this%abort => NULL()
    END IF
  END SUBROUTINE set_l_abort

!> Function to set the aborter function for general exception objects
!! The aborter function is called instead of ERROR STOP when fexception
!! wants to stop a code. Intended for uses in cases such as MPI codes where
!! MPI_ABORT should be called instead of ERROR STOP.
!! @param abort Aborter function. If not specified then revert to default behaviour
!! \memberof fexception
  SUBROUTINE set_abort(abort)
    PROCEDURE(abort_fn), OPTIONAL :: abort
    IF (PRESENT(abort)) THEN
      default_abort => abort
    ELSE
      default_abort => NULL()
    END IF
  END SUBROUTINE set_abort

!> Function to be called if you want to override the default error message generation
!! for a specific exception object.
!! Note that this function is intended to change the *format* of the message, not where
!! it is printed
!! @param generator Generator function. If not specified then return to default behaviour
!! \memberof fexception
  SUBROUTINE set_l_error_message_generator(this, generator)
    CLASS(fexception), INTENT(INOUT) :: this
    PROCEDURE(error_message_generator_fn), OPTIONAL :: generator
    IF (PRESENT(generator)) THEN
      this%error_message_generator => generator
    ELSE
      this%error_message_generator => NULL()
    END IF
  END SUBROUTINE set_l_error_message_generator

!> Function to be called if you want to override the default error message generation
!! for generic exception objects.
!! Note that this function is intended to change the *format* of the message, not where
!! it is printed
!! @param generator Generator function. If not specified then return to default behaviour
  SUBROUTINE set_error_message_generator(generator)
    PROCEDURE(error_message_generator_fn), OPTIONAL :: generator
    IF (PRESENT(generator)) THEN
      default_error_message_generator => generator
    ELSE
      default_error_message_generator => NULL()
    END IF
  END SUBROUTINE set_error_message_generator

!> Function to be called if you want to override the default error message printing
!! for a specific exception object.
!! This function is intended to change the way in which an error report is printed 
!! once it is generated. It is intended to be used if you're using an error logging library
!! or needing to send errors over MPI communication.
!! NOTE that your generator function should block until all output is complete because
!! after it is called immediate termination of the code could happen
!! @param printer Error printer function. If not specified then return to default behaviour
  SUBROUTINE set_l_error_printer(this, printer)
    CLASS(fexception), INTENT(INOUT) :: this
    PROCEDURE(error_printer_fn), OPTIONAL :: printer
    IF (PRESENT(printer)) THEN
      this%error_printer => printer
    ELSE
      this%error_printer => NULL()
    END IF
  END SUBROUTINE set_l_error_printer

!> Function to be called if you want to override the default error message printing
!! for a specific exception object.
!! This function is intended to change the way in which an error report is printed 
!! once it is generated. It is intended to be used if you're using an error logging library
!! or needing to send errors over MPI communication.
!! NOTE that your generator function should block until all output is complete because
!! after it is called immediate termination of the code could happen
!! @param printer Error printer function. If not specified then return to default behaviour
  SUBROUTINE set_error_printer(printer)
    PROCEDURE(error_printer_fn), OPTIONAL :: printer
    IF (PRESENT(printer)) THEN
      default_error_printer => printer
    ELSE
      default_error_printer => NULL()
    END IF
  END SUBROUTINE set_error_printer

!> Function that is called to test whether an exception still has unhandled errors in it and
!! exit the code if it does. Automatically called by the finalizer for the fexception object
  SUBROUTINE fail(this)
    CLASS(fexception), INTENT(IN) :: this
    INTEGER :: iexcept, nexcept, iunit, lunit, errcode
    LOGICAL :: first
    INTEGER, DIMENSION(:), ALLOCATABLE :: units
    CHARACTER(LEN=2000) :: dump
    CHARACTER(LEN=:), ALLOCATABLE :: output, temp
    LOGICAL :: should_exit
    TYPE(fexception_info) :: ei

    IF (.NOT. ALLOCATED(this%exceptions)) RETURN
    IF (.NOT. ANY(this%exceptions)) RETURN
    IF (.NOT. ALLOCATED(this%log_units)) THEN
      IF (.NOT. ALLOCATED(default_log_units)) THEN
        units = [error_unit]
      ELSE
        units = default_log_units
      END IF
    ELSE
      units = this%log_units
    END IF

    first = .TRUE.
    should_exit = .FALSE.
    output =  REPEAT("*",40) // NEW_LINE("A")
    nexcept = 1
    DO iexcept = 1, SIZE(this%exceptions)
      IF (this%exceptions(iexcept)) THEN
        IF (.NOT. should_exit .AND. info(iexcept)%is_fatal) THEN
          should_exit = .TRUE.
          errcode = info(iexcept)%fatal_code
        END IF
        IF (ASSOCIATED(this%error_message_generator)) THEN
          ei%exception_index = nexcept
          ei%name = info(iexcept)%name
          ei%message = this%exception_messages(iexcept)%content
          IF (ALLOCATED(this%exception_files(iexcept)%content)) THEN
            ei%file = this%exception_files(iexcept)%content
            ei%line = this%exception_lines(iexcept)
          END IF
          CALL this%error_message_generator(ei, temp)
        END IF
        IF (.NOT. ALLOCATED(temp)) THEN
          IF (.NOT. first)  output = output // NEW_LINE('A')
          WRITE(dump, "('Exception ',I0, ' triggered - ',A)") nexcept, &
              info(iexcept)%name
          output = output // TRIM(dump) // NEW_LINE('A')
          IF (ALLOCATED(this%exception_files(iexcept)%content)) THEN
            IF (this%exception_lines(iexcept) >= 0) THEN
              WRITE(dump, "('Found in file ',A,' at line ', I0)") &
                  this%exception_files(iexcept)%content, &
                  this%exception_lines(iexcept)
              output = output // TRIM(dump) // NEW_LINE('A')
            ELSE
              WRITE(dump, "('Found in file ',A)") &
                  this%exception_files(iexcept)%content
              output = output // TRIM(dump) // NEW_LINE('A')
            END IF
          END IF
          IF (ALLOCATED(this%exception_messages(iexcept)%content)) THEN
            WRITE(dump, "('Message - ',A)") &
            this%exception_messages(iexcept)%content
            output = output // TRIM(dump) // NEW_LINE('A')
          END IF
           
          IF (.NOT. info(iexcept)%is_fatal) THEN
            WRITE(dump, "(A)") "Error is not fatal."
            output = output // TRIM(dump) // NEW_LINE('A')
          END IF
          first = .FALSE.
          WRITE(dump, '(A)') REPEAT('-', 40)
          output = output // TRIM(dump) // NEW_LINE('A')
        ELSE
          output = output // temp // NEW_LINE('A')
          DEALLOCATE(temp)
        END IF
        nexcept = nexcept + 1
      END IF
    END DO

    IF (should_exit) THEN
      output = output // "Stopping on first fatal error. All errors have been reported" // NEW_LINE('A')
      output = output // "but the error code reported is from the first error in the list" // NEW_LINE('A')
#ifdef F2008
      WRITE(dump,'("Error code is " , I0)') errcode
      output = output // TRIM(dump) // NEW_LINE('A')
#endif
      output = output // REPEAT("*",40)
    END IF

    IF (.NOT. ASSOCIATED(this%error_printer)) THEN
      DO iunit = 1, SIZE(units)
        lunit = units(iunit)
        WRITE(lunit, '(A)') output
        FLUSH(lunit)
      END DO
    ELSE
      CALL this%error_printer(output)
    END IF

    IF (ASSOCIATED(this%abort)) THEN
      CALL this%abort(this, errcode)
    ELSE
#ifndef F2008
      ERROR STOP errcode
#else
      ERROR STOP -1
#endif
    END IF

  END SUBROUTINE fail

!> Destructor
  SUBROUTINE destructor(this)
    TYPE(fexception), INTENT(IN) :: this

    CALL this%fail()

  END SUBROUTINE destructor

END MODULE fexception_mod

