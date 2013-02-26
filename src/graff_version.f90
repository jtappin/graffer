module graff_version
  use iso_fortran_env
  implicit none

    ! The program version
  type :: gr_version
     integer(kind=int16) :: major, minor
     character(len=1) :: revision
   contains
     procedure :: set => gr_set_version
     procedure :: string => gr_version_string
     procedure :: float => gr_version_float
     procedure :: ints => gr_version_int
  end type gr_version

  ! The graffer version
  type(gr_version) :: graffer_version
  
contains
  subroutine gr_set_version(this, major, minor, revision)
    class(gr_version), intent(inout) :: this
    integer, intent(in) :: major, minor
    character(len=1), intent(in), optional :: revision

    this%major = int(major, int16)
    this%minor = int(minor, int16)
    if (present(revision)) then
       this%revision = revision
    else
       this%revision = ''
    end if
  end subroutine gr_set_version

  subroutine gr_version_string(this, sver)
    class(gr_version), intent(in) :: this
    character(len=*), intent(out) :: sver

    write(sver, "(I0,'.',I2.2,a)") this%major, this%minor, this%revision
  end subroutine gr_version_string

  function gr_version_float(this) result(fver)
    real(kind=real32) :: fver
    class(gr_version), intent(in) :: this

    fver = real(this%major, real32) + real(this%minor, real32)/100._real32
  end function gr_version_float

  function gr_version_int(this) result(iver)
    integer(kind=int16), dimension(2) :: iver
    class(gr_version), intent(in) :: this

    iver = [this%major, this%minor]

  end function gr_version_int
end module graff_version
