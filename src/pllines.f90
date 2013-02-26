module styles
  use plplot

  implicit none

  type :: lstyle
     integer :: n
     integer, dimension(:), allocatable :: d,u
  end type lstyle

  type(lstyle), dimension(0:5), private :: sdefs
  logical, private :: is_init=.false.

contains
  subroutine init_styles
    if (is_init) return

    sdefs%n = [0,1,1,2,3,1]

    ! 0 continuous
    allocate(sdefs(0)%d(0), sdefs(0)%u(0))

    ! 1 dotted
    allocate(sdefs(1)%d(1), sdefs(1)%u(1))
    sdefs(1)%d = 20
    sdefs(1)%u = 1980

    ! 2 Dash
    allocate(sdefs(2)%d(1), sdefs(2)%u(1))
    sdefs(2)%d = 1000
    sdefs(2)%u = 1000

    ! 3 dash dot
    allocate(sdefs(3)%d(2), sdefs(3)%u(2))
    sdefs(3)%d = [1000,20]
    sdefs(3)%u = [1000,1000]

    ! 4 dash dot dot
    allocate(sdefs(4)%d(3), sdefs(4)%u(3))
    sdefs(4)%d = [1000,20,20]
    sdefs(4)%u = [1000,1000,1000]

    ! 5 long dash
    allocate(sdefs(5)%d(1), sdefs(5)%u(1))
    sdefs(5)%d = 3000
    sdefs(5)%u = 1000

    is_init = .true.

  end subroutine init_styles

  subroutine set_style(ist)
    integer, intent(in) :: ist

    if (.not. is_init) call init_styles
    if (ist < lbound(sdefs,1) .or. ist > ubound(sdefs,1)) then 
       call pllsty(1)
    else
       call plstyl(sdefs(ist)%n, sdefs(ist)%d(1), sdefs(ist)%u(1))
    end if
  end subroutine set_style
end module styles

program plls
  use plplot
  use styles
  implicit none

  integer, parameter, dimension(16) :: rval = (/255, 0, 255, &
       & 0, 0, 0, 255, 255, 255, 127, 0, 0, 127, 255, 85, 170/),&
       & gval = (/ 255, 0, 0, 255, 0, 255, 0, 255, 127, 255, 255, 127,&
       & 0, 0, 85, 170/), &
       & bval = (/ 255, 0, 0, 0, 255, 255, 255, 0, 0, 0, 127, 255, 255,&
       & 127, 85, 170/)

  integer :: i

  call plparseopts(PL_PARSE_FULL)
!!$  call plsdev('epsqt')
  call plsetopt("geometry", "800x800")

  call plscmap0(rval, gval, bval)

  call plinit

  call plenv(-.5_8, 5.5_8, -0.5_8, 5.5_8, &
       & 0, -2)

  do i = 0, 5
     call set_style(i)
     call plline([0._8, 2.5_8], real([i,i], 8))
  end do

  do i = 1, 8
     call pllsty(i)
     call plline([2.5_8, 5._8], real([i,i],8)/2._8)
  end do
  call plend
end program plls
