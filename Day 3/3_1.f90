program main
    implicit none

    integer, parameter :: SIZE = 140
    integer :: IOS = 0
    integer :: i = 0
    character(len=200) :: buff
    character :: mat(SIZE, SIZE)
    integer :: places(5000, 3)

    places(:, 1) = -1 
    places(:, 2) = -1 
    places(:, 3) = -1 
    open(unit=10, file="day3.txt", status="old")

    do i = 1,140
        read(10,'(A)', IOSTAT=IOS) buff
        call LoadMatrix(i, trim(buff))
    end do


    contains

    subroutine LoadMatrix(num, str)
        integer, intent(in) :: num
        character, intent(in) :: str(SIZE)
        integer :: j
        
        j = 0

        do j = 1,SIZE
            mat(num, j) = str(j)
        end do
    end subroutine LoadMatrix 

    subroutine LoadNums
    end subroutine LoadNums

    logical function GetValid(x, y, size) result(thang)
        integer, intent(in) :: x
        integer, intent(in) :: y
        integer, intent(in) :: size
        
        thang = .false.

        print *, x, y, size
    end function

end program main