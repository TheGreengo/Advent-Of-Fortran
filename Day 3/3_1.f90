program main
    implicit none

    integer, parameter :: SIZE = 140
    integer :: IOS = 0
    integer :: i = 0
    integer :: l = 0
    integer :: k = 0
    integer :: curr = 5
    character(len=200) :: buff
    character :: mat(SIZE, SIZE)
    integer :: places(5000, 3) ! row, column, size
    integer :: vals(5000) 

    vals(:) = 0
    places(:, 1) = -1 
    places(:, 2) = -1 
    places(:, 3) = -1 

    open(unit=10, file="day3.txt", status="old")

    do i = 1, SIZE
        read(10,'(A)', IOSTAT=IOS) buff
        call LoadMatrixRow(i, trim(buff))
    end do

    ! here we go through the matrix and load in all the places and sizes into
    ! the places matrix
    call LoadNums
    ! 
    call LoadVals

    print *, sum(vals)

    contains

    subroutine LoadMatrixRow(num, str)
        integer, intent(in) :: num
        character, intent(in) :: str(SIZE)
        integer :: j
        
        j = 0

        do j = 1,SIZE
            mat(num, j) = str(j)
        end do
    end subroutine LoadMatrixRow 

    subroutine LoadNums
        do i = 1, SIZE
            do k = 1, SIZE
                print *, mat(i,k)
            end do
        end do
    end subroutine LoadNums

    subroutine LoadVals
        do while (curr .ne. 0)
            do k = 1, SIZE
                print *, mat(i,k)
            end do
        end do
    end subroutine LoadVals

    logical function GetValid(x, y, val) result(thang)
        integer, intent(in) :: x
        integer, intent(in) :: y
        integer, intent(in) :: val
        
        thang = .false.

        print *, x, y, val
    end function

end program main