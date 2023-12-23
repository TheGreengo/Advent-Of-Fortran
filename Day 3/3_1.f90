program main
    implicit none

    integer :: IOS = 0
    integer :: i = 0
    character(len=200) :: buff

    open(unit=10, file="day3.txt", status="old")

    do i = 1,140
        read(10,'(A)', IOSTAT=IOS) buff
        print *, buff
    end do


end program main