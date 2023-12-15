program hello 
    implicit none

    integer :: count = 0
    integer :: holder = 0
    integer :: ios = 0
    character(len=200) :: buff

    open(unit=10, file="day1.txt", status="old")

    do while (ios .eq. 0)
        read(10, *, IOSTAT = ios) buff
        write(*,*) buff
    end do

    close(10)

    print *, "Final tally", count, holder
end program hello