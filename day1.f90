program hello 
    implicit none

    integer :: count = 0
    integer :: holder = 0
    character :: let = ' '

    open (unit=10, file='day1.txt', status='old', action='read')

    do while (let .ne. ' ')
        print *, let
    end do

    print *, "Final tally", count, holder
end program hello