program main
    implicit none

    integer :: ids(100)
    integer :: I 
    integer :: ios = 0
    character(len=200) :: buff

    ids = (/(I, I =1, 100)/)

    open(unit=10, file="day2.txt", status="old")

    do i = 1,100
        read(10, '(A)', IOSTAT=ios) buff
        if (GetValid(len(trim(buff)), trim(buff))) then
            ids(i) = 0
        end if
    end do

    close(10)

    print *, sum(ids)

    contains 

    ! So here's the skinny,
    ! we check to see if it's valid
    ! if it's valid, we check for a number,
    ! if there's a number, we check if the next thing is a number
    ! if not, then we keep going until we hit a number
    ! if the next is, we create a number
    ! we then get the first letter of the next num
    ! and then we check to see if it's valid
    ! if not we end the loop and ser thing=.false.
    logical function GetValid(num, str) result(thing)
        integer, intent(in) :: num
        character, intent(in) :: str(num)

        integer :: MaxRed = 12
        integer :: MaxGreen = 13
        integer :: MaxBlue = 14
        logical :: Reached = .false.
        integer :: j = 0

        thing = .true.

        do while (j .lt. num)
            if (str(i) .eq. ':') then
                Reached = .true.
            end if

            print *, MaxRed
            print *, MaxGreen
            print *, MaxBlue
            print *, j
            j = j + 1
        end do

        print *, str
        thing = .false.
    end function GetValid
end program main