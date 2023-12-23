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
        if (.not. GetValid(len(trim(buff)), trim(buff))) then
            ids(i) = 0
        end if
    end do

    close(10)

    print *, sum(ids)

    contains 

    ! if there's a number, we check if the next thing is a number
    ! if not, then we keep going until we hit a number
    ! if the next is, we create a number
    ! we then get the first letter of the next num
    ! and then we check to see if it's valid
    ! if not we end the loop and ser thing=.false.
    logical function GetValid(num, str) result(thing)
        integer, intent(in) :: num
        character, intent(in) :: str(num)

        integer :: nam = 0
        integer :: MaxRed = 12
        integer :: MaxGreen = 13
        integer :: MaxBlue = 14
        logical :: Reached
        integer :: j

        thing = .true.
        Reached = .false.
        j = 0

        do while (j .lt. num)
            if (str(j) .eq. ':') then
                Reached = .true.
            end if

            if (Reached) then
                if ((str(j) .le. '9') .and. (str(j) .ge. '0')) then
                    if ((str(j+1) .le. '9') .and. (str(j+1) .ge. '0')) then
                        nam = ((ICHAR(str(j)) - ICHAR('0')) * 10) + (ICHAR(str(j+1)) - ICHAR('0'))
                        if (str(j+3) .eq. 'r') then
                            if (nam .gt. MaxRed) then
                                thing = .false.
                            end if
                        else if (str(j+3) .eq. 'b') then
                            if (nam .gt. MaxBlue) then
                                thing = .false.
                            end if
                        else if (str(j+3) .eq. 'g') then 
                            if (nam .gt. MaxGreen) then
                                thing = .false.
                            end if
                        end if
                    end if
                end if
            end if

            j = j + 1
        end do

    end function GetValid
end program main