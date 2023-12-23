program main
    implicit none

    integer :: ids(100)
    integer :: I 
    integer :: ios = 0
    character(len=200) :: buff
    integer :: MaxRed
    integer :: MaxGreen
    integer :: MaxBlue

    ids = (/(I, I =1, 100)/)

    open(unit=10, file="day2.txt", status="old")

    do i = 1,100
        read(10, '(A)', IOSTAT=ios) buff
        MaxRed = 0
        MaxGreen = 0
        MaxBlue = 0

        call GetVal(len(trim(buff)), trim(buff))
        ids(i) = MaxRed * MaxGreen * MaxBlue
    end do

    close(10)

    print *, sum(ids)

    contains 

    subroutine GetVal(num, str)

        integer, intent(in) :: num
        character, intent(in) :: str(num)

        integer :: nam = 0
        logical :: Reached
        integer :: j

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
                        j = j + 1
                    else
                        nam = (ICHAR(str(j)) - ICHAR('0'))
                    end if
                    if (str(j+2) .eq. 'r') then
                        if (nam .gt. MaxRed) then
                            MaxRed = nam
                        end if
                    else if (str(j+2) .eq. 'b') then
                        if (nam .gt. MaxBlue) then
                            MaxBlue = nam
                        end if
                    else if (str(j+2) .eq. 'g') then 
                        if (nam .gt. MaxGreen) then
                            MaxGreen = nam
                        end if
                    end if
                end if
            end if

            j = j + 1
        end do

    end subroutine GetVal
end program main