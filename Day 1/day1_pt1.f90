program main 
    implicit none

    integer :: ios = 0
    character(len=200) :: buff
    integer :: i = 0
    integer :: t(1000)

    open(unit=10, file="day1.txt", status="old")

    do while (ios .eq. 0)
        i = i + 1
        read(10, *, IOSTAT = ios) buff
        t(i) = two_nums(len(trim(buff)),trim(buff))
    end do

    close(10)

    print *, sum(t)

  contains

    integer function two_nums(num, str) result(oot)
        implicit none

        integer, intent(in) :: num
        character, intent(in) :: str(num)
        integer :: i
        integer :: num_one
        integer :: num_two
        character :: curr

        num_one = -1
        num_two = -1

        do i = 1, num 
            curr = str(i)
            if ((curr .le. '9') .and. (curr .ge. '0')) then
                if (num_one .eq. -1) then
                    num_one = ICHAR(curr) - ICHAR('0')
                else
                    num_two = ICHAR(curr) - ICHAR('0')
                end if
            end if
        end do

        if (num_two == -1) then
            oot = (10 * num_one) + num_one
        else
            oot = (10 * num_one) + num_two
        end if
    end function two_nums

end program main