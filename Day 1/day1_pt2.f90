program main
    implicit none

    integer :: ios = 0
    character(len=200) :: buff
    integer :: i = 0
    integer :: t(10)

    open(unit=10, file="day1.txt", status="old")

    ! do while (ios .eq. 0)    
    do i = 1,10
        ! i = i + 1
        read(10, *, IOSTAT = ios) buff
        t(i) = with_words(len(trim(buff)), trim(buff))
    end do

    close(10)

    print *, t

    contains 

    integer function with_words(num, str) result(oot)
        implicit none

        integer, intent(in) :: num
        character, intent(in) :: str(num)
        character(len=num) :: pass
        integer :: i
        integer :: num_one
        integer :: num_two
        character :: curr

        num_one = -1
        num_two = -1
        do i = 1, num
            pass(i:i) = str(i)
        end do

        do i = 1, num 
            curr = str(i)
            if ((curr .le. '9') .and. (curr .ge. '0')) then
                if (num_one .eq. -1) then
                    num_one = ICHAR(curr) - ICHAR('0')
                else
                    num_two = ICHAR(curr) - ICHAR('0')
                end if
            else if (get_word(num, pass, i) .gt. -1) then
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
    end function with_words

    integer function get_word(num, str, ind) result(word_num)
        implicit none

        integer, intent(in) :: num
        character(len=num), intent(in) :: str
        integer, intent(in) :: ind
        integer :: dif
        dif = num - ind + 1

        ! one two three four five six seven eight nine zero
        !  3   3    5    4    4    3    5     5    4    4 
        if (dif .gt. 1) then
            if (str(ind:ind+2) .eq. "one") then
                    word_num = 1
                    print *, str(ind:ind+2)
            else if (str(ind:ind+2) .eq. "two") then
                    word_num = 2
                    print *, str(ind:ind+2)
            else if (str(ind:ind+2) .eq. "six") then
                    word_num = 6
                    print *, str(ind:ind+2)
            else if (dif .gt. 3) then
                if (str(ind:ind+3) .eq. "zero")then
                    word_num = 0
                    print *, str(ind:ind+3)
                else if (str(ind:ind+3) .eq. "four") then
                    word_num = 4
                    print *, str(ind:ind+3)
                else if (str(ind:ind+3) .eq. "five")then
                    word_num = 5
                    print *, str(ind:ind+3)
                else if (str(ind:ind+3) .eq. "nine")then
                    word_num = 9
                    print *, str(ind:ind+3)
                else if (dif .gt. 4) then
                    if (str(ind:ind+4) .eq. "three") then
                        word_num = 3
                        print *, str(ind:ind+4)
                    else if (str(ind:ind+4) .eq. "seven") then
                        word_num = 7
                        print *, str(ind:ind+4)
                    else if (str(ind:ind+4) .eq. "eight") then
                        word_num = 8
                        print *, str(ind:ind+4)
                    end if
                end if
            else 
                word_num = -1
            end if
        end if  
    end function get_word
end program main