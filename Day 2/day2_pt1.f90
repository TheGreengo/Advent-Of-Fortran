program main
    implicit none

    integer :: ids(100)
    integer :: I 
    integer :: ios = 0
    character(len=200) :: buff

    ids = (/(I, I =1, 100)/)

    open(unit=10, file="day2.txt", status="old")

    i = 0
    do while (ios .eq. 0)
        i = i + 1
        read(10, '(A)', IOSTAT=ios) buff
        print *, len(trim(buff))
    end do

    close(10)

    ! So I think that this here is the plan. We loop through the file,
    ! for each line, we check if the numbers are valid
    ! if they aren't, we set that index of ids to 0
    ! we return the sum of ids
    print *, sum(ids)
end program main