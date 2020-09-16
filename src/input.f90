module input_mod
    implicit none
    !>input
    real(8)::rou,sub_rou
    real(8)::time
    !>parameters:
    real(8),parameter::beta=0.007

    real(8),parameter::lamda=0.08

    real(8),parameter::A=0.0001

    real(8),parameter::deltat=0.01

    !>program input
    character*15:: string
    contains
    subroutine read_rou
        call get_command_argument(1,string)
        !>Rod ejection accident happened
        if (index(string,'rod') .gt. 0) then
            !difen a rou as a tag
            rou=99.99
        !<
        !> other accident
        else
            read(string,*) rou
        end if
    end subroutine
    subroutine read_time
        call get_command_argument(2,string)
        read(string,*) time
    end subroutine
    subroutine read_rodrou
        call get_command_argument(3,string)
        read(string,*) sub_rou
    end subroutine
end module

