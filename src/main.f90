program main
use fun_mod
use input_mod
    implicit none
    integer(8)::total,i
    real(8),allocatable:: n(:),c(:)
    !>allocate n and c
    allocate(n(100001))
    allocate(c(100001))
    !> init n and c
    n=0
    c=0
    !> define n(1) and c(1),it means n(0) and c(0)
    n(1)=1.00_8
    c(1)=874.9984_8
    !> read time and rou
    call read_time
    call read_rou
    !>calculate time step
    total=int(time/deltat)
    !>open outputfile
    open(99,file='output')
    !>print title
    !> do calculations
    if (rou .lt. 99.99 ) then
        !print title
        if (rou .eq. 0.0) then
            write(*,*) '-------------------Steady  state-------------------'
        else if (rou .lt. 0.0) then
            write(*,*) '-------------------Stop  Reactor-------------------'
        else if (rou .gt. 0.0) then
            write(*,*) '-------------------Rod Extracted-------------------'
        end if
        write(*,*) ' t        n(i)        c(i)      n(i)/n(0)   c(i)/c(0)'
        write(99,*)' t        n(i)        c(i)      n(i)/n(0)   c(i)/c(0)'
        do i = 2,total+1
            !>do calculations
            n(i)=rk_fn(n(i-1),c(i-1))
            c(i)=rk_fc(n(i-1),c(i-1))
            !print results for every step
                write(*, '(F6.2,2X,Es10.4,2X,Es10.4,2X,Es10.4,2X,Es10.4)')(i-1)*deltat,n(i),c(i),n(i)/n(1),c(i)/c(1)
                write(99,'(F6.2,2X,Es10.4,2X,Es10.4,2X,Es10.4,2X,Es10.4)')(i-1)*deltat,n(i),c(i),n(i)/n(1),c(i)/c(1)
        end do
    end if
    if (rou .ge. 99) then
        write(*,*) '-----------Rod Ejection Accident Happend-----------'
        write(*,*) ' t       n(i)        c(i)      n(i)/n(0)   c(i)/c(0)'
        write(99,*)' t       n(i)        c(i)      n(i)/n(0)   c(i)/c(0)'
        do i = 2,total+1
            !>do calculations
            call read_rodrou
            rou=sub_rou*i*deltat
            n(i)=rk_fn(n(i-1),c(i-1))
            c(i)=rk_fc(n(i-1),c(i-1))
            write(*, '(F6.2,2X,Es10.4,2X,Es10.4,2X,Es10.4,2X,Es10.4)')(i-1)*deltat,n(i),c(i),n(i)/n(1),c(i)/c(1)
            write(99,'(F6.2,2X,Es10.4,2X,Es10.4,2X,Es10.4,2X,Es10.4)')(i-1)*deltat,n(i),c(i),n(i)/n(1),c(i)/c(1)
        end do
    end if
    !>deallocate and close
    deallocate(n)
    deallocate(c)
    close(99)
end program