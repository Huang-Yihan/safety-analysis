module fun_mod
    use input_mod
    implicit none
    contains
    !>define fn=dn/dt
    function fn(n,c)
        real(8)::fn
        real(8)::n,c
        fn=(rou-beta)/A*n+lamda*c
    end function
    !>define fc=dc/dt
    function fc(n,c)
        real(8)::fc
        real(8)::n,c
        fc=beta/A*n-lamda*c
    end function
    !>modified elur method of fn
    function eu_fn(n,c)
        real(8)::eu_fn
        real(8)::n,c
        real(8)::n0,c0
        n0=n+deltat*fn(n,c)
        c0=c+deltat*fc(n,c)
        eu_fn=n+0.5*deltat*(fn(n,c)+fn(n0,c0))
    end function
    function rk_fn(n,c)
        real(8)::rk_fn
        real(8)::n,c
        real(8)::kn1,kn2,kn3,kn4,kc1,kc2,kc3,kc4
        kn1=deltat*fn(n,c)
        kc1=deltat*fc(n,c)
        kn2=deltat*fn(n+0.5*kn1,c+0.5*kc1)
        kc2=deltat*fc(n+0.5*kn1,c+0.5*kc1)
        kn3=deltat*fn(n+0.5*kn2,c+0.5*kc2)
        kc3=deltat*fc(n+0.5*kn2,c+0.5*kc2)
        kn4=deltat*fn(n+kn3,c+kc3)
        kc4=deltat*fc(n+kn3,c+kc3)
        rk_fn=n+(kn1+2*kn2+2*kn3+kn4)/6
    end function   
    function rk_fc(n,c)
        real(8)::rk_fc
        real(8)::n,c
        real(8)::kn1,kn2,kn3,kn4,kc1,kc2,kc3,kc4
        kn1=deltat*fn(n,c)
        kc1=deltat*fc(n,c)
        kn2=deltat*fn(n+0.5*kn1,c+0.5*kc1)
        kc2=deltat*fc(n+0.5*kn1,c+0.5*kc1)
        kn3=deltat*fn(n+0.5*kn2,c+0.5*kc2)
        kc3=deltat*fc(n+0.5*kn2,c+0.5*kc2)
        kn4=deltat*fn(n+kn3,c+kc3)
        kc4=deltat*fc(n+kn3,c+kc3)
        rk_fc=c+(kc1+2*kc2+2*kc3+kc4)/6
    end function  
    !>modified elur method of fc
    function eu_fc(n,c)
        real(8)::eu_fc
        real(8)::n,c
        real(8)::n0,c0
        n0=n+deltat*fn(n,c)
        c0=c+deltat*fc(n,c)
        eu_fc=c+0.5*deltat*(fc(n,c)+fc(n0,c0))
    end function
end module