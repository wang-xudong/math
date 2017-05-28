program main
    use math_lib
    implicit none
    integer,parameter::n=4
    real(rkind)::d(n,n),b
    !d=reshape([3,1,1,1,1,3,1,1,1,1,3,1,1,1,1,3],[n,n])
    !d=reshape([4,1,2,4,1,2,0,2,10,5,2,0,0,1,1,7],[n,n])
    !d=reshape([1,1,-1,2,1,1,3,-4,1,0,1,-1,1,-5,3,-3],[n,n])
    d=reshape([1,1,1,1,2,3,4,1,3,4,1,2,4,1,2,3],[n,n])
    write(*,*)d
    b=mat_det(d,n)
    write(*,*) b
    read(*,*)
    end program main