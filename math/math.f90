module math_lib
    implicit none
    integer,parameter::rkind=selected_real_kind(p=30)
    
    contains
    function mat_det(a,n)
    implicit none
    real(rkind)::mat_det
    integer,intent(in)::n
    real(rkind),intent(in)::a(n,n)
    real(rkind)::b(n,n)
    integer::i,j
    
    b=a
    
    
    !转换为上三角矩阵
    do j=1,n   !第j列化为0
        call swap(b,n,j)
        if(abs(b(j,j))<1d-30) then
            b(j,j)=0
            exit
        end if
        
        
        do i=j+1,n   !第i行化为0
            b(i,:)=b(i,:)-b(i,j)/b(j,j)*b(j,:)
        end do
    end do
    
    
    
    mat_det=1
    do i=1,n
        mat_det=mat_det*b(i,i)
    end do
    end function mat_det
    
    
    subroutine swap(a,n,j)
    integer,intent(in)::n,j
    real(rkind),intent(inout)::a(n,n)
    integer::i
    real(rkind)::c(n)
    if(a(j,j)==0) then
        i=j
        do
            if(a(i,j)==0) then
                i=i+1
            else
                c=a(j,:)
                a(j,:)=a(i,:)
                a(i,:)=-c
                exit
            end if
            if(i==n.and.a(i,j)==0) exit
        end do
    end if
    end subroutine swap
        
    end module math_lib