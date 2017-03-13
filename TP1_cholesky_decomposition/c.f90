program c
        integer  :: n        
        real(8), dimension(:),allocatable::b,x,y
        real(8), dimension(:,:),allocatable::A,R
   
        n=5;
        
        allocate(A(n,n),stat=ierr0) 
        allocate(b(n),stat=ierr0) 
        allocate(x(n),stat=ierr0) 
        allocate(y(n),stat=ierr0) 
        
        b(:)=0.d0;
        
        do i=1,n
                do j=1,n
                        A(i,j) = 1.d0 / dble(i+j-1);
                        b(i)= b(i)+A(i,j);
                enddo  
        enddo   
         
       call alg1(n,A);
       
       call alg2(n,A,b,y);      
 
       call alg3(n,A,y,x);
       
       print*,"X=",x
       
contains

                subroutine alg1(n,A)
                        implicit none;
                        
                        real(8),dimension(:,:), intent(inout)::A
                        
                        integer, intent(in)::n
                        integer ::k,j
                 
                        do k=1,n
                                A(k,k)=sqrt(A(k,k));
                                A(k+1:n,k) = A(k+1:n,k)/A(k,k);
                                
                                do j = k+1,n !!perguntar professor
                                        A(j:n,j) = A(j:n,j) - A(j,k)*A(j:n,k)
                                enddo
                        enddo                       
                        
                end subroutine alg1
                
                
                
                subroutine alg2(n,A,b,x)
                        implicit none;
                        
                        real(8),dimension(:,:), intent(inout)::A
                        real(8),dimension(:), intent(inout)::b,x
                        
                        integer, intent(in)::n
                        integer ::k
                        
                        do k=1,n
                                x(k)=(b(k) - dot_product(A(k,1:k-1),x(1:k-1)))/A(k,k);
                        enddo                       
                        
                end subroutine alg2
                
                
                
                
                subroutine alg3(n,A,b,x)
                        implicit none;
                        
                        real(8),dimension(:,:), intent(inout)::A
                        real(8),dimension(:), intent(inout)::b,x
                        
                        integer, intent(in)::n
                        integer ::k
                        x(n)=(b(n))/A(n,n);
                        do k=n-1,1,-1
                             x(k)=(b(k) - dot_product(A(k+1:n,k),x(k+1:n)))/A(k,k);
                        enddo                       
                        
                end subroutine alg3
                
       		
end program c	
