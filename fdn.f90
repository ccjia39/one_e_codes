module fdn_mod
use general
implicit none

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine fdn(ax,bx,lmax,fkl1l2)
      
  integer, intent(in)  :: lmax
  double precision, dimension(0:lmax,0:lmax,0:lmax), intent(inout) :: fkl1l2
  double precision, intent(in)  :: ax, bx

  integer :: k, l1, l2, q, i, j

  fkl1l2(:,:,:) = 0d0
  do l1 = 0, lmax
    do l2 = 0, lmax

      do k = 0, lmax
         do q = max(-k,k-2*l2), min(k,2*l1-k), 2
           i = (k + q)/2
           j = (k - q)/2
           fkl1l2(k,l2,l1) = fkl1l2(k,l2,l1) + cij(l1,i)*cij(l2,j)*ax**(l1-i)*bx**(l2-j)
         enddo
!        WRITE(*,*)"a",k,l1,l2,fkl1l2(k,l2,l1)
      enddo
  
    enddo
  enddo

  end subroutine fdn

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module fdn_mod
