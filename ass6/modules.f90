module modules

implicit none




CONTAINS

subroutine boxSlider(s,latStart,latFin,longStart,longFin,count,A)
! Slides a box across the screen and calculates the event density.
! Box is of size s*s
implicit none
real,intent(in)    :: latStart,longStart,latFin,longFin
real    :: blat1,blat2,blong1,blong2,s
real    :: boxCent(2),pi,dum1,dum2
real,intent(in):: A(5000,2)
integer :: count,k,i
! variables
pi = acos(-1.0)
! I want to be able to reuse the testCount subroutine so I should 
! figure out bounds for latitude and longitude. 
boxCent(1) = (latStart+s/111.2)/2
boxCent(2) = (longStart + s/(111.3*cos(pi/180.*latStart)))/2.0
! do loop from left to right, moving box by amount s/n_boxes
 ! do loop from bottom to top, moving box by amount s/n_boxes
blat2  = -90.0
blong2 = -180.0
blat1  = latStart
blong1 = longStart

!do k=1,200
k=1

do while((blat2.lt.latFin))
blat1  = blat2


do while((blong2.lt.longFin).and.(k.lt.5000))
 
 call calcBounds(s,dum1,dum2,blat1,blat2,blong1,blong2)
print *, blat1,blat2,blong1,blong2
print *, 'dum1,dum2',dum1,dum2
do i =1,500
 call testCount(A(i,1),A(i,2),blat1,blat2,blong1,blong2,count)
end do
blong1  = blong2

 print *, 'Box size ', s,' has center', boxCent(1), boxCent(2)
 print *, 'Box size ', s,' has ',count,' counts.'
 print *, 'k=', k
k = k+1

end do

end do
! write position and average density to file


end subroutine
!_________________________________________________________________________
subroutine testCount(lat1,long1,blat1,blat2,blong1,blong2,count)
implicit none

real    :: lat1,lat2,long1,long2
real    :: blat1,blat2,blong1,blong2
integer, intent(inout) :: count

print *, 'testing ',lat1,' and ',long1
! test if point is in region defined by blats and blongs.
if((lat1.gt.blat1).and.(lat1.lt.blat2))then
 print *, lat1 
 print *, 'in between ',blat1,' and ',blat2,' in latitude.'
  if((long1.gt.blong1).and.(long1.lt.blong2))then
    count=count+1
    write(99,*) long1, lat1
  end if
end if
print *, "count= ",count
end subroutine
!
!_________________________________________________________________________
!
subroutine calcBounds(s,latStart,longStart,blat1,blat2,blong1,blong2)
! calculates the boundaries in latitude and longitude of some region
! given a point and the regions geometry.
implicit none
real :: latStart,longStart
real :: s,dlat,dlong,pi
real,intent(inout):: blat1,blat2,blong1,blong2
!! Assume given starting point is at lower-left end of region.
! 1 degree of latitude is 111.2 km
! 1 degree of longitude is pi/180*R*cos(latitude) or 111.3 * cos(latitude) km
pi = acos(-1.0)

!calculate right boundary given left starting point

dlong = s/(111.3 * cos(pi/180.0*latStart))
print *, 'dlong = ',dlong
print *, 'blong1',blong1
blong2 = blong1 + dlong
! calculate top boundary given lower starting point.
dlat = s/111.2
print *, 'dlat = ',dlat
blat2 = blat1+dlat
! pass back the latitude and longitude boundaries

end subroutine 
!
!_________________________________________________________________________
!
subroutine calcDist(la1,la2,lo1,lo2,dist)
implicit none
real ::la1,la2,lo1,lo2,dist
real :: a,c,d,pi
real,parameter :: radius = 6.371e3
real :: dlat,dlong,d2r
integer  :: i,k,j,imax,n,nmax

!calculate distances between cities
pi = acos(-1.0)
d2r = pi/180.0
     dlat = la1-la2
    dlong = lo1-lo2
    a = (sin(d2r*dlat/2.0))**2+cos(d2r*la2)* &
 &      cos(d2r*la2)*(sin(d2r*dlong/2.0))**2
!    print *, a
    c = 2*atan2(sqrt(a),sqrt(1.0-a))
    dist = c

end subroutine



end module
