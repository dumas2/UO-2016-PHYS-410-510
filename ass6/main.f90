program main
!!
! Driver for homework assignment 6. 
!
!
!!
!!!________________________________________________________________
use modules

implicit none
real    :: long1(5000), long2(5000), lat1(5000), lat2(5000)
integer :: count,i,imax,j,num
real, allocatable :: data(:,:),array(:,:)

allocate(data(5000,2),array(5000,2))
!! read in the data.
data  = 0.0
array = 0.0

i=1
do while(.true.)
  read(*,*,end=17) data(i,1), data(i,2)
  i=i+1
end do

17 continue
! Total number of events
imax = i-1

print *, 'Total number of events ',imax,' .'


! Having a lot of trouble with item 2.  part b) and c).
! the event location, california has too many wiggles.

! Item 3 ... 
! a)  check how many  cities in given box.

count = 0
!do j=1,imax
!call testCount(data(j,1),data(j,2),38.9,41.8,-86.4,-81.9,count)
!end do

print *, 'total_count= ',count

print *, 'End item 3. a)'

! Item 3. b)
print *, 'My eye-brain percieves three large clusters in the data between '
print *, 'latitude 39 and 40, and three smaller clusters between latitudes '
print *, '41 and 42, west of longitude 83.5.'

! Item 3. c)
num = 0
 call boxSlider(5.0,38.9,41.8,-86.4,-81.9,num,data)
print *, 'i got out'

deallocate(data,array)
end program
