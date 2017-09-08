c   *****************************************************************
c   Program Name:       The Artillery Control System
c   Program Number:     8
c   Group Members:      Ryan Romanosky
c                       Jaqua Starr
c                       Alex Bioni
c	  Group Number:       3
c
c	*****************************************************************
c     Begin Program / Declare Variables

      program g3p8
        implicit none
        integer :: n = 1
        integer, dimension(10) :: art
        real, dimension(10) :: rTTarget
        real, dimension(10) :: angleToTarget
        real, dimension(10) :: initVTTarget
        real, dimension(10) :: timeTTarget
        real, dimension(10) :: timeTFire
        real, dimension(7)  :: A2Veloc
        real, dimension(7)  :: A2Ranges
        real, dimension(7)  :: M1Veloc
        real, dimension(7)  :: M1Ranges

        integer :: oerror
        character(len=1) :: overwrite
        character(len=20) :: outputFileName
        logical :: exists


        integer, parameter :: a2 = 1
        integer, parameter :: m1 = 2
c     Angle Parameters
        real, parameter, dimension(2) :: A2Angles =(/-5,66/)
        real, parameter, dimension(2) :: M1Angles =(/-2,65/)
c     Range Parameters (in meters)
        real, parameter :: A2MRange = 11200
        real, parameter :: M1MRange = 23700
c     Shell Parameters (in Kilograms)
        real, parameter :: A2Shell = 15
        real, parameter :: M1Shell = 45.4
c     Initial Velocities (V0, in meters per second)
        real, parameter :: V0a2 = 472
        real, parameter :: V0m1 = 853

        real :: maxTime
        integer :: i = 1
        logical :: quit = .false.
        do i = 7, 1, -1
          A2Veloc(i) = V0a2 * (real(i)/7)
          M1Veloc(i) = V0m1 * (real(i)/7)
        end do



c     Main Loop
        do while (.not. quit)
          call FileSaver(oerror, overwrite, outputFileName, exists)
          call BatteryNumber(n)
          call ArtilleryChooser(art, n, rTTarget)

          call AngleFinder(A2Veloc,M1Veloc,rTTarget,art,A2MRange,M1MRang
     +e,n, angleToTarget, initVTTarget)
          call TimeFinder(rTTarget, art, A2MRange, M1MRange, angleToTarg
     +et, initVTTarget, timeTTarget, n, timeTFire, maxTime)
          call FireControl(rTTarget, angleToTarget, initVTTarget, timeTT
     +arget, n, art, timeTFire, maxTime)
          call EndProgram(quit)

          close(unit=9)
        end do
      end program g3p8


c   Subroutine to open write file for printout
      subroutine FileSaver(oerror, overwrite, outputFileName,exists)
        integer :: oerror
        character(len=1) :: overwrite
        character(len=20) :: outputFileName
        logical :: exists
        write(*,*) 'This program will save its data to an output file, a
     +s well as print it to the display'
        write(*,*) 'Please enter an output filename: '
        read(*,*) outputFileName
        inquire(file=outputFileName, exist = exists)
        do while (exists .and. overwrite .ne. 'y')
          write(*,*) 'The file ', outputFileName, 'exists'
          write(*,*) 'Do you wish to overwrite ', outputFileName, '?'
          write(*,*) 'Enter "y" to overwrite the file or "n" to enter a
     +new file name'
          read(*,*) overwrite
          if (overwrite .ne. 'y') then
            write(*,*) 'Enter a new filename: '
            read(*,*) outputFileName
            inquire(file=outputFileName,exist=exists)
          end if
        end do

        if(.not. exists .or. overwrite .eq. 'y') then
          open(unit=9, file=outputFileName, status='replace',
     +action='write',iostat=oerror)
        write(*,*) 'File: ', outputFileName
        write(9,*) 'File: ', outputFileName
        end if
      end subroutine FileSaver

c   Subroutine for user to enter number of batteries
      subroutine BatteryNumber(n)
        implicit none
        integer :: n

        write(*,*) 'Welcome to the Artillery Control System'
        write(*,*) 'In this program you will control a number of artille
     +ry batteries and fire them upon a central target.'
        write(*,*) 'How many batteries do you wish to use?'
        write(9,*) 'Welcome to the Artillery Control System'
        write(9,*) 'In this program you will control a number of artille
     +ry batteries and fire them upon a central target.'
        write(9,*) 'How many batteries do you wish to use?'
        read(*,*) n
        do while (n .gt. 10 .or. n .lt. 1)
          write(*,10) '*'
          write(*,*) 'Error: Please enter a number between 1 and 10'
          write(*,10) '*'
          write(9,10) '*'
          write(9,*) 'Error: Please enter a number between 1 and 10'
          write(9,10) '*'
          read(*,*) n
        end do
  10  format(' ', 36(A))
      end subroutine BatteryNumber

c   subroutine for user to choose artillery pieces and range
      subroutine ArtilleryChooser(art, n, userR)
        implicit none
        integer, dimension(10) :: art
        real, dimension(10)    :: userR
        integer, dimension(10) :: temp
        real, dimension(10)    :: tempR
c     Range Parameters (in meters)
        real, parameter :: A2MRange = 11200
        real, parameter :: M1MRange = 23700
        integer :: n, i
        write(*,*) ' '
        write(9,*) ' '
        write(*,1) 'You have chosen to use', n, 'artillery batteries.'
        write(*,*)
        write(*,*) 'Please enter the types of battery you want to use. '
        write(*,*) 'Then, enter the the range the battery is from the ta
     +rget.'
        write(*,*) '***************************************************'
        write(*,*) 'Enter "1" for the 105mm A2A1 Howitzer'
        write(*,2) ' * Max-Range (in meters) for the A2A1 is: ', A2MRang
     +e
        write(*,*) 'Enter "2" for the 155mm M1'
        write(*,2) ' * Max-Range (in meters) for the M1 is: ', M1MRange
        write(*,*) '***************************************************'
c *******************************To File*********************************
        write(9,*) '***************************************************'
        write(9,1) 'You have chosen to use', n, 'artillery batteries.'
        write(9,*)
        write(9,*) 'Please enter the types of battery you want to use. '
        write(9,*) 'Then, enter the the range the battery is from the ta
     +rget.'
        write(9,*) 'Enter "1" for the 105mm A2A1 Howitzer'
        write(9,2) ' * Max-Range (in meters) for the A2A1 is: ', A2MRang
     +e
        write(9,*) 'Enter "2" for the 155mm M1'
        write(9,2) ' * Max-Range (in meters) for the M1 is: ', M1MRange
        write(9,*) '***************************************************'
c ***********************************************************************
        do i = 1, n
          write(*,3) 'Enter artillery type for battery number: ', i
          write(9,3) 'Enter artillery type for battery number: ', i
          read(*,*) temp(i)
          do while (temp(i) .ne. 1 .and. temp(i) .ne. 2)
            write(*,*) 'Error: Please enter either "1" or "2"'
            write(9,*) 'Error: Please enter either "1" or "2"'
            read(*,*) temp(i)
          end do
          art(i) = temp(i)
          write(9,*) 'Artillery type: ',art(i)
          write(*,3) 'Enter range (in meters) from target for battery nu
     +mber: ', i
          write(9,3) 'Enter range (in meters) from target for battery nu
     +mber: ', i

          read(*,*) tempR(i)
          if (art(i) .eq. 1) then
            do while (tempR(i) .gt. 11200 .or. tempR(i) .lt. 1)
              write(*,*) 'Error: Max-range for 105mm is 11,200 meters.'
              write(*,*) 'Please enter a range between 0 and 11,200 mete
     +rs'
              write(9,*) 'Error: Max-range for 105mm is 11,200 meters.'
              write(9,*) 'Please enter a range between 0 and 11,200 mete
     +rs'
              read(*,*) tempR(i)
            end do
          else if (art(i) .eq. 2) then
            do while (tempR(i) .gt. 23700 .or. tempR(i) .lt. 0)
              write(*,*) 'Error: Max-Range for 155mm is 23,700 meters.'
              write(*,*) 'Please enter a range between 0 and 23,700 mete
     +rs'
              write(9,*) 'Error: Max-Range for 155mm is 23,700 meters.'
              write(9,*) 'Please enter a range between 0 and 23,700 mete
     +rs'
              read(*,*) tempR(i)
            end do
          end if
          userR(i) = tempR(i)
          write(9,*) 'Range: ', userR(i)
        end do
  1   format(' ', A,X,I2,X,A)
  2   format(' ', A, F8.2,X, 'meters')
  3   format(' ', A, I2)
      end subroutine ArtilleryChooser


      subroutine AngleFinder(A2Veloc, M1Veloc, rTTarget, art,A2MRange,
     +M1MRange, n, angleToTarget, initVTTarget)
        implicit none
        integer :: n, i
c     Fixed velocites of artilleries based on no. of charges
        real, dimension(7) :: A2Veloc, M1Veloc
c     Range-to-target array (parallel with art array)
        real, dimension(10) :: rTTarget
        real, dimension(10) :: angleToTarget
        real, dimension(10) :: initVTTarget
        real :: A2MRange, M1MRange
        integer, dimension(10) :: art
        real :: tempAngle = 0, tempAngle2 = 0
        integer, parameter :: a2 = 1
        integer, parameter :: m1 = 2
        integer :: j

        do i = 1, n
          j = 1
          print*, art(i)
          if (art(i) .eq. a2 ) then
            tempAngle = .5*asin((rTTarget(i)*9.88)/(A2Veloc(j)**2))
            initVTTarget(i) = A2Veloc(j)
c   Loop to cycle through no. of charges to use
c     * j was previously set to 6 (for chages 6/7)
c     * loops in descending order until tempAngle is NOT NaN
c     * or until j equals 0 (no solution, should've been caught earlier)
            do while (tempAngle .ne. tempAngle .or. j .gt. 7)
              tempAngle = .5*asin((rTTarget(i)*9.88)/(A2Veloc(j)**2))
              initVTTarget(i) = A2Veloc(j)
              j = j+1
            end do
          else if (art(i) .eq. m1) then
            tempAngle = .5*asin((rTTarget(i)*9.88)/(M1Veloc(j)**2))
            initVTTarget(i) = M1Veloc(j)
            do while (tempAngle .ne. tempAngle .or. j .lt. 1)
              tempAngle = .5*asin((rTTarget(i)*9.88)/(M1Veloc(j)**2))
              initVTTarget(i) = M1Veloc(j)
              j = j+1
            end do
          end if

          angleToTarget(i) = tempAngle
          if (j .lt. 1) then
            print*, 'Error: No conceivable path to victory'
          end if
        end do
      end subroutine AngleFinder

c   Subroutine to calculate Time to target and max time
      subroutine TimeFinder(rTTarget, art, A2MRange, M1MRange, angleToTa
     +rget, initVTTarget, timeTTarget, n, timeTFire, maxTime)
        integer :: n, i
        real, dimension(10) :: rTTarget
        real, dimension(10) :: angleToTarget
        real, dimension(10) :: initVTTarget
        real, dimension(10) :: timeTTarget
        real, dimension(10) :: timeTFire
        real :: A2MRange, M1MRange
        integer, dimension(10) :: art
        real :: tempTime
        real :: maxTime

        do i = 1, n
          timeTTarget(i) = 2*((initVTTarget(i)*sin(angleToTarget(i)))/
     +9.88)
          if (i .gt. 1) then
            if ( timeTTarget(i) .gt. timeTTarget(i-1) ) then
              tempTime = timeTTarget(i)
            else
              tempTime = timeTTarget(i-1)
            end if
          else
            tempTime = timeTTarget(i)
          end if
        end do
c   maxTime is the time that the longest battery takes
        maxTime = tempTime
        do i = 1, n
          timeTFire(i) = tempTime - timeTTarget(i)
        end do

      end subroutine TimeFinder

c   Subroutine for firing/timing/recording artillery
      subroutine FireControl(rTTarget, angleToTarget, initVTTarget, time
     +TTarget, n, art, timeTFire, maxTime)
        implicit none
        real, dimension(10) :: rTTarget
        real, dimension(10) :: angleToTarget
        real, dimension(10) :: initVTTarget
        integer, dimension(10) :: art
        real, dimension(10) :: timeTFire
        real, dimension(10) :: timeTTarget
        integer, dimension(10) :: impacted = 0
        real :: maxTime
        integer :: n, i, j
        real:: t = 0
        real, dimension(10) :: x, y=0, Vx, Vy
        integer, parameter :: a2 = 1
        integer, parameter :: m1 = 2
        print*, maxTime
c   Time Loop
        do while (t .le. maxTime+.24)
          write(*,*) ' '
          write(9,*) ' '
          write(*,3) 'Time: ', t
          write(9,3) 'Time: ', t
c   Iterates through number of artillery
          do i = 1, n
            if (t .ge. timeTFire(i)) then
c   Loop to tell user when battery has fired
              if (t.le.timeTFire(i)+.24.and.t.gt.timeTFire(i)-.25)then
                write(*,*) ' '
                write(*,4) '** Battery No.', i, 'has fired at t=',timeTF
     +ire(i),'s | Theta=',(angleToTarget(i)*(180/3.13159)),'deg | V0= ',
     +initVTTarget(i),' **'
                write(*,*) ' '
                write(9,*) ' '
                write(9,4) '** Battery No.', i, 'has fired at t=',timeTF
     +ire(i),'s | Theta=',(angleToTarget(i)*(180/3.13159)),'deg | V0= ',
     +initVTTarget(i),' **'
                write(9,*) ' '
              end if
c   Compute X-velocity, Y-veloc, X-position, Y-pos
              Vx(i) = initVTTarget(i) * cos(angleToTarget(i))

              Vy(i) = initVTTarget(i) * sin(angleToTarget(i))-(9.88*(t-
     +timeTFire(i)))
              x(i)  = Vx(i) * (t - timeTFire(i))
              y(i)  = (initVTTarget(i)* sin(angleToTarget(i)))*(t -
     +timeTFire(i) ) - ((9.88/2)*((t-timeTFire(i))**2))
c   Write when shell has impacted
              if (rTTarget(i)-x(i) .lt. 0 .and. impacted(i) .eq. 0)then
                write(*,*) '****************************************'
                write(*,5) 'Battery No.', i, 'impacted target at t=',
     +((maxTime-timeTTarget(i))+timeTTarget(i)),'s'
                write(*,*) '****************************************'
                write(*,*) ' '
                write(9,*) '****************************************'
                write(9,5) 'Battery No.', i, 'impacted target at t=',
     +((maxTime-timeTTarget(i))+timeTTarget(i)),'s'
                write(9,*) '****************************************'
                write(9,*) ' '
                impacted(i) = 1
              else if (impacted(i) .eq. 1) then
c   Write out info for each battery per cycle
              else if ( art(i) .eq. a2 .or. art(i) .eq. m1 ) then
                if (art(i) .eq. a2) then
                  write(*,1) '*Battery No.', i, ': A2A1 105mm: '
                  write(9,1) '*Battery No.', i, ': A2A1 105mm: '
                else
                  write(*,1) '*Battery No.', i, ': M1 155m: '
                  write(9,1) '*Battery No.', i, ': M1 155m: '
                end if
                write(*,2) 'Vx:',Vx(i),'Vy:',abs(Vy(i)),'Alt:',y(i),'X-p
     +os:', x(i), 'Range-to-target:', (rTTarget(i) - x(i))
                write(9,2) 'Vx:',Vx(i),'Vy:',abs(Vy(i)),'Alt:',y(i),'X-p
     +os:', x(i), 'Range-to-target:', (rTTarget(i) - x(i))
              end if
            end if
          end do
          t = t+.25
        end do

  1   format(' ', T3,A, I2,A)
  2   format(' ', T5,A,T10,F6.2,T18,A,T22,F6.2,T31,A,T35,F8.2,T44,A,T50,
     +F8.2,T60,A,T77, F8.2,'m')
  3   format(' ', A, F6.2, 's')
  4   format(' ',A,I2,X,A,F6.2,X,A,F6.2,X,A,F6.2,A)
  5   format(' ',A, I2,X,A,F6.2,A)
      end subroutine

c   Subroutines to end the program if user wishes
      subroutine EndProgram(quit)
        implicit none

        character(len=4) :: temp
        logical :: quit

        write(*,*) 'Do you wish to continue or quit?'
        write(*,*) '(Enter "quit" to quit or anything else to continue)'
        write(9,*) 'Do you wish to continue or quit?'
        write(9,*) '(Enter "quit" to quit or anything else to continue)'
        read(*,*) temp
        if (temp .eq. 'quit' .or. temp .eq. 'Quit') then
          quit = .true.
        else
          quit = .false.
        end if

      end subroutine EndProgram
