! A program to play Tic-Tac-Toe.
      program tictactoe
      implicit none
      
      integer :: i, j
      character(1) :: tictac(3,3), winner
      logical :: over
      logical :: chkplay
      integer :: move, turn

      write(*,*) "Play Tic-Tac-Toe. Enter 1-9 to play:"
      write(*,*) " "
      write(*,*) "        1 | 2 | 3 "
      write(*,*) "       ---+---+---"
      write(*,*) "        4 | 5 | 6 "
      write(*,*) "       ---+---+---"
      write(*,*) "        7 | 8 | 9 "
      write(*,*) " "
            
      call boardsetup(tictac)
      
   10 turn = 0
      write(*,*) "Your move? "
      read(*,*) move
      if (move .gt. 0 .and. move .le. 9) go to 11
      write(*,*) "Invalid input."
      go to 10
   11 if (chkplay(tictac,move)) go to 12
      write(*,*) "Invalid move, box already occupied."
      go to 10
   12 if (move .eq. 1) tictac(1,1) = "x"
      if (move .eq. 2) tictac(1,2) = "x"
      if (move .eq. 3) tictac(1,3) = "x"
      if (move .eq. 4) tictac(2,1) = "x"
      if (move .eq. 5) tictac(2,2) = "x"
      if (move .eq. 6) tictac(2,3) = "x"
      if (move .eq. 7) tictac(3,1) = "x"
      if (move .eq. 8) tictac(3,2) = "x"
      if (move .eq. 9) tictac(3,3) = "x"
      
   14 if (turn .eq. 0) write(*,*) "After your move..."
      if (turn .eq. 1) write(*,*) "After my move..."
      do 20 i=1,3
      write(*,400) (tictac(i,j), j=1,3)
  400 format(2x,a1,1x,"|",1x,a1,1x,"|",1x,a1,1x)
      go to (15,15,20) i
   15 write(*,*) "---+---+---"
   20 continue
      if (turn .eq. 1) goto 16
      
      call chkovr(tictac,over,winner)
      if (over) goto 30
      
      turn = 1
      call compmove(tictac)
      goto 14
   16 call chkovr(tictac,over,winner)
      if (over) goto 30           
      goto 10
      
   30 write(*,*) "The game is over!"
      if (winner .eq. "d") then
      write(*,*) "The game is a draw."
      else
      write(*,*) "The winner is: ", winner
      end if      
      stop 
      end
      
! Subroutine to check to see if the game is over.    
! =========================================
      subroutine chkovr(tictac,over,winner)
      character(1) :: tictac(3,3), winner
      logical :: over
      
      character(1) :: blank, draw
      parameter (blank = ' ', draw = 'd')

      logical :: same
      logical :: dsame
      integer :: ir, ic

! Assume game is over at start.
      over = .true.
!
! Check for a winner.
! Check rows for a winner.
      do 100 ir = 1, 3
      if (same(tictac(ir,1),tictac(ir,2),tictac(ir,3))) then
      winner = tictac(ir,1)
      return
      end if
  100 continue
! No winner by rows, check columns for a winner.
      do 110 ic = 1, 3
      if (same(tictac(1,ic),tictac(2,ic),tictac(3,ic))) then
      winner = tictac(1,ic)
      return
      end if
  110 continue 
! No winner by rows or columns, check diagonals.
      dsame = same(tictac(1,1),tictac(2,2),tictac(3,3)) 
     +   .or. same(tictac(1,3),tictac(2,2),tictac(3,1)) 
      if (dsame) then
      winner = tictac(2,2)
      return
      end if
! No winner at all. See if game is a draw.
! Check each row for an empty space.
      do 140 ir = 1,3
      do 145 ic = 1,3
      if (tictac(ir,ic) .eq. blank) then
      over = .false.
      return
      end if
  145 continue
  140 continue
! 
! No blank found, game is a draw.
      winner = draw

      return    
      end
      
! Subroutine to play for the computer.
! =========================================
      subroutine compmove(tictac)
      implicit none
      integer :: i, j
      character(1) :: tictac(3,3)
      integer :: paths(3,8), pathsum(8)
      data paths/1,2,3,4,5,6,7,8,9,
     +           1,4,7,2,5,8,3,6,9,
     +           1,5,9,3,5,7/
      integer :: board(9,2), k, x, y, randpos
      data board/1,1,1,2,2,2,3,3,3,1,2,3,1,2,3,1,2,3/

      
!     Your code goes here.

!     Calculate the pathsums.
      do 150 i = 1,8
      pathsum(i) = 0
      do 149 j = 1,3
      x = board(paths(j,i),1)
      y = board(paths(j,i),2)
      if (tictac(x,y) .eq. " ") k = 0
      if (tictac(x,y) .eq. "x") k = 1
      if (tictac(x,y) .eq. "o") k = 4 
      pathsum(i) = pathsum(i) + k     
  149 continue
  150 continue 

!     Offensive code to deal with scenarios where the
!     computer has two in a path.
      do 155 i = 1,8
      if (pathsum(i) .eq. 8) then
      do 154 j = 1,3
      x = board(paths(j,i),1)
      y = board(paths(j,i),2)
      if (tictac(x,y) .eq. " ") then
      tictac(x,y) = "o"
      return
      end if
  154 continue
      end if
  155 continue
  
!     Defensive code to deal with scenarios where the
!     opponent has two in a path.
      do 160 i = 1,8
      if (pathsum(i) .eq. 2) then
      do 159 j = 1,3
      x = board(paths(j,i),1)
      y = board(paths(j,i),2)
      if (tictac(x,y) .eq. " ") then
      tictac(x,y) = "o"
      return
      end if
  159 continue
      end if
  160 continue
  
  170 randpos = int(rand(0)*9)+1
      x = board(randpos,1)
      y = board(randpos,2)
      if (tictac(x,y) .eq. " ") then
          tictac(x,y) = "o"
          return
      end if
      go to 170
  
      return    
      end

! Function to check to see if three elements in a row, column, or diagonal
! are the same.
! =========================================
      logical function same(t1,t2,t3)
      character :: t1,t2,t3
      
      if (t1 .eq. "x" .and. t2 .eq. "x" .and. t3 .eq. "x") goto 200      
      if (t1 .eq. "o" .and. t2 .eq. "o" .and. t3 .eq. "o") goto 200      
      same = .false.
      goto 210
  200 same = .true.
  210 end
  
! Subroutine to set up the Tic-Tac-Toe board.
! =========================================  
      subroutine boardsetup(tictac)
      implicit none
      integer :: i, j
      character(1) :: tictac(3,3)

      do 310 i = 1,3
      do 300 j = 1,3
      tictac(i,j) = " "
  300 continue
  310 continue
      return
      end

! Subroutine to check human play.  
! ========================================= 
      logical function chkplay(tictac,move)
      character(1) :: tictac(3,3)
      integer :: move
                
      go to (401,402,403,404,405,406,407,408,409) move
  401 if (tictac(1,1) .eq. " ") goto 411
      go to 410
  402 if (tictac(1,2) .eq. " ") goto 411
      go to 410
  403 if (tictac(1,3) .eq. " ") goto 411
      go to 410
  404 if (tictac(2,1) .eq. " ") goto 411
      go to 410
  405 if (tictac(2,2) .eq. " ") goto 411
      go to 410
  406 if (tictac(2,3) .eq. " ") goto 411
      go to 410
  407 if (tictac(3,1) .eq. " ") goto 411
      go to 410
  408 if (tictac(3,2) .eq. " ") goto 411
      go to 410
  409 if (tictac(3,3) .eq. " ") goto 411
  410 chkplay = .false.
      goto 412
  411 chkplay = .true.
  412 end