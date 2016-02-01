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

  do
    do 
      turn = 0
      write(*,*) "Your move? "
      read(*,*) move
      if (move < 1 .and. move > 9) then
        write(*,*) "Invalid input."
        cycle
      else if (chkplay(tictac,move)) then
        exit
      else 
        write(*,*) "Invalid move, box already occupied."
        cycle
      end if
    end do

    if (move == 1) tictac(1,1) = "x"
    if (move == 2) tictac(1,2) = "x"
    if (move == 3) tictac(1,3) = "x"
    if (move == 4) tictac(2,1) = "x"
    if (move == 5) tictac(2,2) = "x"
    if (move == 6) tictac(2,3) = "x"
    if (move == 7) tictac(3,1) = "x"
    if (move == 8) tictac(3,2) = "x"
    if (move == 9) tictac(3,3) = "x"

    do
      if (turn == 0) write(*,*) "After your move..."
      if (turn == 1) write(*,*) "After my move..."
      do 20 i=1,3
        write(*,400) (tictac(i,j), j=1,3)
        400 format(2x,a1,1x,"|",1x,a1,1x,"|",1x,a1,1x)
        go to (15,15,20) i
        15 write(*,*) "---+---+---"
      20 continue
      call chkovr(tictac,over,winner)
      if (over) exit
      if (turn == 1) exit
      turn = 1
      call compmove(tictac)
    end do
    call chkovr(tictac,over,winner)
    if (over) exit
  end do

  write(*,*) "The game is over!"
  if (winner == "d") then
    write(*,*) "The game is a draw."
  else
    write(*,*) "The winner is: ", winner
  end if
end

! Subroutine to check to see if the game is over.    
! =========================================
subroutine chkovr(tictac,over,winner)
  character(1) :: tictac(3,3), winner
  logical :: over

  character(1), parameter :: blank = ' ', draw = 'd'

  logical :: same
  logical :: dsame
  integer :: ir, ic

  ! Assume game is over at start.
  over = .true.
  !
  ! Check for a winner.
  ! Check rows for a winner.
  do ir = 1, 3
    if (same(tictac(ir,1),tictac(ir,2),tictac(ir,3))) then
      winner = tictac(ir,1)
      return
    end if
  end do
  ! No winner by rows, check columns for a winner.
  do ic = 1, 3
    if (same(tictac(1,ic),tictac(2,ic),tictac(3,ic))) then
      winner = tictac(1,ic)
      return
    end if
  end do
  ! No winner by rows or columns, check diagonals.
  dsame = same(tictac(1,1),tictac(2,2),tictac(3,3)) .or. same(tictac(1,3),tictac(2,2),tictac(3,1)) 
  if (dsame) then
    winner = tictac(2,2)
    return
  end if
  ! No winner at all. See if game is a draw.
  ! Check each row for an empty space.
  do ir = 1,3
    do ic = 1,3
      if (tictac(ir,ic) == blank) then
        over = .false.
        return
      end if
    end do
  end do
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
  integer :: pathsum(8), k, x, y, randpos
  integer, dimension(3,8) :: paths = reshape((/1,2,3,4,5,6,7,8,9,1,4,7,2,5,8,3,6,9,1,5,9,3,5,7/), shape(paths))
  integer, dimension(9,2) :: board = reshape((/ 1,1,1,2,2,2,3,3,3,1,2,3,1,2,3,1,2,3 /), shape(board))

  !     Your code goes here.

  !     Calculate the pathsums.
  do i = 1,8
    pathsum(i) = 0
    do j = 1,3
      x = board(paths(j,i),1)
      y = board(paths(j,i),2)
      if (tictac(x,y) == " ") k = 0
      if (tictac(x,y) == "x") k = 1
      if (tictac(x,y) == "o") k = 4 
      pathsum(i) = pathsum(i) + k     
    end do
  end do

  !     Offensive code to deal with scenarios where the
  !     computer has two in a path.
  do i = 1,8
    if (pathsum(i) == 8) then
      do j = 1,3
        x = board(paths(j,i),1)
        y = board(paths(j,i),2)
        if (tictac(x,y) == " ") then
          tictac(x,y) = "o"
          return
        end if
      end do
    end if
  end do

  !     Defensive code to deal with scenarios where the
  !     opponent has two in a path.
  do i = 1,8
    if (pathsum(i) == 2) then
      do j = 1,3
        x = board(paths(j,i),1)
        y = board(paths(j,i),2)
        if (tictac(x,y) == " ") then
          tictac(x,y) = "o"
          return
        end if
      end do
    end if
  end do

  do
    randpos = int(rand(0)*9)+1
    x = board(randpos,1)
    y = board(randpos,2)
    if (tictac(x,y) == " ") then
      tictac(x,y) = "o"
      return
    end if
  end do

  return    
end

! Function to check to see if three elements in a row, column, or diagonal
! are the same.
! =========================================
logical function same(t1,t2,t3)
  character :: t1,t2,t3

  if (t1 == "x" .and. t2 == "x" .and. t3 == "x") then
    same = .true.
    return
  else if (t1 == "o" .and. t2 == "o" .and. t3 == "o") then
    same = .true. 
  else
    same = .false.
  end if
end

! Subroutine to set up the Tic-Tac-Toe board.
! =========================================  
subroutine boardsetup(tictac)
  implicit none
  integer :: i, j
  character(1) :: tictac(3,3)

  do i = 1,3
    do j = 1,3
      tictac(i,j) = " "
    end do
  end do
  return
end

! Subroutine to check human play.  
! ========================================= 
logical function chkplay(tictac,move)
  character(1) :: tictac(3,3)
  integer :: move

  chkplay = .false.
  select case (move)
    case (1)
      if (tictac(1,1) == " ") chkplay = .true.
    case (2)
      if (tictac(1,2) == " ") chkplay = .true.
    case (3)
      if (tictac(1,3) == " ") chkplay = .true.
    case (4)
      if (tictac(2,1) == " ") chkplay = .true.
    case (5)
      if (tictac(2,2) == " ") chkplay = .true.
    case (6)
      if (tictac(2,3) == " ") chkplay = .true.
    case (7)
      if (tictac(3,1) == " ") chkplay = .true.
    case (8)
      if (tictac(3,2) == " ") chkplay = .true.
    case (9)
      if (tictac(3,3) == " ") chkplay = .true.
  end select
end